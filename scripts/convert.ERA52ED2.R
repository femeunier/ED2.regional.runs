rm(list = ls())

library(ncdf4)
library(stringr)
library(dplyr)
library(PEcAn.ED2)
library(udunits2)
library(ED2.regional.runs)
library(xts)

Sys.setenv(HDF5_USE_FILE_LOCKING = "FALSE")

################################################################################
method = "ncss"
maxErrors = 10
sleep = 2
verbose = FALSE
overwrite = TRUE

################################################################################
# Functions

merge_periods <- function(spinup, historical1, historical2) {
  x <- rbind(spinup, historical1, historical2)

  # ensure increasing time
  x <- x[order(index(x)), ]

  # if overlaps exist, keep first (spinup/hist1 priority if you rbind in that order)
  x <- x[!duplicated(index(x)), ]

  x
}

add_yearly_co2_xts <- function(x, co2_by_year, tz = "UTC") {
  yrs <- as.integer(format(index(x), "%Y", tz = tz))
  co2_ppm <- unname(co2_by_year[as.character(yrs)])

  if (anyNA(co2_ppm)) {
    missing_years <- sort(unique(yrs[is.na(co2_ppm)]))
    stop("Missing CO2 values for years: ", paste(missing_years, collapse = ", "))
  }

  x$co2 <- co2_ppm / 1e6
  x
}


is_leap <- function(y) (y %% 4 == 0 & y %% 100 != 0) | (y %% 400 == 0)

# shift an xts "year slice" into a target year (keeps month/day/time)
shift_to_year <- function(x_year, target_year, tz = "UTC") {
  idx <- index(x_year)
  mmdd_hms <- format(idx, "%m-%d %H:%M:%S", tz = tz)

  new_str <- sprintf("%04d-%s", as.integer(target_year), mmdd_hms)
  new_idx <- as.POSIXct(new_str, format = "%Y-%m-%d %H:%M:%S", tz = tz)

  # drop invalid (e.g. Feb-29 in non-leap years)
  keep <- !is.na(new_idx)
  x2 <- x_year[keep, ]
  new_idx <- new_idx[keep]

  # enforce sorted & unique index
  o <- order(new_idx)
  x2 <- x2[o, ]
  new_idx <- new_idx[o]

  # if duplicates remain, keep first occurrence
  dup <- duplicated(new_idx)
  if (any(dup)) {
    x2 <- x2[!dup, ]
    new_idx <- new_idx[!dup]
  }

  index(x2) <- new_idx
  x2
}

shift_years_block <- function(x, from_years = 1941:1950, to_years = 1690:1699, tz="UTC") {
  stopifnot(length(from_years) == length(to_years))

  # helper: shift one year slice to a target year (drops Feb-29 if invalid)
  shift_to_year <- function(x_year, target_year) {
    idx <- index(x_year)
    mmdd_hms <- format(idx, "%m-%d %H:%M:%S", tz = tz)
    new_str <- sprintf("%04d-%s", as.integer(target_year), mmdd_hms)
    new_idx <- as.POSIXct(new_str, format = "%Y-%m-%d %H:%M:%S", tz = tz)

    ii <- which(!is.na(new_idx))
    if (length(ii) == 0) return(x_year[0, , drop = FALSE])

    x2 <- x_year[ii, , drop = FALSE]
    new_idx <- new_idx[ii]

    o <- order(new_idx)
    x2 <- x2[o, , drop = FALSE]
    new_idx <- new_idx[o]

    dup <- duplicated(new_idx)
    if (any(dup)) {
      x2 <- x2[!dup, , drop = FALSE]
      new_idx <- new_idx[!dup]
    }

    index(x2) <- new_idx
    x2
  }

  pieces <- vector("list", length(from_years))
  for (k in seq_along(from_years)) {
    fy <- from_years[k]
    ty <- to_years[k]

    x_year <- x[paste0(fy, "/")]
    if (nrow(x_year) == 0) stop("No data found for template year: ", fy)

    pieces[[k]] <- shift_to_year(x_year, ty)
  }

  out <- do.call(rbind, pieces)

  # final safety: ensure sorted/unique index
  out <- out[order(index(out)), ]
  out <- out[!duplicated(index(out)), ]

  out
}


make_historical1 <- function(raw, co2_by_year,
                             start_year = 1700, end_year = 1939,
                             template_years = 1941:1950,
                             tz = "UTC") {

  # template block (10 years)
  template <- raw[paste0(min(template_years), "/", max(template_years))]

  out_list <- vector("list", length = end_year - start_year + 1)
  k <- 1

  for (y in start_year:end_year) {
    # pick which template year to use (cycle over 10 years)
    y_tmpl <- template_years[( (y - start_year) %% length(template_years) ) + 1]

    x_year <- template[paste0(y_tmpl, "/")]
    x_year <- shift_to_year(x_year, y, tz = tz)

    # assign yearly CO2 (ppm -> mol/mol)
    co2_ppm <- unname(co2_by_year[as.character(y)])
    if (is.na(co2_ppm)) stop("Missing CO2 for year: ", y)

    x_year$co2 <- co2_ppm / 1e6

    out_list[[k]] <- x_year
    k <- k + 1
  }

  do.call(rbind, out_list)
}



.pick_first <- function(nm, candidates) {
  hit <- intersect(nm, candidates)
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

get_point_xts_ncdf4 <- function(ncfile, slon, slat, ens = 1,
                                drop_vars = c("expver","number")) {

  nc <- nc_open(ncfile)
  on.exit(try(nc_close(nc), silent = TRUE), add = TRUE)

  # ---- dimension/coordinate variable names
  dnames <- names(nc$dim)
  vnames <- names(nc$var)

  lon_name  <- .pick_first(c(vnames, dnames), c("longitude","lon","LONGITUDE","LON"))
  lat_name  <- .pick_first(c(vnames, dnames), c("latitude","lat","LATITUDE","LAT"))
  time_name <- .pick_first(c(vnames, dnames), c("valid_time","time","TIME","forecast_reference_time"))

  if (is.na(lon_name) || is.na(lat_name) || is.na(time_name)) {
    stop("Could not identify lon/lat/time names in file.")
  }

  lon <- ncvar_get(nc, lon_name)
  lat <- ncvar_get(nc, lat_name)

  ix <- which.min(abs(lon - slon))
  iy <- which.min(abs(lat - slat))

  # ---- time
  tvals <- ncvar_get(nc, time_name)
  tunits <- ncatt_get(nc, time_name, "units")$value
  origin <- sub(".*since\\s+", "", tunits)
  timestamp <- as.POSIXct(tvals, tz = "UTC", origin = origin)

  # ---- variables to read (exclude coordinates + drop_vars)
  vars <- setdiff(names(nc$var), c(drop_vars, lon_name, lat_name, time_name))

  read_var_point <- function(vn) {
    v <- nc$var[[vn]]
    dn <- vapply(v$dim, `[[`, "", "name")

    start <- rep(1L, length(dn))
    count <- rep(1L, length(dn))

    # map common dim names to indices
    if (any(dn %in% c("longitude","lon","LON","LONGITUDE"))) start[dn %in% c("longitude","lon","LON","LONGITUDE")] <- ix
    if (any(dn %in% c("latitude","lat","LAT","LATITUDE")))  start[dn %in% c("latitude","lat","LAT","LATITUDE")]  <- iy

    # time: read full time series
    if (any(dn %in% c("valid_time","time","TIME"))) count[dn %in% c("valid_time","time","TIME")] <- -1L

    # ensemble/member dimension (if present)
    if (any(dn %in% c("number","member","ensemble","realization"))) {
      start[dn %in% c("number","member","ensemble","realization")] <- ens
      count[dn %in% c("number","member","ensemble","realization")] <- 1L
    }

    x <- ncvar_get(nc, vn, start = start, count = count)

    # mv <- v$missval
    # if (!is.null(mv) && is.finite(mv)) x[x == mv] <- NA_real_

    as.numeric(x)
  }

  mat <- sapply(vars, read_var_point)
  colnames(mat) <- vars

  xts::xts(mat, order.by = timestamp)
}

###############################################################################
# CO2
# scp ./data/CO2_1700_2019_TRENDYv2020.txt hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/data/
fileCO2 <- "./data/global_co2_ann_1700_2024.txt"
dataC02 <- read.table(fileCO2,stringsAsFactors = FALSE)

dataCO2.n <- dataC02 %>%
  mutate(year = as.numeric(V1),
         CO2 = as.numeric(V2)) %>%
  dplyr::select(year,CO2)

co2_by_year <- setNames(dataCO2.n$CO2, dataCO2.n$year)


################################################################################
# Extract
in.path = "/data/gent/vo/000/gvo00074/ED_common_data/met/CB/ERA5/"

in.prefix = "ERA5_CB_"
vars = NULL
overwrite = TRUE

lats <- seq(0,0,0.5)
lons <- seq(24,24,0.5)
coords <- expand.grid(lon = lons,
                      lat = lats)

# Years of drivers
start_date = "1941-01-01"
end_date = "2024-12-31"

for (isite in seq(1,nrow(coords))){

  print(isite/nrow(coords))

  # Your site coordinates
  slon <- coords[["lon"]][isite]
  slat <- coords[["lat"]][isite]

  csuffix <- paste0("lat_",
               slat,
               "_lon_",
               slon)
  csite <- paste0("ERA5_",
                  csuffix)


  outfolder = file.path("/data/gent/vo/000/gvo00074/ED_common_data/met/CB/ERA5_ED2/",
                        csite)
  newsite = csite

  years <- seq(lubridate::year(start_date),lubridate::year(end_date),1)
  ensemblesN <- seq(1, 1)

  tryCatch({
    #for each ensemble
    one.year.out <- years %>%
      purrr::map(function(year) {

        # for each year
        ncfile <- file.path(in.path, paste0(in.prefix, year, ".nc"))

        point.data <- ensemblesN %>%
          purrr::map(function(ens) {

            PEcAn.logger::logger.info(paste0("Reading point data from: ", ncfile))
            if (!file.exists(ncfile)) stop("NetCDF file not found: ", ncfile)

            XTS1 <- get_point_xts_ncdf4(ncfile, slon = slon, slat = slat, ens = ens)
            XTS1
          }) %>%
          setNames(paste0("ERA_ensemble_", ensemblesN))

        #Merge mean and the speard
        return(point.data)

      }) %>%
      setNames(years)

    # The order of one.year.out is year and then Ens - Mainly because of the spead  / I wanted to touch each file just once.
    # This now changes the order to ens - year
    point.data <- ensemblesN %>%
      purrr::map(function(Ensn) {
        one.year.out %>%
          purrr::map( ~ .x [[Ensn]]) %>%
          do.call("rbind.xts", .)
      })

    # We arrange everything for spinup/historical period

    forcing <- spinup <- historical1 <- historical2 <-
      list()
    for (iens in seq(1,ensemblesN)){

      raw.data.ensemble <- point.data[[iens]][!duplicated(index(point.data[[iens]])),c("t2m","sp","d2m","tp","v10","u10","ssrd","strd")]

      spinup[[iens]] <- shift_years_block(raw.data.ensemble[index(raw.data.ensemble) <= as.POSIXct("1950-01-01", tz = "UTC"),],
                                          from_years = 1941:1950, to_years = 1690:1699, tz="UTC")

      # constant CO2 for spinup (mol/mol)
      spinup[[iens]]$co2 <- 276.59/1e6

      historical1[[iens]] <- make_historical1(raw.data.ensemble, co2_by_year,
                                              start_year = 1700, end_year = 1940,
                                              template_years = 1941:1950,
                                              tz = "UTC")

      historical2[[iens]] <- raw.data.ensemble["1941/2024"]

      historical2[[iens]] <- add_yearly_co2_xts(historical2[[iens]], co2_by_year, tz = "UTC")

      forcing[[iens]] <- merge_periods(spinup[[iens]], historical1[[iens]], historical2[[iens]])

    }

    # Calling the met2CF inside extract bc in met process met2CF comes before extract !
    out <- ED2.regional.runs::met2CF.ERA5(
      slat,
      slon,
      start_date = "1690-01-01",
      end_date = "2024-12-31",
      sitename=csuffix,
      outfolder,
      forcing,
      overwrite = TRUE,
      verbose = TRUE
    )


  }, error = function(e) {
    PEcAn.logger::logger.severe(paste0(conditionMessage(e)))
  })


  saveRDS(forcing,
          file.path("./data",paste0("TS_",csite,".RDS")))

  raster::removeTmpFiles(h = 0)
  closeAllConnections()
  gc(FALSE); gc(FALSE)
}

for (isite in seq(1,nrow(coords))){

  print(isite/nrow(coords))

  slon <- coords[["lon"]][isite]
  slat <- coords[["lat"]][isite]

  csuffix <- paste0("lat_",
                    slat,
                    "_lon_",
                    slon)
  csite <- paste0("ERA5_",
                  csuffix)


  outfolder = file.path("/data/gent/vo/000/gvo00074/ED_common_data/met/CB/ERA5_ED2/",
                        csite)
  newsite = csite

  ED2.regional.runs::met2model.ED2(in.path = file.path(outfolder,paste0("ERA5_",csuffix,"_1")),
                                   in.prefix = "ERA5.1",
                                   outfolder = file.path(outfolder, paste0("ERA5_",csuffix,"_1"),"ED2"),
                                   start_date = "1690-01-01",
                                   end_date = "2024-12-31",
                                   lat = slat,
                                   lon = slon,
                                   overwrite = TRUE)

}

# scp /Users/felicien/Documents/projects/ED2.regional.runs/scripts/convert.ERA52ED2.R hpc:/data/gent/vo/000/gvo00074/felicien/R
