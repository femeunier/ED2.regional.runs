rm(list = ls())

library(ncdf4)
library(stringr)
library(dplyr)
library(PEcAn.ED2)
library(udunits2)
library(ED2.regional.runs)
library(xts)
library(lubridate)

################################################################################
# Extract
in.path = "/data/gent/vo/000/gvo00074/ED_common_data/met/CB/ERA5/"

in.prefix = "ERA5_CB_"
vars = NULL
overwrite = TRUE

lats <- seq(2.5,7.5,1)
lons <- seq(20,25,1)
coords <- expand.grid(lon = lons,
                      lat = lats)

# Years of drivers
start_date = "1941-01-01"
end_date = "2024-12-31"

overwrite <- FALSE

################################################################################
# Functions

is_leap <- function(year) {
  (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
}

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

  x$co2 <- co2_ppm
  x
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
         co2 = as.numeric(V2)/1e6) %>%
  dplyr::select(year,co2)

co2_by_year <- setNames(dataCO2.n$co2, dataCO2.n$year)


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

  pecan.op.files <- paste0(file.path(outfolder,
                              paste0(csite,"_1"),
                              "ERA5.1."),unique(seq(year(as.Date(start_date)),
                                                   year(as.Date(end_date)))),
                           ".nc")

  if (all(file.exists(pecan.op.files))){
    next()
  }

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

      raw.data.ensemble.init <- raw.data.ensemble["1941/1950"]

      raw.data.ensemble.spinup <- raw.data.ensemble.init
      raw.data.ensemble.spinup$co2 <- 276.59/1e6

      raw.df <- as.data.frame(raw.data.ensemble.spinup) %>%
        mutate(date = (index(raw.data.ensemble.spinup))) %>%
        mutate(year = year(date),
               month = month(date),
               day = day(date),
               hour = hour(date))

      cdf <- raw.df %>%
        filter(!(month == 2 & day == 29))

      cdf.spinup <- cdf %>%
        mutate(year = year - 251)

      years <- unique(cdf.spinup$year)
      years.leap <- years[is_leap(years)]

      for (year.leap in years.leap){
        cdf.spinup <- bind_rows(cdf.spinup,
                                cdf.spinup %>%
                                  filter(year == year.leap,
                                         month == 2,
                                         day == 28) %>%
                                  mutate(day = 29))

      }

      cdf.spinup <- cdf.spinup %>%
        arrange(year,month,day,hour) %>%
        mutate(date = paste0(year,"/",sprintf("%02d",month),"/",
                           sprintf("%02d",day)," ",
                           sprintf("%02d",hour),":00:00"))

      raw.data.ensemble.spinup_mod <- as.xts(cdf.spinup %>%
                                               dplyr::select(-c("date","year","month","day","hour")))

      index(raw.data.ensemble.spinup_mod) <-
        as.POSIXct(cdf.spinup$date, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
      spinup[[iens]] <- raw.data.ensemble.spinup_mod

      all_deltas <- seq(241,11,-10)
      all.df <- data.frame()
      for (delta in all_deltas){
        all.df <- bind_rows(all.df,
                            cdf %>%
                              mutate(year = year - delta))
      }

      all.df <- bind_rows(all.df,
                          cdf %>%
                            filter(year == 1941) %>%
                            mutate(year = 1940)) %>%
        dplyr::select(-co2) %>%
        left_join(dataCO2.n,
                  by = "year")

      years <- unique(all.df$year)
      years.leap <- years[is_leap(years)]

      for (year.leap in years.leap){
        all.df <- bind_rows(all.df,
                            all.df %>%
                              filter(year == year.leap,
                                     month == 2,
                                     day == 28) %>%
                              mutate(day = 29))

      }

      all.df <- all.df %>%
        arrange(year,month,day,hour) %>%
        mutate(date = paste0(year,"/",sprintf("%02d",month),"/",
                             sprintf("%02d",day)," ",
                             sprintf("%02d",hour),":00:00"))

      idx <- as.POSIXct(all.df$date, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")

      raw.data.ensemble.historical <- xts::xts(
        all.df %>% dplyr::select(-c("date","year","month","day","hour")),
        order.by = idx,
        tzone = "UTC"
      )
      historical1[[iens]] <- raw.data.ensemble.historical

      historical2[[iens]] <- raw.data.ensemble["1941/2024"]

      historical2[[iens]] <- add_yearly_co2_xts(historical2[[iens]],
                                                co2_by_year, tz = "UTC")

      forcing[[iens]] <- merge_periods(spinup[[iens]],
                                       historical1[[iens]],
                                       historical2[[iens]])

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
      verbose = verbose
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

for (isite in seq(1,nrow(coords),1,1)){

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
                                   overwrite = overwrite)

}

# scp /Users/felicien/Documents/projects/ED2.regional.runs/scripts/convert.ERA52ED2.R hpc:/data/gent/vo/000/gvo00074/felicien/R
