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

dataCO2.n <- dataC02 %>% mutate(years = str_sub(V1,7,10),
                                CO2 = as.numeric(str_sub(V1,12,17))) %>%
  dplyr::select(years,CO2)


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
start_date = "1940-01-01"
end_date = "1949-12-31"

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

    point.data.fixed <- list()
    for (iens in seq(1,ensemblesN)){
      # Remove duplicate
      point.data.fixed[[iens]] <-
        point.data[[iens]][!duplicated(index(point.data[[iens]])),]
      # reorder
      point.data.fixed[[iens]] <-
        point.data.fixed[[iens]][,c("t2m","sp","d2m","tp",
                                    "v10","u10","ssrd","strd")]
      point.data.fixed[[iens]]$co2 <- 280

    }

    # Calling the met2CF inside extract bc in met process met2CF comes before extract !
    out <- met2CF.ERA5(
      slat,
      slon,
      start_date,
      end_date,
      sitename=csuffix,
      outfolder,
      point.data.fixed,
      overwrite = TRUE,
      verbose = TRUE
    )


  }, error = function(e) {
    PEcAn.logger::logger.severe(paste0(conditionMessage(e)))
  })


  saveRDS(point.data,file.path("./data",paste0("TS_",csite,".RDS")))

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
                                   start_date = start_date,
                                   end_date = end_date,
                                   lat = slat,
                                   lon = slon,
                                   overwrite = TRUE)

}

# scp /Users/felicien/Documents/projects/ED2.regional.runs/scripts/convert.ERA52ED2.R hpc:/data/gent/vo/000/gvo00074/felicien/R
