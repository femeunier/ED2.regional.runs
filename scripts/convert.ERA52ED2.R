rm(list = ls())

library(ncdf4)
library(stringr)
library(dplyr)
library(PEcAn.ED2)
library(udunits2)
library(ED2.regional.runs)
library(xts)

################################################################################
method = "ncss"
maxErrors = 10
sleep = 2
verbose = FALSE
overwrite = TRUE


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
in.path = "/data/gent/vo/000/gvo00074/ED_common_data/met/global"

in.prefix = "ERA5_global_"
in.prefix2 = "ERA5_global_pressure_"
vars = NULL
overwrite = TRUE

lats <- seq(0,1,0.5)
lons <- seq(24,25,0.5)
coords <- expand.grid(lon = lons,
                      lat = lats)

# Years of drivers
start_date = "1962-01-01"
end_date = "1962-12-31"

for (isite in seq(1,nrow(coords))){

  print(isite/nrow(coords))

  # Your site coordinates
  slon <- coords[["lon"]][isite]
  slat <- coords[["lat"]][isite]

  csite <- paste0("ERA5_lat_",
                  slat,
                  "_lon_",
                  slon)

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
        point.data <- ensemblesN %>%
          purrr::map(function(ens) {

            ncfile <- file.path(in.path, paste0(in.prefix, year, ".nc"))
            ncfile2 <- file.path(in.path, paste0(in.prefix2, year, ".nc"))

            PEcAn.logger::logger.info(paste0("Trying to open :", ncfile, " "))

            if (!file.exists(ncfile)){PEcAn.logger::logger.severe("The nc file was not found.")}

            PEcAn.logger::logger.info(paste0("Trying to open :", ncfile2, " "))

            if (!file.exists(ncfile2)){PEcAn.logger::logger.severe("The nc file was not found.")}


            #msg
            PEcAn.logger::logger.info(paste0(year, " is being processed ", "for ensemble #", ens, " "))
            #open the file
            nc_data <- ncdf4::nc_open(ncfile)
            nc_data2 <- ncdf4::nc_open(ncfile2)
            # time stamp

            t <- ncdf4::ncvar_get(nc_data, "time")
            tunits <- ncdf4::ncatt_get(nc_data, 'time')
            tustr <- strsplit(tunits$units, " ")
            timestamp <-
              as.POSIXct(t * 3600, tz = "UTC", origin = tustr[[1]][3])

            t2 <- ncdf4::ncvar_get(nc_data2, "valid_time")
            tunits2 <- ncdf4::ncatt_get(nc_data2, 'valid_time')
            tustr2 <- strsplit(tunits2$units, " ")
            timestamp2 <-
              as.POSIXct(t2, tz = "UTC", origin = tustr2[[1]][3])

            try(ncdf4::nc_close(nc_data))
            try(ncdf4::nc_close(nc_data2))


            # set the vars
            if (is.null(vars))
              vars <- names(nc_data$var)
            # for the variables extract the data


            vars2 <- names(nc_data2$var)

            vars2 <- vars2[!(vars2 %in% c("number","expver"))]


            all.data.point <- vars %>%
              purrr::map_dfc(function(vname) {
                PEcAn.logger::logger.info(paste0(" \t ",vname, " is being extracted ! "))



                brick.tmp <-
                  raster::brick(ncfile, varname = vname, level = ens)
                nn <-
                  raster::extract(brick.tmp,
                                  sp::SpatialPoints(cbind(slon, slat)),
                                  method = 'simple')

                if (!is.numeric(nn)) {
                  PEcAn.logger::logger.severe(paste0(
                    "Expected raster object to be numeric, but it has type `",
                    paste0(typeof(nn), collapse = " "),
                    "`"
                  ))
                }

                # replacing the missing/filled values with NA
                nn[nn == nc_data$var[[vname]]$missval] <- NA
                # send out the extracted var as a new col
                t(nn)

              }) %>%
              `colnames<-`(vars)


            # Second file

                all.data.point2 <- vars2 %>%
                  purrr::map_dfc(function(vname) {
                    PEcAn.logger::logger.info(paste0(" \t ",vname, " is being extracted ! "))



                    brick.tmp <-
                      raster::brick(ncfile2, varname = vname, level = ens)
                    nn <-
                      raster::extract(brick.tmp,
                                      sp::SpatialPoints(cbind(slon, slat)),
                                      method = 'simple')

                    if (!is.numeric(nn)) {
                      PEcAn.logger::logger.severe(paste0(
                        "Expected raster object to be numeric, but it has type `",
                        paste0(typeof(nn), collapse = " "),
                        "`"
                      ))
                    }

                    # replacing the missing/filled values with NA
                    nn[nn == nc_data2$var[[vname]]$missval] <- NA
                    # send out the extracted var as a new col
                    t(nn)

                  }) %>%
                  `colnames<-`(vars2)


            #close the connection


            # send out as xts object
            XTS1 <- xts::xts(all.data.point, order.by = timestamp)
            XTS2 <- xts::xts(all.data.point2, order.by = timestamp2)
            merge(XTS1, XTS2)
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


    # Calling the met2CF inside extract bc in met process met2CF comes before extract !
    out <- met2CF.ERA5(
      slat,
      slon,
      start_date,
      end_date,
      sitename=newsite,
      outfolder,
      point.data,
      overwrite = TRUE,
      verbose = TRUE
    )



  }, error = function(e) {
    PEcAn.logger::logger.severe(paste0(conditionMessage(e)))
  })



  saveRDS(point.data,file.path("./data",paste0("TS_",csite,".RDS")))



  PEcAn.ED2::met2model.ED2(in.path = file.path(outfolder,paste0(in.prefix,newsite,"_1")),
                           in.prefix = "ERA5.1",
                           outfolder = file.path(outfolder, paste0(in.prefix,newsite,"_1"),"ED2"),
                           start_date = start_date,
                           end_date = end_date,
                           lat = slat,
                           lon = slon,
                           overwrite = TRUE)
}



# scp /Users/felicien/Documents/projects/ED2.regional.runs/scripts/convert.ERA52ED2.R hpc:/data/gent/vo/000/gvo00074/felicien/R
