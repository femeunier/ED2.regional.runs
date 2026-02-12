rm(list = ls())

library(ncdf4)
library(dplyr)

dir <- "/kyukon/scratch/gent/gvo000/gvo00074/felicien/CB/out/CB_X_9W_Y_9N/histo/"
BaseNames <- c("history-eco-lu")
years <- 1560:2660
months <- 1
suffix <- "-01-000000-g01.h5"

df <- data.frame()

for (BaseName in BaseNames){

  for (cyear in years){

    print(cyear)

    for (cmonth in months){
      cfile <- file.path(dir,
                         paste0(BaseName,"-S-",cyear,"-",
                                sprintf("%02d",cmonth),suffix))

      if (!file.exists(cfile)) next
      nc <- nc_open(cfile)
      AGB_SI <- ncvar_get(nc,"AGB_SI")
      LAI_SI <- ncvar_get(nc,"LAI_PY")
      df <- bind_rows(df,
                      data.frame(BaseName,
                                 year = cyear,
                                 month = cmonth,
                                 pft = c(2,3,4),
                                 LAI = (apply(LAI_SI,1,sum))[c(2,3,4)],
                                 AGB = (apply(AGB_SI,1,sum))[c(2,3,4)]))

      nc_close(nc)

    }
  }
}


saveRDS(df,
        "./outputs/ts.AGB.CB.RDS")
