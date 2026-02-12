read.single.run.S.files <- function(dir,
         BaseName,
         years,monthd = 1,suffix = "-01-000000-g01.h5"){


  df <- data.frame()
  for (cyear in years){

    print(cyear)

    for (cmonth in months){
      cfile <- file.path(dir,
                         paste0(BaseName,"-S-",cyear,"-",
                                sprintf("%02d",cmonth),suffix))

      if (!file.exists(cfile)) next
      nc <- nc_open(cfile)
      cvar <- ncvar_get(nc,"AGB_SI")
      df <- bind_rows(df,
                      data.frame(year = cyear,
                                 month = cmonth,
                                 pft = c(2,3,4),
                                 AGB = (apply(cvar,1,sum))[c(2,3,4)]))

      nc_close(nc)

    }
  }

  return(df)

}
