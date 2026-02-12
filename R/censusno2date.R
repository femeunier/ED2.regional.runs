censusno2date <- function(no,ID){

  Census.date

  ID <- as.vector(ID)
  no <- as.vector(no)
  df <- data.frame(PlotID = ID,
                   census.no = no)

  date.df <- df %>%
    left_join(Census.date,
              by = c("PlotID","census.no"))

  date <- date.df %>% pull(census.date)

  return(date)

}
