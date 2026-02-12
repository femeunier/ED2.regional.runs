ID2Code <- function(ID){

  ID <- as.vector(ID)
  ID.df <- data.frame(PlotID = ID)

  Code.ID.df <- ID.df %>%
    left_join(PlotID.Code,
              by = "PlotID") %>%
    dplyr::select(-PlotID)

  Code <- Code.ID.df %>% pull(PlotCode)

  return(Code)

}
