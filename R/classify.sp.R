classify.sp <- function(species){

  species <- as.vector(tolower(species))
  classification <- ED2.Congo::species.classification
  species.classified <- data.frame(species = species) %>%
    left_join(classification,
              by = "species") %>%
    mutate(PFT = case_when(temperament == "1 short-lived pioneer" ~ 2,
                           temperament == "2 long-lived pioneer" ~ 3,
                           temperament == "3 non-pioneer light demander" ~ 2,
                           temperament == "4a light and shade tolerant" ~ 3,
                           temperament == "4 shade tolerant" ~ 4,
                           temperament == "5 shade bearing" ~ 3,
                           is.na(temperament) & succession_type == "1 pioneer" ~ 2,
                           is.na(temperament) & succession_type == "2 successional" ~ 3,
                           is.na(temperament) & succession_type == "3 old growth - successional" ~ 3,
                           is.na(temperament) & succession_type == "4 old growth" ~ 4,
                           TRUE ~ 3)) %>%  # Default = 3, use WD instead
    dplyr::select(species,PFT)


  return(species.classified)
}
