cleanup.ED2.directory <- function(
    Dir.analy = c("/kyukon/scratch/gent/gvo000/gvo00074/felicien/CB/out/CB_X_9W_Y_9N/analy"),
    Dir.hist = c("/kyukon/scratch/gent/gvo000/gvo00074/felicien/CB/out/CB_X_9W_Y_9N/histo"),

    prefix.analy = "analy",
    prefix.histo = "history-eco",

    years2keep.full.analy = c(2014:2024),
    years2keep.month.histo = c(1400:2024),

    month2keep.analy = 1,
    month2keep.histo = 1){

  library(dplyr)

  analy.files <- list.files(Dir.analy,pattern = paste0(prefix.analy,"-Q-*"),
                            full.names = TRUE)
  analy.years <- as.numeric(
    sapply(strsplit(sub(prefix.analy,"",basename(analy.files)),"-"),"[[",3))
  analy.months <- as.numeric(
    sapply(strsplit(sub(prefix.analy,"",basename(analy.files)),"-"),"[[",4))

  hist.files <- list.files(Dir.hist,pattern = paste0(prefix.histo,"-S-*"),
                           full.names = TRUE)
  hist.years <- as.numeric(
    sapply(strsplit(sub(prefix.histo,"",basename(hist.files)),"-"),"[[",3))
  hist.months <- as.numeric(
    sapply(strsplit(sub(prefix.histo,"",basename(hist.files)),"-"),"[[",4))


  df.analy <-
    data.frame(file = analy.files) %>%
    mutate(year = analy.years,
           month = analy.months)

  df.hist <-
    data.frame(file = hist.files) %>%
    mutate(year = hist.years,
           month = hist.months)

  df.analy2keep <- df.analy %>%
    filter(year %in% years2keep.full.analy,
           month %in% month2keep.analy)
  df.hist2keep <- df.hist %>%
    filter(year %in% years2keep.month.histo,
           month %in% month2keep.histo)

  keep.analy <- file.path(df.analy2keep$file)
  keep.hist  <- file.path(df.hist2keep$file)
  files.keep <- c(keep.analy, keep.hist)

  all.analy <- df.analy$file
  all.hist  <- df.hist$file

  files.all <- c(all.analy, all.hist)
  files.delete <- setdiff(files.all, files.keep)

  # print(files.delete)
  file.remove(files.delete)

}

# scp /Users/felicien/Documents/projects/ED2.regional.runs/scripts/cleanup.ED2.directory.R hpc:/data/gent/vo/000/gvo00074/felicien/R


