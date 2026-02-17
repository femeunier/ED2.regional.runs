rm(list = ls())

library(dplyr)
library(ggplot2)

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/ts.AGB.CB.RDS",
          "./outputs/"))

raw <- readRDS("./outputs/ts.AGB.CB.RDS")

ts.AGB.CB <- bind_rows(raw,
                       raw %>%
                         group_by(BaseName,year,month) %>%
                         summarise(AGB = sum(AGB,na.rm = TRUE),
                                   LAI = sum(LAI,na.rm = TRUE),
                                   pft = 0,
                                   .groups = "keep"))

ggplot(data = ts.AGB.CB %>%
         filter(pft == 1)) +
  geom_line(aes(x = year + (month-1/2)/12,
                y = LAI,
                linetype = BaseName,
                color = as.factor(pft))) +
  # scale_x_continuous(limits = c(1560,1590)) +
  theme_bw()


ggplot(data = ts.AGB.CB %>%
         filter(pft == 16)) +
  geom_line(aes(x = year + (month-1/2)/12,
                y = AGB,
                linetype = BaseName,
                color = as.factor(pft))) +
  # scale_x_continuous(limits = c(1560,1590)) +
  theme_bw()
