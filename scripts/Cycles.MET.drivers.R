rm(list = ls())

library(xts)
library(tidyverse)
library(lubridate)
library(ggplot2)

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/data/TS_ERA5_lat_0_lon_24.RDS",
          "./outputs/"))

X <- readRDS("./outputs/TS_ERA5_lat_0_lon_24.RDS")[[1]]

df <- fortify.zoo(X) |>        # keeps index
  dplyr::rename(time = Index) |>     # rename index
  pivot_longer(
    -time,
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    year = year(time),
    hour = hour(time),
    day = day(time),
    doy  = yday(time),
    month.num = month(time),
    month = month(time, label = TRUE)
  )


df %>%
  filter(year == 1692,
         month.num == 2) %>%
  pull(day) %>%
  unique()

diel <- df |>
  group_by(variable, hour) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

diel %>% dplyr::filter(variable == "ssrd")

ggplot(diel, aes(hour, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Hour of day", y = "Mean value",
       title = "Diel cycle") +
  theme_bw()

seasonal <- df |>
  group_by(variable, month) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

ggplot(seasonal, aes(month, value, group = 1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Month", y = "Mean value",
       title = "Seasonal cycle") +
  theme_bw()

