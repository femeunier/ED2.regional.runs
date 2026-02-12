rm(list = ls())


library(xts)
library(tidyverse)
library(lubridate)

system2("rsync",
        c("-avz",
          "hpc:/vscmnt/gent_kyukon_data/_kyukon_data_gent/vo/000/gvo00074/ED_common_data/met/CB/extracted/ERA5_lat_1_lon_24.5_1/ERA5.RDS",
          "./outputs/"))

X <- readRDS("./outputs/ERA5.RDS")


df <- fortify.zoo(X) |>        # keeps index
  rename(time = Index) |>     # rename index
  pivot_longer(
    -time,
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    year = year(time),
    hour = hour(time),
    doy  = yday(time),
    month = month(time, label = TRUE)
  ) %>%
  filter(year %in% 1960:1969)

diel <- df |>
  group_by(variable, hour) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

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

