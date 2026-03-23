rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

load("~/Downloads/analysis.RData")

plot(datum$emean$agb,type = "l")

months <- datum$month
years <- datum$year

rain <- datum$emean$rain
plot(rain,type = 'l')

nep <- datum$emean$nep
tas <- datum$emean$atm.temp

plot(tas,type = 'l')

df <- data.frame(month = months,
                 year = years,
                 nep,
                 rain,
                 tas)

df.long <- df %>%
  pivot_longer(cols = -c(month,year))

df.seas <- df.long %>%
  group_by(month,name) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.seas) +
  geom_line(aes(x = month,
                y = value.m)) +
  facet_wrap(~name,scales = "free") +
  theme_bw()


matplot(t(datum$qmean$nee),type = "l")

matplot((datum$szpft$gpp[,12,c(2,3,4)]), type = "l")
matplot((datum$szpft$agb[,12,c(2,3,4)]), type = "l")
matplot((datum$szpft$npp[,12,c(2,3,4)]), type = "l")

