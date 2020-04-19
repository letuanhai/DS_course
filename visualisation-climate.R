library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% 
  .$year %>% min()

year <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% 
  .$year

temp <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% 
  .$temp_anomaly

temp[which.max(year)]-temp[which.min(year)]
max(year)
min(year)

p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% 
  ggplot()+
  geom_line(aes(year, temp_anomaly))


p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x=2000, y=.05, label="20th century mean"), col = "blue") +
  geom_hline(aes(yintercept = 0), col = "blue") +
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  geom_line(aes(year, land_anomaly), col = "red")

year[temp > 0]
year[temp < 0]
year[temp >= .5]

greenhouse_gases %>% ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% 
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  geom_vline(xintercept = 1850)

co2_time <- historic_co2 %>% ggplot(aes(year, co2, col = source)) +
  geom_line()

co2_time + scale_x_continuous(limits = c(-3000, 2018))
