#--purpose: use lat/long to ping nasa power database
#--get temperature at 2 m height
#--get corrected precipitation
#--note it takes a while to run
#--reran 4/25/23 bc thigns weren't matching the hand downloaded data https://power.larc.nasa.gov/data-access-viewer/

library(nasapower)
library(tidyverse)

rm(list = ls())


#--keep just lat/lon, use vity as identifier to avoid getting veenstra twice
d <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(city, latitude, longitude)

# loop --------------------------------------------------------------------

uni_cities <- 
  d |> 
  pull(city) |> 
  unique()

for (i in 1:length(uni_cities)) {
  
    c.tmp <- uni_cities[i]
  
  d.tmp <- 
    d |>
    select(city, latitude, longitude) |> 
    filter(city == c.tmp) |> 
    distinct()
  
  t.tmp <- 
    get_power(
      community = "ag",
      lonlat = c(d.tmp$longitude, d.tmp$latitude),
      pars = c("T2M", "PRECTOTCORR"),
      dates = c("1982-01-01", "2022-12-31"),
      temporal_api = "daily"
    ) |> 
    mutate(city = paste(c.tmp))
  
  t.tmp |> 
    write_csv(paste0("data_wea/raw/", c.tmp, ".csv"))
  
}
