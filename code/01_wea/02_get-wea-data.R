library(tidygeocoder)
library(nasapower)
library(tidyverse)

rm(list = ls())


#--read in the raw cooperator/city locations
d <- read_csv("data_tidy/coop-lat-lon.csv")

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
