library(tidygeocoder)
library(nasapower)
library(tidyverse)


#--read in the raw cooperator/city locations
d <- read_csv("data_raw/byhand_cooperator-locations.csv", skip = 5)

# https://stackoverflow.com/questions/68671631/getting-latitude-and-longitude-in-r-with-only-city-state


#--add lat and long

d1 <- 
  d |> 
  geocode(city = city, state = state, method = 'osm', lat = latitude , long = longitude)

#--write it
d1 |> 
  write_csv("data_tidy/coop-lat-lon.csv")
