#--make a clean meta-data file
#--add lat and long

rm(list = ls())

library(tidyverse)
library(readxl)
library(tidygeocoder)

# meta data --------------------------------------------------------------------

md <- read_csv("data_raw/byhand_cooperator-metadata.csv", skip = 5)

md2 <- 
  md %>% 
  select(trial_key = trial_code, trial_label, last_name, first_name, everything())



# add lat and lon ---------------------------------------------------------

#--takes awhile to run
md3 <- 
  md2 |> 
  geocode(city = city, state = state, method = 'osm', lat = latitude , long = longitude)

#--add field size for now, later should go into mgmt file


# field sizes --------------------------------------------------------------------

#--made a sheet by hand (field size)
fz <- 
  read_excel("data_raw/byhand_cooperator-field-size.xlsx", skip = 5) %>% 
  mutate(trial_label = ifelse(grepl("veenstra", last_name), "veenstra", last_name)) 

#--average length of field? 1437

m_length <- 
  fz %>% 
  summarise(length_ft = mean(length_ft, na.rm = T)) %>% 
  pull(length_ft)

#--average width? 30

m_width <- 
  fz %>% 
  summarise(width_ft = mean(width_ft, na.rm = T)) %>% 
  pull(width_ft)


#--already have acres
fz_acres <- 
  fz %>% 
  filter(!is.na(acres))

#--fill in NAs
fz_new <- 
  fz %>% 
  filter(is.na(acres)) %>% 
  select(-acres) %>% 
  mutate(width_ft = ifelse(is.na(width_ft), m_width, width_ft),
         length_ft = ifelse(is.na(length_ft), m_length, length_ft),
         ft2 = width_ft * length_ft,
         acres = ft2 * 2.29568e-5) %>% 
  bind_rows(fz_acres) %>% 
  select(last_name = trial_label, strip_size_acres = acres) 

fz_new %>% 
  summarise(ac = mean(strip_size_acres))

#--combind
md4 <- 
  md3 %>% 
  left_join(fz_new, by = "last_name") %>% 
  distinct()


md4 %>% 
  write_csv("data_tidy/td_trialkey.csv")
