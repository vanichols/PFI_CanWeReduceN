#--purpose: read in raw nasa weather data
#--summarise into 'figurable' form

library(tidyverse)

rm(list = ls())

my_wea_files <- fs::dir_ls("data_wea/raw/", regexp = "\\.csv$")

w <- 
  my_wea_files %>% 
  map_dfr(read_csv) |> 
  janitor::clean_names() |> 
  mutate(t2m = weathermetrics::celsius.to.fahrenheit(t2m),
         prectotcorr = prectotcorr * 0.0393701) #--change to F and inches

# temperature ---------------------------------------------------------------

#--long term temperature (30 years)
t_lt <- 
  w |> 
  group_by(lon, lat, city, doy) |> #--can't group by mm and dd, not always the same doy 
  summarise(t_lt = mean(t2m, na.rm = T)) 

#--temperature that year
t_22 <- 
  w |> 
  filter(year == 2022) |> 
  group_by(lon, lat, city, mm, dd, doy) |> 
  summarise(t_22 = mean(t2m, na.rm = T)) 

t_final <- 
  t_lt |> 
  left_join(t_22) |>
  pivot_longer(c(t_lt, t_22))|> 
  #--remove day 366
  filter(!is.na(mm))

# cum precip ------------------------------------------------------------------

#--cumulative precip
pc <- 
  w |> 
  filter(!is.na(prectotcorr)) |> 
  group_by(city, year) |> 
  mutate(
    precip_in = prectotcorr,
    cumprecip_in = cumsum(precip_in)) 

#--long term
pc_lt <- 
  pc |>  
  group_by(lon, lat, city, doy) |> 
  summarise(cp_lt = mean(cumprecip_in)) 

#--that year
pc_22 <- 
  pc |> 
  filter(year == 2022) |> 
  group_by(lon, lat, city, mm, dd, doy) |> 
  summarise(cp_22 = mean(cumprecip_in)) 

pc_final <- 
  pc_lt |> 
  left_join(pc_22) |> 
  pivot_longer(c(cp_lt, cp_22)) |> 
  #--remove day 366
  filter(!is.na(mm)) 


# write -------------------------------------------------------------------

t_final %>% 
  mutate(wea_type = "air temperature deg F") %>% 
  bind_rows(pc_final %>% 
              mutate(wea_type = "cumulative precip inches")) %>% 
  write_csv("data_tidy/td_wea.csv")
