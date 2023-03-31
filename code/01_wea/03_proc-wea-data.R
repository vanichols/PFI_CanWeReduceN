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

# precip ---------------------------------------------------------------

#--long term precip
p_lt <- 
  w |> 
  filter(!is.na(prectotcorr)) |> 
  group_by(lon, lat, city, doy) |> #--can't group by mm and dd, not always the same doy 
  summarise(p_lt = mean(prectotcorr, na.rm = T)) 

#--precip that year
p_22 <- 
  w |> 
  filter(!is.na(prectotcorr)) |> 
  filter(year == 2022) |> 
  group_by(lon, lat, city, mm, dd, doy) |> 
  summarise(p_22 = mean(prectotcorr, na.rm = T)) 

p_final <- 
  p_lt |> 
  left_join(p_22) |>
  pivot_longer(c(p_lt, p_22)) |> 
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


# temperature, cum --------------------------------------------------------

#--growing degree days, base 0
ct_lt <- 
  t_lt |> 
  group_by(city) |> 
  mutate(
    t_lt = ifelse(t_lt < 0, 0, t_lt),
    ct_lt = cumsum(t_lt)) |> 
  select(-t_lt)

ct_22 <- 
  t_22 |> 
  group_by(city) |> 
  mutate(
    t_22 = ifelse(t_22 < 0, 0, t_22),
    ct_22 = cumsum(t_22)) |> 
  select(-t_22)

ct_final <- 
  ct_lt |> 
  left_join(ct_22) |> 
  pivot_longer(c(ct_lt, ct_22))|> 
  #--remove day 366
  filter(!is.na(mm))



# write -------------------------------------------------------------------

t_final |> 
  write_csv("data_wea/temperature-F.csv")

ct_final |> 
  write_csv("data_wea/cum-temperature-F.csv")

p_final |> 
  write_csv("data_wea/precip-in.csv")

pc_final |> 
  write_csv("data_wea/cum-precip-in.csv")
