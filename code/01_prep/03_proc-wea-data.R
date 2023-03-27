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

t_lt <- 
  w |> 
  group_by(lon, lat, city, doy) |> #--can't group by mm and dd, not always the same doy 
  summarise(t_lt = mean(t2m)) 

t_22 <- 
  w |> 
  filter(year == 2022) |> 
  group_by(lon, lat, city, mm, dd, doy) |> 
  summarise(t_22 = mean(t2m)) 

t_final <- 
  t_lt |> 
  left_join(t_22) |>
  pivot_longer(c(t_lt, t_22))

# precip ------------------------------------------------------------------

p <- 
  w |> 
  group_by(city, year) |> 
  mutate(
    precip_in = prectotcorr,
    cumprecip_in = cumsum(precip_in)) 

#--long term
p_lt <- 
  p |>  
  group_by(lon, lat, city, doy) |> 
  summarise(cp_lt = mean(cumprecip_in)) 

p_22 <- 
  p |> 
  filter(year == 2022) |> 
  group_by(lon, lat, city, mm, dd, doy) |> 
  summarise(cp_22 = mean(cumprecip_in)) 

p_final <- 
  p_lt |> 
  left_join(p_22) |> 
  pivot_longer(c(cp_lt, cp_22))


# temperature, cum --------------------------------------------------------

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
  pivot_longer(c(ct_lt, ct_22))


# figurability ------------------------------------------------------------

p_final |> 
  filter(city == "albion") |> 
  ggplot(aes(doy, value)) +
  geom_line(aes(color = name), size = 3)

t_final |> 
  filter(city == "albion") |> 
  ggplot(aes(doy, value)) +
  geom_line(aes(color = name), size = 3)

ct_final |> 
  filter(city == "albion") |> 
  ggplot(aes(doy, value)) +
  geom_line(aes(color = name), size = 3)


# write -------------------------------------------------------------------

t_final |> 
  write_csv("data_wea/temperature-F.csv")

ct_final |> 
  write_csv("data_wea/cum-temperature-F.csv")

p_final |> 
  write_csv("data_wea/cum-precip-in.csv")
