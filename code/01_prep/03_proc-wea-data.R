library(tidyverse)

rm(list = ls())

my_wea_files <- fs::dir_ls("data_wea/raw/", regexp = "\\.csv$")

w <- 
  my_wea_files %>% 
  map_dfr(read_csv) |> 
  janitor::clean_names() 

# long term ---------------------------------------------------------------

t_lt <- 
  w |> 
  group_by(lon, lat, city, doy) |> 
  summarise(t2m = mean(t2m)) |> 
  mutate(time_frame = "1982-2022")

t_22 <- 
  w |> 
  filter(year == 2022) |> 
  group_by(lon, lat, city, doy) |> 
  summarise(t2m = mean(t2m)) |> 
  mutate(time_frame = "2022")

t <- 
  t_lt |> 
  bind_rows(t_22)

t |> 
  filter(city == "hastings") |> 
  ggplot(aes(doy, t2m)) + 
  geom_line(aes(color = time_frame))

t |> 
  mutate(t2m = weathermetrics::celsius.to.fahrenheit(t2m),
         t2m = ifelse(t2m <0, 0, t2m)) |> 
  pivot_wider(names_from = time_frame,
              values_from = t2m) |> 
  janitor::clean_names() |>
  group_by(city) |> 
  mutate(cum_lt = cumsum(x1982_2022),
         cum_22 = cumsum(x2022)) |> 
  filter(city == "albion") |> 
  ggplot() + 
  geom_line(aes(doy, cum_22), color= "red") + 
  geom_line(aes(doy, cum_lt), color= "black") +
  facet_wrap(~city)

t |> 
  mutate(t2m = weathermetrics::celsius.to.fahrenheit(t2m),
         t2m = ifelse(t2m <0, 0, t2m)) |> 
  pivot_wider(names_from = time_frame,
              values_from = t2m) |> 
  janitor::clean_names() |>
  group_by(city) |> 
  mutate(cum_lt = x1982_2022,
         cum_22 = x2022) |> 
  filter(city == "albion") |> 
  ggplot() + 
  geom_line(aes(doy, cum_22), color= "red") + 
  geom_line(aes(doy, cum_lt), color= "black") +
  facet_wrap(~city)
