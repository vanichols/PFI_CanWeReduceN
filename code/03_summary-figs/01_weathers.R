library(tidyverse)
library(scales)
library(lubridate)

# data --------------------------------------------------------------------

t <- read_csv("data_wea/temperature-F.csv")

ct <- read_csv("data_wea/cum-temperature-F.csv")

p <- read_csv("data_wea/precip-in.csv")

cp <- read_csv("data_wea/cum-precip-in.csv")



# summary figs ------------------------------------------------------------

#--deviation from long term average, temp
t |>
  group_by(city, mm, name) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(dev_t = t_22 - t_lt) |> 
  ggplot(aes(mm, dev_t)) + 
  geom_hline(yintercept = 0) +
  geom_line(aes(group = city), color= "red")


#--deviation from long term average, precip
p |>
  group_by(city, mm, name) |>
  summarise(value = mean(value, na.rm = T)) |>
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(dev_p = p_22 - p_lt) |> 
  ggplot(aes(mm, dev_p)) + 
  geom_hline(yintercept = 0) +
  geom_line(aes(group = city), color= "red")


#--deviation from long term average, cum precip
#--why doesn't it go the entire year?
cp |>
  mutate(dd_date = as.Date(doy - 1, 
                           origin = "2022/01/01")) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(dev_cp = cp_22 - cp_lt) |> 
  ggplot(aes(dd_date, dev_cp)) + 
  geom_hline(yintercept = 0) +
  geom_line(aes(group = city), color= "red") + 
  scale_x_date()

