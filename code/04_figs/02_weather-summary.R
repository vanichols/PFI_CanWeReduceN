#--summary of all weathers at all trials
#--these functions seem to be different compared to the 'wea fxn file'
#--theme and weather files have Times New Roman

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)

rm(list = ls())
source("code/04_figs/00_weather-figs-fxns.R")
source("code/04_figs/00_fig-colors.R")
source("code/04_figs/00_fig-themes.R")

# data --------------------------------------------------------------------

ln_key <- 
  read_csv("data_raw/byhand_cooperator-metadata.csv", skip = 5) |> 
  select(last_name, city) %>% 
  distinct()

t <- 
  read_csv("data_wea/temperature-F.csv") |> 
  arrange(city) %>% 
  full_join(ln_key, by = "city")

ct <- read_csv("data_wea/cum-temperature-F.csv") |> 
  full_join(ln_key)

p <- read_csv("data_wea/precip-in.csv") |> 
  full_join(ln_key)

cp <- read_csv("data_wea/cum-precip-in.csv") |> 
  full_join(ln_key)



# figs --------------------------------------------------------------------

fig_cp <-
  CumPrecipFigSummary(f.data = cp)

fig_cp

fig_t <- 
  TempFigSummary(f.data = t) + 
    geom_text(aes(x = as.Date("2022-06-28"), y = -2.5, label = "Cool Aprils"),
              family = "Times New Roman",
              check_overlap = T) + 
    geom_segment(aes(xend = as.Date("2022-04-15"),
                     yend = -3.5, 
                     x = as.Date("2022-06-05"),
                     y = -2.7),
                 arrow = arrow())
  

fig_t



fig_t + fig_cp + 
  plot_annotation(
    theme = theme_border,
    title = str_wrap("Weather deviations from historical averages", width = 80), 
    subtitle = str_wrap("Cool Aprils, dry growing seasons at all 17 trials", width = 80))

ggsave("figs/wea.png", width = 7, height = 5)


