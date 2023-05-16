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


# border for patchwork plot -----------------------------------------------

theme_border <- 
  theme(
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3),
    text = element_text(family = "Times New Roman"))

# data --------------------------------------------------------------------

tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label, city)

w <- 
  read_csv("data_tidy/td_wea.csv") %>% 
  arrange(city) %>% 
  full_join(tk, by = "city")

t <- 
  w %>% 
  filter(grepl("temperature", wea_type))

cp <- 
  w %>% 
  filter(grepl("precip", wea_type))


# figs --------------------------------------------------------------------

fig_cp <-
  CumPrecipFigSummary(f.data = cp)

fig_cp

#--add an arrow to temperature plot
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


