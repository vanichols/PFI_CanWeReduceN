library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/00_fig-things.R")
theme_set(my_wind_theme)

# data --------------------------------------------------------------------

y <- read_csv("data_tidy/yields.csv") |>
  filter(!is.na(yield_buac))

#--make up dollar amounts
#--N high price = $1.20/lb
#--N low price = $0.60/lb
n_hi <- 1.2
n_lo <- 0.6
n_av <- mean(c(n_hi, n_lo))

#--corn price = $7/bu
crn_hi <- 8
crn_lo <- 5.5
crn_av <- mean(c(crn_hi, crn_lo))

m <- 
  y |> 
  group_by(last_name, trt) |> 
  summarise(nrate_lbac = mean(nrate_lbac, na.rm = T),
            yield_buac = mean(yield_buac, na.rm = T)) |> 
  mutate(ncost_hi = n_hi * nrate_lbac,
         ncost_lo = n_lo * nrate_lbac,
         ncost_av = n_av * nrate_lbac,
         
         crev_hi = crn_hi * yield_buac,
         crev_lo = crn_lo * yield_buac,
         crev_av = crn_av * yield_buac) 


m |> 
  mutate(most_savings = crev_lo - ncost_hi,
         least_savings = crev_hi - ncost_lo,
         avg_savings = crev_av - ncost_av) |> 
  select(last_name, trt, contains("savings"))|> 
  pivot_longer(most_savings:avg_savings) |> 
  pivot_wider(names_from = trt, values_from = value) |> 
  mutate(savings = red - typ,
         assump = case_when(
           name == "most_savings" ~ "Expensive N, low priced corn",
           name == "least_savings" ~ "Cheap N, high priced corn",
           name == "avg_savings" ~ "Avg N, avg priced corn",
         )) |> 
  ggplot() + 
  geom_point(aes(reorder(last_name, typ), typ), color = pfi_dkgreen) + 
  geom_point(aes(last_name, red), color = pfi_green) + 
  facet_grid(.~ assump)


#--show ranges of high and low
m |> 
  mutate(most_savings = crev_lo - ncost_hi,
         least_savings = crev_hi - ncost_lo,
         avg_savings = crev_av - ncost_av) |> 
  select(last_name, trt, contains("savings")) %>%
  pivot_longer(most_savings:ncol(.)) |> 
  pivot_wider(names_from = trt, values_from = value) |> 
  mutate(savings = red - typ) |> 
  select(last_name, name, savings) |> 
  pivot_wider(names_from = name, values_from = savings) |> 
  mutate(clr = case_when(
    (most_savings < 0 & least_savings < 0) ~ "bad",
    (most_savings > 0 & least_savings < 0) ~ "neutral",
    (most_savings > 0 & least_savings > 0) ~ "good",
  )) |> 
  ggplot() + 
  geom_hline(yintercept = 0) +
  geom_segment(aes(
    x = reorder(last_name, -most_savings),
    xend = reorder(last_name, -most_savings),
    y = least_savings,
    yend = most_savings, 
    color = clr),
    size = 3,
    show.legend = F
  )  + 
  geom_point(aes(
    x = reorder(last_name, -most_savings),
    y = avg_savings),
    color = "white",
    size = 1) +
  geom_text(aes(x = 1, y = 180), label = "Financial advantage from reduced nitrogen rate", check_overlap = T, hjust = 0,
            fontface = "italic", color = "gray50") +
  geom_text(aes(x = 1, y = -400), label = "Financial loss from reduced nitrogen rate", check_overlap = T, hjust = 0,
            fontface = "italic", color = "gray50") +
  scale_y_continuous(labels = label_dollar(), limits = c(-410, 200), breaks = c(-400, -300, -200, -100, 0, 100, 200)) + 
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Dollars per acre",
       title = str_wrap("7 of the 17 trials saw a financial advantage to the reduced nitrogen rate under average price scenarios, 10 saw financial losses",
                          width = 80))

ggsave("figs/monetary-diffs.png", width = 8, height = 6)
