#-notes:
# 4/24 - reran using data-supported prices for corn
# 5/23 - changed to be based on statistical yields

library(tidyverse)
library(lubridate)

rm(list = ls())


# data --------------------------------------------------------------------

nrates <- 
  read_csv("data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(nrate_lbac)) %>% 
  select(trial_key, trt, nrate_lbac) %>% 
  distinct()

#--yields
y_raw <- 
  read_csv("data_stats/stat_cornyield.csv")

#--use statistical means (either grand mean or trt mean if trt is sig)
y <- 
  y_raw %>% 
  group_by(trial_key) %>% 
  mutate(grand_mean = mean(estimate),
         yield_buac = ifelse(pval < 0.05, estimate, grand_mean)
         #yield_buac = estimate
         ) %>% 
  select(trial_key, trt, yield_buac) %>% 
  left_join(nrates)

#--stefan provided range in N prices
#--N high price = $1.20/lb
#--N low price = $0.60/lb
n_hi <- 1.2
n_lo <- 0.6
n_av <- mean(c(n_hi, n_lo))

#--corn price in 2022 https://www.extension.iastate.edu/agdm/crops/pdf/a2-11.pdf
crn_hi <- 7.48
crn_lo <- 5.70
crn_av <- mean(c(crn_hi, crn_lo))


m <- 
  y |> 
  mutate(ncost_hi = n_hi * nrate_lbac,
         ncost_lo = n_lo * nrate_lbac,
         ncost_av = n_av * nrate_lbac,
         
         crev_hi = crn_hi * yield_buac,
         crev_lo = crn_lo * yield_buac,
         crev_av = crn_av * yield_buac) 


#--show ranges of high and low

d_money <- 
  m |> 
  mutate(bestsav_dolac = crev_lo - ncost_hi,
         worstsav_dolac = crev_hi - ncost_lo,
         midsav_dolac = crev_av - ncost_av) |> 
  select(trial_key, trt, contains("sav")) %>%
  pivot_longer(bestsav_dolac:ncol(.)) |> 
  pivot_wider(names_from = trt, values_from = value) |> 
  mutate(savings = red - typ) |> 
  select(trial_key, name, savings) |> 
  pivot_wider(names_from = name, values_from = savings) 

d_money |> 
  write_csv("data_tidy/td_money.csv")

