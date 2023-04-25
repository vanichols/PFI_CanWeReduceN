#-notes:
# 4/24 - reran using data-supported prices for corn

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())


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
crn_hi <- 5.70
crn_lo <- 4.48
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
  mutate(most_savings = crev_lo - ncost_hi,
         least_savings = crev_hi - ncost_lo,
         avg_savings = crev_av - ncost_av) |> 
  select(last_name, rep, trt, contains("savings")) %>%
  pivot_longer(most_savings:ncol(.)) |> 
  pivot_wider(names_from = trt, values_from = value) |> 
  mutate(savings = red - typ) |> 
  select(last_name, rep, name, savings) |> 
  pivot_wider(names_from = name, values_from = savings) 

d_money |> 
  write_csv("data_tidy/money.csv")


