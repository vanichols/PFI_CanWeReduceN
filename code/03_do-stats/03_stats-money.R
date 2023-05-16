library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(agricolae)
library(broom)
library(emmeans)

rm(list = ls())


# data --------------------------------------------------------------------

m <- read_csv("data_tidy/td_money.csv")


#--get the trial keys to loop through 
tk <- 
  m |> 
  pull(trial_key) |> 
  unique()



#--test individual
tst <- m |> filter(trial_key== "dool_22")


# t-test ------------------------------------------------------------------

mod <- t.test(tst$midsav_dolac)

tidy(mod)

res <- NULL

i <- 1

for (i in 1:length(tk)) {
  
  tmp.tk <- tk[i]
  d.tmp <- m |> filter(trial_key == tmp.tk) 
  
  #--most savings
  tmp.mod1 <- t.test(d.tmp$bestsav_dolac)
  tmp.p1 <- 
    tidy(tmp.mod1) |> 
    select(estimate, p.value) %>% 
    mutate(var = "bestsav_dolac")

  #--avg savings
  tmp.mod2 <- t.test(d.tmp$midsav_dolac)
  tmp.p2 <- 
    tidy(tmp.mod2) |> 
    select(estimate, p.value) %>% 
    mutate(var = "midsav_dolac")
  
  #--least savings
  tmp.mod3 <- t.test(d.tmp$worstsav_dolac)
  tmp.p3 <- 
    tidy(tmp.mod3) |> 
    select(estimate, p.value) %>% 
    mutate(var = "worstsav_dolac")
  
  tmp.res <-
    tmp.p1 |>
    bind_rows(tmp.p2) %>% 
    bind_rows(tmp.p3) %>% 
    rename(pval = p.value) %>% 
    mutate(
      trial_key = tmp.tk)
  
  res <- bind_rows(res, tmp.res)
  
}

res


res |> 
  write_csv("data_stats/stat_money.csv")


