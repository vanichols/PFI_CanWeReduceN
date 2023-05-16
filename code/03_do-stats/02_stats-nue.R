library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(agricolae)
library(broom)
library(emmeans)

rm(list = ls())



# nue ---------------------------------------------------------------------

d_raw <- read_csv("data_tidy/td_cornyields.csv")


nue <-
  d_raw  |> 
  filter(!(trial_key == "bake_22" & rep == 1)) |> 
  mutate(nue = nrate_lbac/yield_buac)


#--get the trial keys to loop through 
tk <- 
  nue |> 
  pull(trial_key) |> 
  unique()



#--test individual
tst <- nue |> filter(trial_key== "dool_22")


# nue stats ---------------------------------------------------------------------

mod1 <- lm(nue ~ trt, data = tst)

res <- NULL

i <- 1

for (i in 1:length(tk)) {
  
  tmp.tk <- tk[i]
  d.tmp <- y |> filter(trial_key == tmp.tk) 
  
  tmp.mod <- lm(nue ~ trt, data = d.tmp)
  
  tmp.em <- 
    emmeans(tmp.mod, "trt") |> 
    tidy() |> 
    select(-std.error, -df, -p.value)
  
  tmp.smy <- 
    summary(tmp.mod) %>% 
    tidy()  %>%
    filter(term == "trttyp")
  
  tmp.lsd <- 
    (LSD.test(tmp.mod, "trt"))$statistics |> 
    as_tibble() |> 
    pull(LSD)
  
  tmp.res <-
    tmp.em |>
    mutate(
      trial_key = tmp.tk,
      diff_est = tmp.smy |> pull(estimate),
      pval = tmp.smy |> pull(p.value),
      lsd = tmp.lsd
    )
  
  res <- bind_rows(res, tmp.res)
  
}

res

res |> 
  select(trial_key, everything()) %>% 
  write_csv("data_stats/stat-nue.csv")



