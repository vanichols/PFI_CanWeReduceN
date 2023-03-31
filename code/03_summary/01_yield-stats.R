library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(agricolae)

rm(list = ls())

source("code/00_fig-things.R")
theme_set(my_wea_theme)

# data --------------------------------------------------------------------

d <- read_csv("data_tidy/yields.csv")


y <-
  d |> 
  select(-nrate_lbac) |> 
  filter(!(last_name == "Bakehouse" & rep == 1))

  

p_n <- 
  d |> 
  select(-yield_buac) |> 
  pivot_wider(names_from = trt, values_from = nrate_lbac) |> 
  filter(rep == 2) |> 
  mutate(pct_red = (typ - red)/typ * 100) |> 
  select(last_name, pct_red)

d_y <- 
  y |> 
  pivot_wider(names_from = trt, values_from = yield_buac) |> 
  mutate(y_diff = red- typ) |> 
  group_by(last_name) |> 
  summarise(y_diff = mean(y_diff, na.rm = T))
  
pd_y <- 
  d_y |> 
  left_join(y |> 
              filter(trt == "typ") |> 
              select(last_name, yield_buac) |> 
              group_by(last_name) |> 
              summarise(typ_yld = mean(yield_buac, na.rm = T))) |> 
    mutate(p_yld_chng = y_diff/typ_yld * 100) |> 
    select(last_name, p_yld_chng)

ln <- 
  y |> 
  pull(last_name) |> 
  unique()

# stats -------------------------------------------------------------------

#--test individual
tst <- y |> filter(last_name == "Amundson")

mod <- lm(yield_buac ~ trt, data = tst)

pval <- 
  anova(mod) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  slice(1) |> 
  pull(pr_f)

lsd <- 
  (LSD.test(mod, "trt"))$statistics |> 
  as_tibble() |> 
  pull(LSD)

res <- NULL

for (i in 1:length(ln)) {

  tmp.ln <- ln[i]
  d.tmp <- y |> filter(last_name == tmp.ln) 
  
  tmp.mod <- lm(yield_buac ~ trt, data = d.tmp)
  
  tmp.pval <- 
    anova(tmp.mod) |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    slice(1) |> 
    pull(pr_f)
  
  tmp.lsd <- 
    (LSD.test(tmp.mod, "trt"))$statistics |> 
    as_tibble() |> 
    pull(LSD)

  tmp.res <- 
    tibble(last_name = tmp.ln,
           pval = tmp.pval,
           lds = tmp.lsd)
  
  res <- bind_rows(res, tmp.res)
    
}

res


res |> 
  write_csv("data_tidy/stats.csv")

