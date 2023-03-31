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
  select(-nrate_lbac)
  

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


res <- read_csv("data_tidy/stats.csv")

# figs --------------------------------------------------------------------

pd_y |> 
  left_join(p_n) |> 
  ggplot(aes(reorder(last_name, p_yld_chng), p_yld_chng)) + 
  geom_col(aes(fill = pct_red)) + 
  scale_fill_viridis_c()


#--color by significance
pd_y |> 
  left_join(res) |> 
  mutate(sig = ifelse(pval < 0.05, "Y", "N")) |> 
  ggplot(aes(reorder(last_name, p_yld_chng), p_yld_chng)) + 
  geom_col(aes(fill = sig))



y |> 
  ggplot(aes(rep, yield_buac)) +
  geom_point(aes(fill = nrate_lbac), 
             color = "black", 
             pch = 21, 
             size = 4) +
  facet_wrap(~last_name, scale = "free_x") +
  scale_fill_viridis_c()

y |> 
  ggplot(aes(trt, yield_buac, group = rep)) +
  geom_col(aes(fill = nrate_lbac),
           position = position_dodge2(),
           color = "black") +
  facet_wrap(~last_name, scale = "free_x") +
  scale_fill_viridis_c()


#--what if we did it as a % of maximum yield?

#--get means for each group
yld_avg <- 
  y |>
  group_by(last_name, trt) |>
  summarise(avg_yield = mean(yield_buac, na.rm = T),
            nrate_lbac = mean(nrate_lbac, na.rm = T))



y |>
  group_by(last_name) |> 
  mutate(max_yield = max(yield_buac, na.rm = T),
         yield_pct = yield_buac/max_yield) |> 
  ggplot(aes(trt, yield_pct, group = rep)) +
  geom_col(aes(fill = nrate_lbac),
           position = position_dodge2(),
           color = "black") +
  facet_wrap(~last_name, scale = "free_x") +
  scale_fill_viridis_c()



y |>
  ggplot(aes(trt, yield_buac, group = rep)) +
  geom_col(aes(fill = nrate_lbac),
           position = position_dodge2(),
           color = "black") +
  geom_hline(data = yld_avg, 
             aes(yintercept = avg_yield,
                 color = nrate_lbac),
             size = 2) +
  facet_wrap(~last_name, scale = "free_x") +
  scale_fill_viridis_c() +
  scale_color_viridis_c()


y |>
  ggplot(aes(trt, yield_buac, group = rep)) +
  geom_hline(data = yld_avg, 
             aes(yintercept = avg_yield,
                 color = trt),
             size = 2) +
  geom_col(aes(fill = trt),
           position = position_dodge2(),
           color = "black") +
  facet_wrap(~last_name, scale = "free_x") +
  scale_fill_manual(values = c("lightgreen", "darkgreen")) +
  scale_color_manual(values = c("lightgreen", "darkgreen")) +
  theme_bw()







