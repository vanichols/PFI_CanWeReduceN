library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)


rm(list = ls())

source("code/00_fig-things.R")
theme_set(my_wind_theme)

# data --------------------------------------------------------------------


y <- read_csv("data_tidy/yields.csv") |>
  filter(!is.na(yield_buac))

s_raw <- read_csv("data_tidy/stats.csv")

s <- 
  s_raw |> 
  select(trt, estimate, last_name, diff_est, pval) |> 
  rename(yld_pval = pval,
         yld_mn = estimate,
         yld_dif = diff_est)

nue <- read_csv("data_tidy/stats-nue.csv") |> 
  select(trt, estimate, last_name, diff_est, pval) |> 
  rename(nue_pval = pval,
         nue_mn = estimate,
         nue_dif = diff_est)

d_raw <- 
  y |> 
  left_join(s) |> 
  left_join(nue) |> 
  select(-rep, -yield_buac) |> 
  distinct()

d <- 
  d_raw |> 
  select(last_name, yld_dif, yld_pval, nue_dif, nue_pval) |> 
  distinct() |> 
  mutate(yld_sig = ifelse(yld_pval < 0.05, "*", " "),
         nue_sig = ifelse(nue_pval < 0.05, "*", " "))

my_names <- 
  d |> 
  pull(last_name) |> 
  unique()


# simple summarise --------------------------------------------------------

#--how much did they reduce it?
n_pct <-
  y |> 
  select(-yield_buac, -rep)  |> 
  distinct() |> 
  pivot_wider(names_from = trt, values_from = nrate_lbac) |> 
  mutate(red_pct = (typ - red)/typ * 100)

summary(n_pct$red_pct)

#--range in LSDs?
s_raw |> 
  pull(lsd) |> 
  summary(.)
  
  
#--range in typical rates?
y |> 
  filter(trt == "typ") |> 
  select(nrate_lbac) |> 
  summary(.)

# yield diffs -------------------------------------------------------------

d |> 
  #--make it so it is red-typ
  mutate(yld_dif = -yld_dif) |>
  ggplot(aes(reorder(last_name, yld_dif), yld_dif))+ 
  geom_col(aes(fill = yld_sig), color = "black", show.legend = F) +
  #geom_text(aes(reorder(last_name, yld_dif), yld_dif - 5, label = yld_sig)) +
  geom_text(x = 1, y = 10, label = "Yields increased at reduced N rate",
            check_overlap = T, hjust = 0, fontface = "italic", 
            color = "gray50") +
  geom_text(x = 1, y = -60, label = "Yields decreased at reduced N rate",
            check_overlap = T, hjust = 0, fontface = "italic",
            color = "gray50") +
  scale_y_continuous(limits = c(-65, 15)) +
  scale_fill_manual(values = c(pfi_tan, pfi_orange)) +
  labs(x = NULL,
       y = "Change in corn yield with reduced N rate\n(bu/ac)") 

ggsave("figs/yield-diff.png", width = 4.4, height = 3)

#--yield diffs, width by pct reduction?

d |> 
  left_join(n_pct) |> 
  #--normalize the range in n_pct?
  mutate(yld_dif = -yld_dif,
         red_pct_sc = (red_pct - min(red_pct))/(max(red_pct) - min(red_pct))) |> 
  ggplot(aes(reorder(last_name, yld_dif), yld_dif))+ 
  geom_col(aes(fill = yld_sig, width = red_pct_sc), color = "black", show.legend = F) +
  #geom_text(aes(reorder(last_name, yld_dif), yld_dif - 5, label = yld_sig)) +
  geom_text(x = 1, y = 10, label = "Yields increased at reduced N rate",
            check_overlap = T, hjust = 0, fontface = "italic", 
            color = "gray50") +
  geom_text(x = 1, y = -60, label = "Yields decreased at reduced N rate",
            check_overlap = T, hjust = 0, fontface = "italic",
            color = "gray50") +
  scale_y_continuous(limits = c(-65, 15)) +
  scale_fill_manual(values = c(pfi_tan, pfi_orange)) +
  labs(x = NULL,
       y = "Change in corn yield with reduced N rate\n(bu/ac)") 

ggsave("figs/yield-diff-width.png", width = 4.4, height = 3)


# nue ---------------------------------------------------------------------


d |> 
  #--make it so it is red-typ
  mutate(nue_dif = -nue_dif) |>
  ggplot(aes(reorder(last_name, nue_dif), nue_dif))+ 
  geom_col(aes(fill = nue_sig), color = "black", show.legend = F) +
  #geom_text(aes(reorder(last_name, nue_dif), nue_dif - 5, label = nue_sig)) +
  geom_text(x = 1, y = 0.1, label = "Nitrogen use per bushel increased at reduced N rate",
            check_overlap = T, hjust = 0, fontface = "italic", 
            color = "gray50") +
  geom_text(x = 1, y = -0.5, label = "Nitrogen use per bushel decreased at reduced N rate",
            check_overlap = T, hjust = 0, fontface = "italic",
            color = "gray50") +
  scale_y_continuous(limits = c(-.5, .1)) +
  scale_fill_manual(values = c(pfi_tan, pfi_blue)) +
  labs(x = NULL,
       y = "Change in nitrogen used per busehl\n(lb N/bu)") 

ggsave("figs/nue-diff.png", width = 4.4, height = 3)




d |>
  left_join(n_pct) |>
  #--make it so it is red-typ
  mutate(nue_dif = -nue_dif,
         red_pct_sc = (red_pct - min(red_pct)) / (max(red_pct) - min(red_pct))) |>
  ggplot(aes(reorder(last_name, nue_dif), nue_dif)) +
  geom_col(
    aes(fill = nue_sig, width = red_pct_sc),
    color = "black",
    show.legend = F
  ) +
  #geom_text(aes(reorder(last_name, nue_dif), nue_dif - 5, label = nue_sig)) +
  geom_text(
    x = 1,
    y = 0.1,
    label = "Nitrogen use per bushel increased at reduced N rate",
    check_overlap = T,
    hjust = 0,
    fontface = "italic",
    color = "gray50"
  ) +
  geom_text(
    x = 1,
    y = -0.5,
    label = "Nitrogen use per bushel decreased at reduced N rate",
    check_overlap = T,
    hjust = 0,
    fontface = "italic",
    color = "gray50"
  ) +
  scale_y_continuous(limits = c(-.5, .1)) +
  scale_fill_manual(values = c(pfi_tan, pfi_blue)) +
  labs(x = NULL,
       y = "Change in nitrogen used per busehl\n(lb N/bu)")

ggsave("figs/nue-diff-width.png", width = 4.4, height = 3)
