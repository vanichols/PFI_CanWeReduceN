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
       y = "Change in corn yield\nwith reduced N rate\n(bu/ac)") 

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
       y = "Change in corn yield\nwith reduced N rate\n(bu/ac)",
       title = "NEEDS TITLE") 

ggsave("figs/yield-diff-width.png", width = 6, height = 4)


#--% red vs yield red

d_redpct <- 
  y |> 
  select(-nrate_lbac) |> 
  pivot_wider(names_from = trt, values_from = yield_buac) |> 
  group_by(last_name) |> 
  summarise(red = mean(red, na.rm = T),
            typ = mean(typ, na.rm = T)) |> 
  mutate(yldred_pct = (typ - red)/typ * 100,
         yldred_buac = (typ - red)) |> 
  select(-red, -typ) |> 
  left_join(n_pct |> 
              mutate(nred_lbac = typ - red) |> 
              select(last_name, red_pct, nred_lbac) |> 
              rename(nred_pct = red_pct))

d_redpct |> 
  ggplot(aes(nred_pct, yldred_pct)) + 
  geom_point() + 
  geom_label(aes(label = last_name))

# nue ---------------------------------------------------------------------

#--arrows from old nue to new
nue_dat <- 
  nue |> 
  pivot_wider(names_from = trt, values_from = nue_mn) |> 
  mutate(nue_sig = ifelse(nue_pval < 0.05, "*", "NS")) 

nue_dat_arrows <- 
  nue_dat |> 
  filter(nue_pval < 0.05)


ggplot() +
  geom_point(
    data = nue_dat,
    aes(reorder(last_name, -typ), typ),
    fill = pfi_green,
    color = "black",
    pch = 21,
    size = 5
  ) +
  geom_arrowsegment(
    data = nue_dat_arrows,
    aes(
      x = last_name,
      xend = last_name,
      y = typ,
      yend = red + 0.01
    ),
    arrows = arrow(type = "closed", length = unit(0.3, "cm"))
  ) +
  geom_text(data = nue_dat,
            aes(reorder(last_name, -typ),  typ + 0.15, label = nue_sig),
            size = 5) +
  labs(
    x = NULL,
    y = "Nitrogen applied\nper unit corn produced\n(lb N/bu corn)",
    title = "Of the 17 trials, 15 saw significant reductions\nin nitrogen applied per unit corn yield at the reduced N rate",
    caption = "* = Statistiignificant change at a 95% confidence level\nNS = No statistically significant change"
  )

ggsave("figs/nue-change.png", width = 8, height = 6)


# d |> 
#   #--make it so it is red-typ
#   mutate(nue_dif = -nue_dif) |>
#   ggplot(aes(reorder(last_name, nue_dif), nue_dif))+ 
#   geom_col(aes(fill = nue_sig), color = "black", show.legend = F) +
#   #geom_text(aes(reorder(last_name, nue_dif), nue_dif - 5, label = nue_sig)) +
#   geom_text(x = 1, y = 0.1, label = "Nitrogen use per bushel increased at reduced N rate",
#             check_overlap = T, hjust = 0, fontface = "italic", 
#             color = "gray50") +
#   geom_text(x = 1, y = -0.5, label = "Nitrogen use per bushel decreased at reduced N rate",
#             check_overlap = T, hjust = 0, fontface = "italic",
#             color = "gray50") +
#   scale_y_continuous(limits = c(-.5, .1)) +
#   scale_fill_manual(values = c(pfi_tan, pfi_blue)) +
#   labs(x = NULL,
#        y = "Change in nitrogen used per busehl\n(lb N/bu)") 
# 
# ggsave("figs/nue-diff.png", width = 6, height = 4)
# 
# 
# 
# 
# d |>
#   left_join(n_pct) |>
#   #--make it so it is red-typ
#   mutate(nue_dif = -nue_dif,
#          red_pct_sc = (red_pct - min(red_pct)) / (max(red_pct) - min(red_pct))) |>
#   ggplot(aes(reorder(last_name, nue_dif), nue_dif)) +
#   geom_col(
#     aes(fill = nue_sig, width = red_pct_sc),
#     color = "black",
#     show.legend = F
#   ) +
#   #geom_text(aes(reorder(last_name, nue_dif), nue_dif - 5, label = nue_sig)) +
#   geom_text(
#     x = 1,
#     y = 0.1,
#     label = "Nitrogen use per bushel increased at reduced N rate",
#     check_overlap = T,
#     hjust = 0,
#     fontface = "italic",
#     color = "gray50"
#   ) +
#   geom_text(
#     x = 1,
#     y = -0.5,
#     label = "Nitrogen use per bushel decreased at reduced N rate",
#     check_overlap = T,
#     hjust = 0,
#     fontface = "italic",
#     color = "gray50"
#   ) +
#   scale_y_continuous(limits = c(-.5, .1)) +
#   scale_fill_manual(values = c(pfi_tan, pfi_blue)) +
#   labs(x = NULL,
#        y = "Change in nitrogen applied\nper bushel of corn yielded\n(lb N/bu)")
# 
# ggsave("figs/nue-diff-width.png", width = 6, height = 4)
