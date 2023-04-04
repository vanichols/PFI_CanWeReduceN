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

# theme -------------------------------------------------------------------

my_nue_theme <- 
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,
                                vjust = 0.5),
    axis.title = element_text(size = rel(1.0)),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.2)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    panel.border = element_blank()
    
  ) 


theme_set(my_nue_theme)

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
  scale_y_continuous(limits = c(0, 2.5)) +
  labs(
    x = NULL,
    y = "Nitrogen applied\nper unit corn produced\n(lb N/bu corn)",
    title = str_wrap("Of the 17 trials, 15 saw significant reductions in nitrogen applied per unit corn yield at the reduced N rate", width = 70),
    caption = "* = Statistiignificant change at a 95% confidence level\nNS = No statistically significant change"
  )

ggsave("figs/nue-change.png", width = 8, height = 6)

