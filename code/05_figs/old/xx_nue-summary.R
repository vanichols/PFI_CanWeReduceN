library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/00_fig-things.R")

# data --------------------------------------------------------------------

y <- 
  read_csv("data_tidy/yields.csv") |>
  filter(!is.na(yield_buac))

s_raw <- read_csv("data_tidy/stats.csv")

s <- 
  s_raw |> 
  select(trt, estimate, last_name, diff_est, pval) |> 
  rename(yld_pval = pval,
         yld = estimate,
         yld_dif = diff_est)

nue <- 
  read_csv("data_tidy/stats-nue.csv") |> 
  select(trt, estimate, last_name, diff_est, pval) |> 
  rename(nue_pval = pval,
         nue = estimate,
         nue_dif = diff_est)

#--get rid of reps
d_raw <- 
  y |> 
  left_join(s) |> 
  left_join(nue) |> 
  select(-rep, -yield_buac) |> 
  distinct()

#--assign significance
d <- 
  d_raw |> 
  mutate(yld_sig = ifelse(yld_pval < 0.05, "*", " "),
         nue_sig = ifelse(nue_pval < 0.05, "*", " ")) %>% 
  select(last_name, trt, yld, yld_dif, yld_sig, nue, nue_dif, nue_sig) |> 
  distinct() 

# theme -------------------------------------------------------------------

my_nue_theme <- 
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,
                                vjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.3)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot"
  ) 



# nue yields---------------------------------------------------------------------


d %>% 
  mutate(nue2 = 1/nue) %>% 
  ggplot(aes(reorder(last_name, nue2, max), nue2)) + 
  geom_point(aes(size = yld, color = nue_sig, shape = trt)) +
  labs(title = "Less nitrogen application per bushel is possible for 15 of the 17 sites",
       subtitle = "At 14 sites, N was reduced too much")

#--using a n response curve, you want the change in bu/n to be close to 0?? Ugh I need to think

d %>% 
  mutate(nue_dif2 = 1/nue_dif) %>% 
  ggplot(aes(reorder(last_name, nue_dif2, max), nue_dif2)) + 
  geom_point(aes(size = yld, color = nue_sig)) +
  labs(title = "Less nitrogen application per bushel is possible for 15 of the 17 sites",
       subtitle = "At 14 sites, N was reduced too much")

# nue trad---------------------------------------------------------------------


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
    aes(reorder(last_name, -typ), typ,
        fill = nue_sig),
    show.legend = F,
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
  scale_fill_manual(values = c(pfi_green, pfi_tan)) +
  labs(
    x = NULL,
    y = "Nitrogen applied\nper unit corn produced\n(lb N/bu corn)",
    title = str_wrap("Less nitrogen (N) applied per bushel of corn at the reduced N rate", width = 70),
    subtitle = "Majority of trials saw significant reductions in N applied per bushel of corn",
    caption = "* = Statistically significant change at a 95% confidence level\nNS = No significant change"
  )

ggsave("figs/nue-change.png", width = 7, height = 5)

