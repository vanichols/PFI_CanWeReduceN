#-notes:
# 4/24 - reran using data-supported prices for corn
# 5/15 reran again using updated numbers (confused)

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/05_figs/00_fig-colors.R")

# theme -------------------------------------------------------------------

my_money_theme <- 
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
    plot.caption.position =  "plot",
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3),
    text = element_text(family = "Times New Roman")) 


# money --------------------------------------------------------

tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label)

d_money_raw <- 
  read_csv("data_tidy/td_money.csv") %>% 
  left_join(tk) %>% 
  select(trial_key, trial_label, everything())
  
#--nrate and yield diffs by rep
d_diffs <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  left_join(tk)


# process data ------------------------------------------------------------


d_money <-
  d_money_raw %>%
  rename(value_max = bestsav_dolac,
         value_mid = midsav_dolac,
         value_min = worstsav_dolac) %>%
  mutate(clr = case_when(
    (value_max < 0 & value_min < 0) ~ "bad",
    (value_max > 0 & value_min < 0) ~ "neutral",
    (value_max > 0 & value_min > 0) ~ "good"
  ))
         

# ordered by max value -------------------------------------------------------------

#--create nice labels for fig
#--don't normalize
d_money_fig <-
  d_money %>% 
  left_join(d_diffs %>% select(trial_label, dif_nrate_lbac) %>% distinct()) %>% 
  mutate(#value_min = value_min / dif_nrate_lbac,
         #value_max = value_max/dif_nrate_lbac,
         #value_mid = value_mid/dif_nrate_lbac,
         trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb/ac"))


d_money_fig %>% 
  ggplot() + 
  geom_hline(yintercept = 0) +
  #--creating white background for alpha
  geom_segment(aes(
    x = reorder(trial_label, value_max),
    xend = reorder(trial_label, value_max),
    y = value_min,
    yend = value_max), 
    color = "white",
    linewidth = 4,
    show.legend = F
  )  +   
  #--alpha
  geom_segment(aes(
    x = reorder(trial_label, value_max),
    xend = reorder(trial_label, value_max),
    y = value_min,
    yend = value_max, 
    color = clr),
    alpha = 0.6,
    linewidth = 4,
    show.legend = F
  )  + 
  #--best case
  geom_segment(aes(
    x = reorder(trial_label, value_max),
    xend = reorder(trial_label, value_max),
    y = value_max - 5,
    yend = value_max, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  #--wrost case
  geom_segment(aes(
    x = reorder(trial_label, value_max),
    xend = reorder(trial_label, value_max),
    y = value_min + 5,
    yend = value_min, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  #--midpoint
  geom_point(aes(
    x = reorder(trial_label, value_max),
    y = value_mid, 
    color = clr),
    #color = "white",
    show.legend = F,
    pch = 17,
    size = 2) +
  geom_text(aes(x = 12, y = 110), label = "Financial savings", check_overlap = T,
            #hjust = 0,
            fontface = "italic", color = pfi_blue) +
  geom_text(aes(x = 7, y = -200), label = "Financial loss", check_overlap = T,
            #hjust = 0,
            fontface = "italic", color = pfi_orange) +
  scale_y_continuous(labels = label_dollar(),
#                     limits = c(-15, 5),
                     #breaks = c(-400, -300, -200, -100, 0, 100, 200, 300)
  ) +
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Dollars\nper acre",
       title = str_wrap("Seven (41%) trials saw potential for savings when reducing N rates",
                        width = 80),
       subtitle = "Ten trials saw financial losses in all price scenarios") + 
  my_money_theme

ggsave("figs/fig04_money.jpg", width = 7, height = 5)


