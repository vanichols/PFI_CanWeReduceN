#-notes:
# 4/24 - reran using data-supported prices for corn
# 5/15 reran again using updated numbers (confused)

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/04_figs/00_fig-colors.R")

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


# money by rep --------------------------------------------------------

tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label)

d_money_raw <- 
  read_csv("data_tidy/td_money.csv") %>% 
  left_join(tk) %>% 
  select(trial_key, trial_label, everything())
  
st_money <- 
  read_csv("data_stats/stat_money.csv") %>% 
  left_join(tk)

#--nrate and yield diffs by rep
d_diffs <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  left_join(tk)


# process data ------------------------------------------------------------

d_money <- 
  d_money_raw %>%
  pivot_longer(4:ncol(.)) %>% 
  left_join(st_money %>% select(-estimate) %>% rename(name = var)) %>% 
  group_by(trial_label) %>% 
  mutate(pval_max = max(pval),
         value_min = min(value, na.rm = T),
         value_max = max(value, na.rm = T)) %>%
  ungroup() %>% 
  mutate(fin_sig = ifelse(pval_max < 0.05, "*", "NS"),
         clr = case_when(
         (value_max < 0 & value_min < 0) ~ "bad",
         (value_max > 0 & value_min < 0) ~ "neutral",
         (value_max > 0 & value_min > 0) ~ "good"
         ))
         

d_money_simp <-
  d_money %>%
  select(trial_label, value_min, value_max, fin_sig, clr) %>% 
  left_join(
    st_money %>% 
      filter(var == "midsav_dolac") %>% 
      select(trial_label, estimate) %>% 
      rename(midsav_dolac = estimate)
  ) %>% 
  distinct()
  

# ordered by max value -------------------------------------------------------------

#--create nice labels for fig
d_money_fig <-
  d_money_simp %>% 
  left_join(d_diffs %>% select(trial_label, dif_nrate_lbac) %>% distinct()) %>% 
  mutate(value_min = value_min / dif_nrate_lbac,
         value_max = value_max/dif_nrate_lbac,
         midsav_dolac = midsav_dolac/dif_nrate_lbac,
         trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb/ac"))


d_money_fig %>% 
  filter(grepl("Bardole", trial_label))

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
    y = value_max-0.2,
    yend = value_max, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  #--wrost case
  geom_segment(aes(
    x = reorder(trial_label, value_max),
    xend = reorder(trial_label, value_max),
    y = value_min+0.2,
    yend = value_min, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  #--midpoint
  geom_point(aes(
    x = reorder(trial_label, value_max),
    y = midsav_dolac, 
    color = clr),
    #color = "white",
    show.legend = F,
    pch = 17,
    size = 2) +
  geom_text(aes(x = 12, y = 4), label = "Financial savings", check_overlap = T,
            #hjust = 0,
            fontface = "italic", color = pfi_blue) +
  geom_text(aes(x = 7, y = -10), label = "Financial loss", check_overlap = T,
            #hjust = 0,
            fontface = "italic", color = pfi_orange) +
  scale_y_continuous(labels = label_dollar(),
                     #limits = c(-15, 5),
                     #breaks = c(-400, -300, -200, -100, 0, 100, 200, 300)
  ) +
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "$/ac per\nlb N reduced",
       title = str_wrap("Over half the trials saw potential for savings when reducing N rates",
                        width = 80),
       subtitle = "Eight trials saw financial losses in all price scenarios, the remaining nine saw potential savings") + 
  my_money_theme



# ordered by mid -------------------------------------------------------------

d_money_fig %>% 
  ggplot() + 
  geom_hline(yintercept = 0) +
  #--creating white background for alpha
  geom_segment(aes(
    x = reorder(trial_label, midsav_dolac),
    xend = reorder(trial_label, midsav_dolac),
    y = value_min,
    yend = value_max), 
    color = "white",
    linewidth = 4,
    show.legend = F
  )  +   
  #--alpha
  geom_segment(aes(
    x = reorder(trial_label, midsav_dolac),
    xend = reorder(trial_label, midsav_dolac),
    y = value_min,
    yend = value_max, 
    color = clr),
    alpha = 0.6,
    linewidth = 4,
    show.legend = F
  )  + 
  #--best case
  geom_segment(aes(
    x = reorder(trial_label, midsav_dolac),
    xend = reorder(trial_label, midsav_dolac),
    y = value_max-0.2,
    yend = value_max, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  #--wrost case
  geom_segment(aes(
    x = reorder(trial_label, midsav_dolac),
    xend = reorder(trial_label, midsav_dolac),
    y = value_min+0.2,
    yend = value_min, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  #--midpoint
  geom_point(aes(
    x = reorder(trial_label, midsav_dolac),
    y = midsav_dolac, 
    color = clr),
    #color = "white",
    show.legend = F,
    pch = 17,
    size = 2) +
  # geom_text(aes(
  #   x = reorder(trial_label, midsav_dolac),
  #   y = -11,
  #   label = paste0(round(dif_nrate_lbac), " lb/ac")),
  #   color = "gray",
  #   fontface = "italic") +
  geom_text(aes(x = 12, y = 4), label = "Financial savings", check_overlap = T,
            #hjust = 0,
            fontface = "italic", color = pfi_blue) +
  geom_text(aes(x = 7, y = -10), label = "Financial loss", check_overlap = T,
            #hjust = 0,
            fontface = "italic", color = pfi_orange) +
  scale_y_continuous(labels = label_dollar(),
                     #limits = c(-15, 5),
                     #breaks = c(-400, -300, -200, -100, 0, 100, 200, 300)
  ) +
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "$/ac per\nlb N reduced",
       title = str_wrap("Over half the trials saw potential for savings when reducing N rates",
                        width = 80),
       subtitle = "Eight trials saw financial losses in all price scenarios, the remaining nine saw potential savings") + 
  my_money_theme


ggsave("figs/money.png", width = 7, height = 5)
