#-notes:
# 4/24 - reran using data-supported prices for corn

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/00_fig-things.R")

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
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3)) 


# money by rep --------------------------------------------------------

d_stats <- 
  read_csv("data_tidy/stats-savings.csv")

d_tst <- 
  read_csv("data_tidy/money.csv") %>%
  pivot_longer(3:ncol(.)) %>% 
  left_join(d_stats %>% select(-estimate) %>% rename(name = var))

#--look at money by rep
d_tst  %>%
  filter(!is.na(value)) %>% 
  mutate(sig = ifelse(pval < 0.05, "*", "NS")) %>% 
  ggplot(aes(last_name, value, shape = name, color = sig)) + 
  geom_point(alpha = 1) + 
  #stat_summary() +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("black", "gray"))

ggsave("figs/scratch-money-stats.png")


# data --------------------------------------------------------------------

#--nrate and yield diffs by rep
d_diffs <- read_csv("data_tidy/trt-diffs.csv")

#--if any scenario has a dot above and below the line, it's inconclusive

d_money_raw <- 
  read_csv("data_tidy/money.csv") 

d_money <- 
  d_money_raw %>%
  pivot_longer(3:ncol(.))  %>% 
  left_join(d_stats %>% select(-estimate) %>% rename(name = var)) %>% 
  group_by(last_name) %>% 
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
         

# fig w reps -------------------------------------------------------------

d_money_fig <-
  d_money %>%
  select(last_name, value_min, value_max, fin_sig, clr) %>% 
  left_join(
    d_stats %>% 
      filter(var == "avg_savings") %>% 
      select(last_name, estimate) %>% 
      rename(avg_savings = estimate)
  ) %>% 
  distinct()
  

d_money_fig |> 
  ggplot() + 
  geom_hline(yintercept = 0) +
  geom_segment(aes(
    x = reorder(last_name, value_max),
    xend = reorder(last_name, value_max),
    y = value_min,
    yend = value_max, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  geom_point(aes(
    x = reorder(last_name, value_max),
    y = avg_savings),
    color = "white",
    pch = 17,
    size = 1) +
  geom_text(aes(x = 12, y = 250), label = "Financial advantage at reduced nitrogen rate", check_overlap = T, 
            #hjust = 0,
            fontface = "italic", color = pfi_blue) +
  geom_text(aes(x = 7, y = -300), label = "Financial loss at reduced nitrogen rate", check_overlap = T, 
            #hjust = 0,
            fontface = "italic", color = pfi_orange) +
  scale_y_continuous(labels = label_dollar(), limits = c(-400, 300),
                     breaks = c(-400, -300, -200, -100, 0, 100, 200, 300)) + 
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Dollars\nper acre",
       title = str_wrap("Reduced nitrogen (N) rates were more profitable in some trials",
                        width = 80),
       subtitle = "Three trials were significantly more profitable per acre at the reduced N rate in all price scenarios") + 
  my_money_theme

ggsave("figs/monetary-diffs.png", width = 7, height = 5)


# fig by unit N  -------------------------------------------------------------


d_money %>% 
  left_join(d_diffs) %>% 
  mutate(value_perunitn = value/dif_nrate_lbac)

d_money_fig %>% 
  left_join(d_diffs %>% select(last_name, dif_nrate_lbac) %>% distinct()) %>% 
  mutate(value_min = value_min / dif_nrate_lbac,
         value_max = value_max/dif_nrate_lbac,
         avg_savings = avg_savings/dif_nrate_lbac,
         last_name = paste0(last_name, ", ", round(dif_nrate_lbac), " lb/ac")) %>% 
  ggplot() + 
  geom_hline(yintercept = 0) +
  geom_segment(aes(
    x = reorder(last_name, avg_savings),
    xend = reorder(last_name, avg_savings),
    y = value_min,
    yend = value_max, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  geom_point(aes(
    x = reorder(last_name, avg_savings),
    y = avg_savings),
    color = "white",
    pch = 17,
    size = 1) +
  # geom_text(aes(
  #   x = reorder(last_name, avg_savings),
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
                     limits = c(-15, 5),
                     #breaks = c(-400, -300, -200, -100, 0, 100, 200, 300)
                     ) + 
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "$/ac per\nlb N reduced",
       title = str_wrap("11 trials saw potential for savings when reducing N rates",
                        width = 80),
       subtitle = "Three trials saw reliable savings of up to $0.50/ac per lb N reduced") + 
  my_money_theme

ggsave("figs/monetary-diffs-per-unit-n.png", width = 7, height = 5)
