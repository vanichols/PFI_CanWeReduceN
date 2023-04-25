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
    plot.caption.position =  "plot"
    
  ) 


theme_set(my_money_theme)


# what is going on --------------------------------------------------------


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

#--if any scenario has a dot above and below the line, it's inconclusive

d_stats <- 
  read_csv("data_tidy/stats-savings.csv")

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
         


# full figure -------------------------------------------------------------

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
  geom_text(aes(x = 12, y = 300), label = "Financial advantage at reduced nitrogen rate", check_overlap = T, 
            #hjust = 0,
            fontface = "italic", color = pfi_blue) +
  geom_text(aes(x = 8, y = -300), label = "Financial loss at reduced nitrogen rate", check_overlap = T, 
            #hjust = 0,
            fontface = "italic", color = pfi_orange) +
  scale_y_continuous(labels = label_dollar(), limits = c(-400, 400),
                     breaks = c(-400, -300, -200, -100, 0, 100, 200, 300, 400)) + 
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Dollars\nper acre",
       title = str_wrap("Reduced nitrogen (N) rates were more profitable in some trials",
                        width = 80),
       subtitle = "Three trials were significantly more profitable per acre at the reduced N rate in all price scenarios")

ggsave("figs/monetary-diffs.png", width = 7, height = 5)


# mock figure -------------------------------------------------------------

#--show ranges of high and low
d_money_fig |> 
  filter(last_name == "Bennett") |> 
  mutate(last_name = "Example") %>% 
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
  geom_segment(aes(xend = 1, x = 1.3,
                   yend = value_max + 1, y = value_max + 12),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(xend = 1, x = 1.2,
                   yend = value_min - 1, y = value_min - 15),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(xend = 1.05, x = 1.3,
                   yend = avg_savings, y = avg_savings + 2),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(aes(x = 1.3, y = value_max + 12), 
            label = "Best-case savings\n(expensive N, low corn revenue)", check_overlap = T, 
            hjust = 0,
            fontface = "italic", color = "gray50") +
  geom_text(aes(x = 1.2, y = value_min - 15), 
            label = "Worst-case savings\n(cheap N, high corn revenue)", check_overlap = T, 
            hjust = 0,
            fontface = "italic", color = "gray50") +
  geom_text(aes(x = 1.3, y = avg_savings + 2), 
            label = "Average savings", check_overlap = T, 
            hjust = 0,
            fontface = "italic", color = "gray50") +
  scale_y_continuous(labels = label_dollar(),
                     expand = expansion(add = 20)) +
  expand_limits(x = 3)+
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Dollars\nper acre",
       title = str_wrap("Three price scenarios* when reducing nitrogen application to corn",
                        width = 70),
       subtitle = "Best, average, and worst savings potential examples",
       caption = "*Nitrogen prices ranged from $0.60-$1.20/lb N\n Corn revenue ranged from $5.70-$7.48/bu")

ggsave("figs/monetary-example.png", width = 6, height = 5)






#-------------old



# data --------------------------------------------------------------------

d_stats <- 
  read_csv("data_tidy/stats-savings.csv")

d_money <- 
  read_csv("data_tidy/money.csv") %>% 
  #group_by(last_name) %>% 
  #summarise(across(contains("savings"), ~mean(.x, na.rm = T))) %>% 
  mutate(clr = case_when(
    (most_savings < 0 & least_savings < 0) ~ "bad",
    (most_savings > 0 & least_savings < 0) ~ "neutral",
    (most_savings > 0 & least_savings > 0) ~ "good",
  )) %>% 
  left_join(d_stats %>% select(last_name, pval)) %>% 
  mutate(sig = ifelse(pval < 0.05, "*", "NS"))
  
d_money_raw <- 
  read_csv("data_tidy/money.csv") %>%
  pivot_longer(3:ncol(.)) %>% 
  left_join(d_stats %>% select(-estimate) %>% rename(name = var))

#--look at money by rep
d_money_raw  %>%
  filter(!is.na(value)) %>% 
  mutate(sig = ifelse(pval < 0.05, "*", "NS")) %>% 
  ggplot(aes(last_name, value, shape = name, color = sig)) + 
  geom_point(alpha = 1) + 
  #stat_summary() +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("black", "gray"))

ggsave("figs/scratch-money-stats.png")

# full figure -------------------------------------------------------------

#--maybe I use the raw reps for the ranges?

d_money |> 
  ggplot() + 
  geom_hline(yintercept = 0) +
  geom_segment(aes(
    x = reorder(last_name, most_savings),
    xend = reorder(last_name, most_savings),
    y = least_savings,
    yend = most_savings, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  geom_point(aes(
    x = reorder(last_name, most_savings),
    y = avg_savings),
    color = "white",
    pch = 17,
    size = 1) +
  geom_text(aes(x = 12, y = 300), label = "Financial advantage at reduced nitrogen rate", check_overlap = T, 
            #hjust = 0,
            fontface = "italic", color = pfi_blue) +
  geom_text(aes(x = 8, y = -300), label = "Financial loss at reduced nitrogen rate", check_overlap = T, 
            #hjust = 0,
            fontface = "italic", color = pfi_orange) +
  scale_y_continuous(labels = label_dollar(), limits = c(-400, 400),
                     breaks = c(-400, -300, -200, -100, 0, 100, 200, 300, 400)) + 
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Dollars\nper acre",
       title = str_wrap("Reduced nitrogen (N) rates were more profitable in some trials",
                          width = 80),
       subtitle = "Seven trials were more profitable per acre at the reduced N rate in an average price scenario")

ggsave("figs/monetary-diffs.png", width = 7, height = 5)


# mock figure -------------------------------------------------------------

#--show ranges of high and low
d_money |> 
  filter(last_name == "Waldo") |> 
  mutate(last_name = "Example") %>% 
  ggplot() + 
  geom_hline(yintercept = 0) +
  geom_segment(aes(
    x = reorder(last_name, -most_savings),
    xend = reorder(last_name, -most_savings),
    y = least_savings,
    yend = most_savings, 
    color = clr),
    linewidth = 5,
    show.legend = F
  )  + 
  geom_point(aes(
    x = reorder(last_name, -most_savings),
    y = avg_savings),
    color = "white",
    pch = 17,
    size = 2) +
  geom_segment(aes(xend = 1, x = 1.3,
                   yend = most_savings + 1, y = most_savings + 12),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(xend = 1, x = 1.2,
                   yend = least_savings - 1, y = least_savings - 15),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(xend = 1.05, x = 1.3,
                   yend = avg_savings, y = avg_savings + 2),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(aes(x = 1.3, y = most_savings + 12), 
            label = "Best-case savings\n(expensive N, low corn revenue)", check_overlap = T, 
             hjust = 0,
             fontface = "italic", color = "gray50") +
  geom_text(aes(x = 1.2, y = least_savings - 15), 
            label = "Worst-case savings\n(cheap N, high corn revenue)", check_overlap = T, 
            hjust = 0,
            fontface = "italic", color = "gray50") +
  geom_text(aes(x = 1.3, y = avg_savings + 2), 
            label = "Average savings", check_overlap = T, 
            hjust = 0,
            fontface = "italic", color = "gray50") +
  scale_y_continuous(labels = label_dollar(),
                     limits = c(-10, 110)) +
  expand_limits(x = 3)+
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Dollars\nper acre",
       title = str_wrap("Three price scenarios* when reducing nitrogen application to corn",
                        width = 70),
       subtitle = "Best, average, and worst savings potential examples",
       caption = "*Nitrogen prices ranged from $0.60-$1.20/lb N\n Corn revenue ranged from $5.70-$7.48/bu")

ggsave("figs/monetary-example.png", width = 6, height = 5)




