library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/04_figs/00_fig-colors.R")

# theme -------------------------------------------------------------------

my_yield_theme <- 
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,
                                 vjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.3)),
    panel.grid.minor = element_blank(),
   # panel.grid.major.y = element_blank(),
    plot.caption = element_text(hjust = 1),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
   text = element_text(family = "Times New Roman"),
   plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3)
  ) 


# data --------------------------------------------------------------------

#--yields
y <- read_csv("data_tidy/yields.csv") |>
  filter(!is.na(yield_buac))

#--nrate and yield diffs by rep
d_diffs <- read_csv("data_tidy/trt-diffs.csv")

#--stats
s_raw <- read_csv("data_tidy/stats.csv")

s_buac <- 
  s_raw |> 
  select(trt, estimate, last_name, diff_est, pval) |> 
  rename(yld_pval = pval,
         yld_mn = estimate,
         yld_dif_buac = diff_est)

s_pct <- 
  s_buac |> 
  filter(trt == "typ") %>% 
  mutate(yld_dif_pct = yld_dif_buac/yld_mn * 100) %>% 
  select(-trt, -yld_mn)
  
s <- 
  s_buac %>% 
  left_join(s_pct)

d_raw <- 
  y |> 
  left_join(s) |> 
  select(-rep, -yield_buac) |> 
  distinct()


#--money data
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

d_money_fig <-
  d_money %>%
  select(last_name, value_min, value_max, fin_sig, clr) %>% 
  left_join(
    d_stats %>% 
      filter(var == "avg_savings") %>% 
      select(last_name, estimate) %>% 
      rename(avg_savings = estimate)
  ) %>% 
  distinct() %>% 
  select(last_name, clr, avg_savings) %>% 
  mutate(avg_savings_lab = ifelse(avg_savings < 0, 
                                  paste0("-$", abs(round(avg_savings))),
                                  paste0("$", round(avg_savings))),
         avg_savings_lab = ifelse(clr == "neutral", " ", paste0(avg_savings_lab, "/ac")))

d_new <- 
  d_raw |> 
  mutate(yld_sig = ifelse(yld_pval < 0.05, "*", " ")) %>% 
  select(last_name, yld_dif_buac, yld_sig) |> 
  distinct() |> 
  left_join(d_money_fig)


# yield diffs, money-------------------------------------------------------------

d_new %>% 
  filter(last_name == "Bennett")

#--make them colored by financial losses

d_new |>
  left_join(d_diffs %>% select(last_name, dif_nrate_lbac) %>% distinct()) %>% 
  #--make it so it is red-typ
  mutate(yld_dif_buac = -yld_dif_buac, 
         last_name = case_when(
           last_name == "Veenstra_1" ~ "Veenstra1",
           last_name == "Veenstra_2" ~ "Veenstra2",
           TRUE ~ last_name),
         last_name = paste0(last_name, ", -", round(dif_nrate_lbac), " lb/ac")) %>% 
  ggplot(aes(reorder(last_name, -yld_dif_buac), yld_dif_buac)) +
  geom_col(aes(fill = clr),
           color = "black",
           show.legend = F) +
  geom_text(aes(reorder(last_name, -yld_dif_buac), 
                yld_dif_buac - 2, 
                label = yld_sig,
                hjust = 0,
                vjust = 0.75)) +
  #--savings values
  geom_text(aes(reorder(last_name, -yld_dif_buac), 
                y = 28, 
                hjust = 1,
                label = avg_savings_lab,
                color = clr),
            angle = 0,
            show.legend = F,
            fontface = "bold") +
  coord_flip() +
  my_yield_theme +
  scale_y_continuous(limits = c(-60, 40),
                     breaks = c(-60, -40, -20, 0,
                                20, 40)) +
  scale_fill_manual(values = c("neutral" = pfi_tan, 
                               "good" = pfi_blue,
                               "bad" = pfi_orange)) +
  scale_color_manual(values = c("neutral" = pfi_tan, 
                                "good" = pfi_blue,
                                "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Impact of reduced N rate on corn yield (bu/ac)\nrelative to typical N treatment",
       title = "Significant yield reductions are not indicative of financial outcomes",
       subtitle = "Financial outcome from reduced N rate assuming midpoint price scenario as reference on right",
       caption = "* = Significant change in yield at 95% confidence level")

ggsave("figs/yield-diff.png", width = 7, height = 5)

