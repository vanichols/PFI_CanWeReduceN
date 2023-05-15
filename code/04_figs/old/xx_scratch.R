library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/00_fig-things.R")

# data --------------------------------------------------------------------

#--N high price = $1.20/lb
#--N low price = $0.60/lb
n_hi <- 1.2
n_lo <- 0.6
n_av <- mean(c(n_hi, n_lo))

#--corn price = $7/bu
crn_hi <- 5.70
crn_lo <- 4.48
crn_av <- mean(c(crn_hi, crn_lo))


#--yields
y <- read_csv("data_tidy/yields.csv") |>
  filter(!is.na(yield_buac))

#--stats on yields
ys <- 
  read_csv("data_tidy/stats.csv") %>% 
  select(trt, estimate, last_name, diff_est, pval) |> 
  rename(y_buac = estimate,
         ydif_buac = diff_est) %>% 
  mutate(y_pval = ifelse(pval < 0.05, "*", "NS"))

#--get rid of reps
d <- 
  y |> 
  left_join(ys) |> 
  select(-rep, -yield_buac) |> 
  distinct() %>% 
  left_join(
    
    #--nrate diff
    y %>% 
      select(last_name, trt, nrate_lbac) %>% 
      distinct() %>% 
      pivot_wider(names_from = trt, values_from = nrate_lbac) %>% 
      group_by(last_name) %>% 
      summarise(ndif_lbac = typ- red)
    
  )

ddif <- 
  d %>% 
  select(last_name, ydif_buac, y_pval, ndif_lbac) %>% 
  distinct()

# bushels lost per n ------------------------------------------------------

ddif %>% 
  mutate(bulost_per_nred = ydif_buac/ndif_lbac) %>% 
  ggplot(aes(reorder(last_name, bulost_per_nred), bulost_per_nred)) + 
  geom_point() + 
  coord_flip()

#--keep reps
ddif_rep <- 
  read_csv("data_tidy/trt-diffs.csv") %>% 
  mutate(bulost_per_nred = yield_buac / nrate_lbac)

ddif_rep %>% 
  ggplot(aes(reorder(last_name, bulost_per_nred), bulost_per_nred)) + 
  geom_point() + 
  coord_flip()

#--add money? I really like this. What are they sacrificing. 
ddif_rep %>% 
  mutate(mnet = (nrate_lbac * n_lo) - (yield_buac * crn_hi),
         mchange_per_nred = mnet/nrate_lbac) %>% 
  ggplot(aes(reorder(last_name, mchange_per_nred), mchange_per_nred)) + 
  geom_point(aes(size = nrate_lbac)) + 
  coord_flip()


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
         avg_savings_lab = ifelse(clr == "neutral", " ", avg_savings_lab))

d_new <- 
  d_raw |> 
  mutate(yld_sig = ifelse(yld_pval < 0.05, "*", " ")) %>% 
  select(last_name, yld_dif_buac, yld_sig) |> 
  distinct() |> 
  left_join(d_money_fig)


# yield diffs, money-------------------------------------------------------------

#--make them colored by financial losses

d_new |>
  #--make it so it is red-typ
  mutate(yld_dif_buac = -yld_dif_buac) %>% 
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
       y = "Change in corn yield (bu/ac) with reduced N application",
       title = "Significant yield reductions are not indicative of financial outcomes",
       subtitle = "Financial outcome assuming midpoint price scenario as reference",
       caption = "* = Significant change in yield at 95% confidence level")

ggsave("figs/yield-diff.png", width = 7, height = 5)

