library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/05_figs/00_fig-colors.R")

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

tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label)

#--yields-----------------

#--raw yields
y <- 
  read_csv("data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(yield_buac)) %>% 
  left_join(tk)

# #--nrate and yield diffs by rep
d_diffs <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  left_join(tk)

#--raw stats on yields
st_buac <- 
  read_csv("data_stats/stat_cornyield.csv") %>% 
  left_join(tk) 

#--rename to make joins easier
st_yld <- 
   st_buac %>% 
   select(trt, estimate, trial_label, diff_est, pval) |>
   rename(yld_pval = pval,
          yld_mn = estimate,
          yld_dif_buac = diff_est) 
   
#--yields and stats
y_dst <- 
   y |> 
   left_join(st_yld) |> 
   select(-rep, -yield_buac) |> 
   distinct()

#--money data------------------

d_money_raw <- 
  read_csv("data_tidy/td_money.csv") %>% 
  left_join(tk) %>% 
  select(trial_key, trial_label, everything())

#--money data and stats
d_money_fig <- 
  d_money_raw %>%
  rename(value_max = bestsav_dolac,
         value_mid = midsav_dolac,
         value_min = worstsav_dolac) %>%
  mutate(clr = case_when(
    (value_max < 0 & value_min < 0) ~ "bad",
    (value_max > 0 & value_min < 0) ~ "neutral",
    (value_max > 0 & value_min > 0) ~ "good"
  )) %>% 
  select(trial_label, clr, value_mid) %>% 
  #--create nice label
  mutate(value_mid_lab = ifelse(value_mid < 0, 
                                  paste0("-$", abs(round(value_mid)), "/ac"),
                                  paste0("$", round(value_mid), "/ac")))


#--combine money and yield info
d_fig <- 
  y_dst |> 
  mutate(yld_sig = ifelse(yld_pval < 0.05, "*", " ")) %>% 
  select(trial_label, yld_dif_buac, yld_sig) |> 
  distinct() |> 
  left_join(d_money_fig)


# yield diffs, money-------------------------------------------------------------

d_fig %>% 
  filter(trial_label == "Anderson"|trial_label == "Frederick")

#--make them colored by financial losses

d_fig |>
  left_join(d_diffs %>% select(trial_label, dif_nrate_lbac) %>% distinct()) %>% 
  #--make it so it is red-typ and nice labels
  mutate(yld_dif_buac = -yld_dif_buac,
         trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb/ac")) %>% 
  #--fig
  ggplot(aes(reorder(trial_label, -yld_dif_buac), yld_dif_buac)) +
  geom_col(aes(fill = yld_sig),
               #size = yld_sig),
           color = "black",
           show.legend = F) +
  geom_text(aes(reorder(trial_label, -yld_dif_buac), 
                yld_dif_buac - 2, 
                label = yld_sig,
                hjust = 0,
                vjust = 0.75)) +
  #--savings values
  geom_text(aes(reorder(trial_label, -yld_dif_buac), 
                y = 28, 
                hjust = 1,
                label = value_mid_lab,
                color = clr),
            angle = 0,
            show.legend = F,
            fontface = "bold") +
  coord_flip() +
  my_yield_theme +
  scale_size_manual(values = c(0, 2)) +
  scale_y_continuous(limits = c(-60, 40),
                     breaks = c(-60, -40, -20, 0,
                                20, 40)) +
  scale_fill_manual(values = c(pfi_tan, pfi_green)) +
  scale_color_manual(values = c("neutral" = pfi_tan, 
                                "good" = pfi_blue,
                                "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Impact of reduced N rate on corn yield (bu/ac)\nrelative to typical N treatment",
       title = "Significant yield reductions are not indicative of financial outcomes",
       subtitle = "Financial outcome from reduced N rate assuming midpoint price scenario as reference on right",
       caption = "* = Significant change in yield at 95% confidence level")

ggsave("figs/fig05_yields.jpg", width = 7, height = 5)

