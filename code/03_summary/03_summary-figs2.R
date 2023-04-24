library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/00_fig-things.R")
# theme -------------------------------------------------------------------

my_yield_theme <- 
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

my_yield_theme2 <- 
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
    plot.caption.position =  "plot"
  ) 


theme_set(my_yield_theme)



# data --------------------------------------------------------------------

#--yields
y <- read_csv("data_tidy/yields.csv") |>
  filter(!is.na(yield_buac))

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

#--nue
nue <- read_csv("data_tidy/stats-nue.csv") |> 
  select(trt, estimate, last_name, diff_est, pval) |> 
  rename(nue_pval = pval,
         nue_mn = estimate,
         nue_dif = diff_est)


d_stats <- 
  read_csv("data_tidy/stats-savings.csv")

d_money <- 
  read_csv("data_tidy/money.csv") %>% 
  group_by(last_name) %>% 
  summarise(across(contains("savings"), ~mean(.x, na.rm = T))) %>% 
  mutate(clr = case_when(
    (most_savings < 0 & least_savings < 0) ~ "bad",
    (most_savings > 0 & least_savings < 0) ~ "neutral",
    (most_savings > 0 & least_savings > 0) ~ "good",
  )) %>% 
  left_join(d_stats %>% select(last_name, pval)) %>% 
  mutate(fin_sig = ifelse(pval < 0.05, "**", " ")) %>% 
  mutate(fin_outcome = ifelse(avg_savings > 0, "savings", "loss"),
         avg_savings = 
           ifelse(fin_outcome == "loss", 
                  paste0("-$", abs(round(avg_savings, 0)), "/ac"),
                  paste0(" $", abs(round(avg_savings, 0)), "/ac"))) |> 
  select(last_name, avg_savings, fin_outcome, fin_sig)



d_raw <- 
  y |> 
  left_join(s) |> 
  left_join(nue) |> 
  select(-rep, -yield_buac) |> 
  distinct()

d <- 
  d_raw |> 
  select(last_name, yld_dif_buac, yld_dif_pct, yld_pval, nue_dif, nue_pval) |> 
  distinct() |> 
  mutate(yld_sig = ifelse(yld_pval < 0.05, "*", " "),
         nue_sig = ifelse(nue_pval < 0.05, "*", " ")) %>% 
  left_join(d_money)

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

y %>% 
  group_by(trt) %>% 
  summarise(maxn = max(nrate_lbac, na.rm = T),
            minn = min(nrate_lbac, na.rm = T))

d %>% 
  filter(yld_sig == " ") %>% 
  arrange(yld_dif_buac)

#--range in LSDs?
s_raw |> 
  pull(lsd) |> 
  summary(.)
  
  
#--range in typical rates?
y |> 
  filter(trt == "typ") |> 
  select(nrate_lbac) |> 
  summary(.)


# N application rates -----------------------------------------------------

#--get order I want them in
my_order <- 
  y %>% 
  select(last_name, trt, nrate_lbac) %>% 
  distinct() %>% 
  filter(trt == "typ") %>% 
  arrange(trt, nrate_lbac) %>% 
  pull(last_name)


y %>% 
  select(last_name, trt, nrate_lbac) %>% 
  distinct() %>% 
  pivot_wider(names_from = trt, values_from = nrate_lbac) %>% 
  mutate(xdif = typ - red) %>% 
  arrange(xdif) %>% 
  select(-typ) %>% 
  pivot_longer(red:xdif) %>% 
  rename(trt = name, nrate_lbac = value) %>% 
  mutate(trt = ifelse(trt == "xdif", "Typical N rate", "Reduced N rate")) %>% 
  left_join(n_pct %>% select(-red)) %>% 
  mutate(trt_fct = factor(trt, levels = c("Typical N rate", "Reduced N rate")),
         last_name = factor(last_name, levels = my_order)) %>% 
  #--fig
  ggplot(aes(last_name, nrate_lbac)) +
  geom_col(aes(fill = trt_fct), color = "black") +
  geom_text(aes(x = last_name, y = typ + 10,
                label = paste0(round(red_pct, 0), "%")),
            fontface = "italic") +
  scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) + 
  scale_y_continuous(breaks = seq(0, 300, 50),
                     limits = c(0, 300)) +
  labs(x = "Cooperator",
       y = "Nitrogen applied\n(lb/ac)",
       fill = NULL,
       title = "Typical N rate treatments ranged from 108-264 lb N/ac",
       subtitle = "Rates were reduced by 20-74 lb N/ac") + 
  theme(legend.position = c(0.2, 0.9),
        legend.text = element_text(size = rel(1.2)),
        legend.background = element_blank())

ggsave("figs/nrates.png", width = 8, height = 5)


# yield diffs with money-------------------------------------------------------------

#--make them colored by financial losses

d |>
  #--make it so it is red-typ
  mutate(yld_dif_buac = -yld_dif_buac,
         clr = case_when(
           (fin_sig == "**" & fin_outcome == "loss") ~ "org",
           (fin_sig == "**" & fin_outcome == "savings") ~ "blu",
           TRUE ~ "gra"
         )) |>
  select(clr, everything()) %>% 
  ggplot(aes(reorder(last_name, -yld_dif_buac), yld_dif_buac)) +
  geom_col(aes(fill = clr),
           color = "black",
           show.legend = F) +
  geom_text(aes(reorder(last_name, yld_dif_buac), 
                yld_dif_buac - 2, 
                label = yld_sig,
                hjust = 0,
                vjust = 0.75)) +
  #--savings values
  geom_text(data = . %>% filter(fin_sig == "**"),
            aes(reorder(last_name, yld_dif_buac), 
                y = 28, 
                hjust = 1,
                label = avg_savings,
                color = clr),
            angle = 0,
            show.legend = F,
            fontface = "bold") +
  geom_text(data = . %>% filter(fin_sig == " "),
            aes(reorder(last_name, yld_dif_buac), 
                y = 28, 
                hjust = 1,
                label = avg_savings,
                color = clr),
            angle = 0,
            show.legend = F,
            fontface = "italic") +
  coord_flip() +
  my_yield_theme2 +
  scale_y_continuous(limits = c(-60, 40),
                     breaks = c(-60, -40, -20, 0,
                                20, 40)) +
  scale_fill_manual(values = c("gra" = pfi_tan, 
                               "blu" = pfi_blue,
                               "org" = pfi_orange)) +
  scale_color_manual(values = c("gra" = pfi_tan, 
                                "blu" = pfi_blue,
                                "org" = pfi_orange)) +
  geom_hline(yintercept = -7, linetype = "dashed") +
  labs(x = NULL,
       y = "Change in corn yield (bu/ac)\nwhen reducing nitrogen (N) rate by 50 lb N/ac",
       title = "Yield reductions smaller than 7 bu/ac resulted in financial savings",
       subtitle = "Average financial outcome assuming $0.90/lb N and $6.59/bu",
       caption = "* = Significant change in yield at 95% confidence level")

ggsave("figs/yield-diff.png", width = 7, height = 5)
