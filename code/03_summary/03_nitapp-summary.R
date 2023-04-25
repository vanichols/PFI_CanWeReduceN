library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/00_fig-things.R")

# theme -------------------------------------------------------------------

my_nitapp_theme <- 
  theme_bw() +
  theme(
    legend.position = c(0.2, 0.9),
    legend.text = element_text(size = rel(1.2)),
    legend.background = element_blank(),
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
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3)
  ) 


# data --------------------------------------------------------------------

#--yields
y <- read_csv("data_tidy/yields.csv") |>
  filter(!is.na(yield_buac))


# N application rates -----------------------------------------------------

#--how much did they reduce it?
n_pct <-
  y |> 
  select(-yield_buac, -rep)  |> 
  distinct() |> 
  pivot_wider(names_from = trt, values_from = nrate_lbac) |> 
  mutate(red_pct = (typ - red)/typ * 100)

summary(n_pct$red_pct)



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
  geom_text(aes(x = last_name, y = typ + 20,
                label = paste0(round(red_pct, 0), "%")),
            fontface = "italic") +
  scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) + 
  scale_y_continuous(breaks = seq(0, 300, 50),
                     limits = c(0, 300)) +
  labs(x = "Cooperator",
       y = "Nitrogen\napplied\n(lb/ac)",
       fill = NULL,
       title = "Typical N rate treatments ranged from 108-264 lb N/ac",
       subtitle = "Rates were reduced by 20-74 lb N/ac") + 
  my_nitapp_theme 

ggsave("figs/nrates.png", width = 8, height = 5.5)


