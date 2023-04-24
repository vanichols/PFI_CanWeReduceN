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


# data --------------------------------------------------------------------

d_money <- read_csv("data_tidy/money.csv")


# full figure -------------------------------------------------------------


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
  theme(axis.text.x = element_blank()) +
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Dollars\nper acre",
       title = str_wrap("Three price scenarios* when reducing nitrogen application to corn by 50 units",
                        width = 50),
       subtitle = "Best, average, and worst savings potential examples",
       caption = "*Nitrogen prices ranged from $0.60-$1.20/lb N\n Corn revenue ranged from $5.70-$7.48/bu")

ggsave("figs/monetary-example.png", width = 5, height = 5)




