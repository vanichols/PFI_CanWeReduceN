library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/00_fig-things.R")

# data --------------------------------------------------------------------

y <- read_csv("data_tidy/yields.csv") |>
  filter(!is.na(yield_buac))

#--make up dollar amounts
#--N high price = $1.20/lb
#--N low price = $0.60/lb
n_hi <- 1.2
n_lo <- 0.6
n_av <- mean(c(n_hi, n_lo))

#--corn price = $7/bu
crn_hi <- 8
crn_lo <- 5.5
crn_av <- mean(c(crn_hi, crn_lo))

m <- 
  y |> 
  group_by(last_name, trt) |> 
  summarise(nrate_lbac = mean(nrate_lbac, na.rm = T),
            yield_buac = mean(yield_buac, na.rm = T)) |> 
  mutate(ncost_hi = n_hi * nrate_lbac,
         ncost_lo = n_lo * nrate_lbac,
         ncost_av = n_av * nrate_lbac,
         
         crev_hi = crn_hi * yield_buac,
         crev_lo = crn_lo * yield_buac,
         crev_av = crn_av * yield_buac) 


m |> 
  mutate(most_savings = crev_lo - ncost_hi,
         least_savings = crev_hi - ncost_lo,
         avg_savings = crev_av - ncost_av) |> 
  select(last_name, trt, contains("savings"))|> 
  pivot_longer(most_savings:avg_savings) |> 
  pivot_wider(names_from = trt, values_from = value) |> 
  mutate(savings = red - typ,
         assump = case_when(
           name == "most_savings" ~ "Expensive N, low priced corn",
           name == "least_savings" ~ "Cheap N, high priced corn",
           name == "avg_savings" ~ "Avg N, avg priced corn",
         )) |> 
  ggplot() + 
  geom_point(aes(reorder(last_name, typ), typ), color = pfi_blue) + 
  geom_point(aes(last_name, red), color = pfi_green) + 
  facet_grid(.~ assump)



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





# money -------------------------------------------------------------


#--show ranges of high and low

d_money <- 
  m |> 
  mutate(most_savings = crev_lo - ncost_hi,
         least_savings = crev_hi - ncost_lo,
         avg_savings = crev_av - ncost_av) |> 
  select(last_name, trt, contains("savings")) %>%
  pivot_longer(most_savings:ncol(.)) |> 
  pivot_wider(names_from = trt, values_from = value) |> 
  mutate(savings = red - typ) |> 
  select(last_name, name, savings) |> 
  pivot_wider(names_from = name, values_from = savings) |> 
  mutate(clr = case_when(
    (most_savings < 0 & least_savings < 0) ~ "bad",
    (most_savings > 0 & least_savings < 0) ~ "neutral",
    (most_savings > 0 & least_savings > 0) ~ "good",
  )) 

d_money |> 
  write_csv("data_tidy/money.csv")


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
m |> 
  mutate(most_savings = crev_lo - ncost_hi,
         least_savings = crev_hi - ncost_lo,
         avg_savings = crev_av - ncost_av) |> 
  select(last_name, trt, contains("savings")) %>%
  pivot_longer(most_savings:ncol(.)) |> 
  pivot_wider(names_from = trt, values_from = value) |> 
  mutate(savings = red - typ) |> 
  select(last_name, name, savings) |> 
  pivot_wider(names_from = name, values_from = savings) |> 
  mutate(clr = case_when(
    (most_savings < 0 & least_savings < 0) ~ "bad",
    (most_savings > 0 & least_savings < 0) ~ "neutral",
    (most_savings > 0 & least_savings > 0) ~ "good",
  )) |>
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
       title = str_wrap("Three price scenarios*",
                        width = 80),
       subtitle = "Best, average, and worst savings potential",
       caption = "*Nitrogen prices ranged from $0.60-$1.20/lb N\n Corn revenue ranged from $5.50-$8.00/bu")

ggsave("figs/monetary-example.png", width = 5, height = 5)




