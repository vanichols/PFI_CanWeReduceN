library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX replaced

rm(list = ls())

source("code/00_fig-things.R")

# theme -------------------------------------------------------------------

my_money_theme <- 
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,
                                vjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.3)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot"
    
  ) 


theme_set(my_money_theme)


# data -------------------------------------------------------------------

d_money <- read_csv("data_tidy/money.csv")

nue <- read_csv("data_tidy/stats-nue.csv") |> 
  select(trt, estimate, last_name, diff_est, pval) |> 
  rename(nue_pval = pval,
         nue_mn = estimate,
         nue_dif = diff_est)

the_names <- 
  d_money %>% 
  pull(last_name) %>% 
  unique()


i <- 1


for (i in 1:length(the_names)) {
  
  
  # money -------------------------------------------------------------------
  
  
  #--show ranges of high and low
  fig_money <- 
    d_money |> 
    mutate(
      most_savings_lab = ifelse(most_savings < 0, 
                                paste0("-$", abs(round(most_savings, 0))), 
                                paste0("+$", abs(round(most_savings, 0)))),
      least_savings_lab = ifelse(least_savings < 0, 
                                 paste0("-$", abs(round(least_savings, 0))), 
                                 paste0("+$", abs(round(least_savings, 0)))),
      avg_savings_lab = ifelse(avg_savings < 0, 
                               paste0("-$", abs(round(avg_savings, 0))), 
                               paste0("+$", abs(round(avg_savings, 0))))
    ) %>% 
    filter(last_name == the_names[i]) |> 
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
    #--arrows
    geom_segment(aes(xend = 1, x = 1.3,
                     yend = most_savings + 1, y = most_savings + 12),
                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(aes(xend = 1, x = 1.2,
                     yend = least_savings - 1, y = least_savings - 15),
                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(aes(xend = 1.05, x = 1.3,
                     yend = avg_savings, y = avg_savings + 2),
                 arrow = arrow(length = unit(0.2, "cm"))) +
    #--text
    geom_text(aes(
      x = 1.3, 
      y = most_savings + 12, 
      label = paste0("Best-case, ", most_savings_lab, "\n(expensive N, low corn revenue)")),
      check_overlap = T, 
      hjust = 0,
      fontface = "italic", color = "gray50") +
    geom_text(aes(
      x = 1.2, y = least_savings - 15, 
      label = paste0("Worst-case, ", least_savings_lab, "\n(cheap N, high corn revenue)")),
      check_overlap = T, 
      hjust = 0,
      fontface = "italic", color = "gray50") +
    geom_text(aes(x = 1.3, y = avg_savings + 2, 
                  label = paste0("Average, ", avg_savings_lab)),
              check_overlap = T, 
              hjust = 0,
              fontface = "italic", color = "gray50") +
    scale_y_continuous(labels = label_dollar(), expand = expansion(add = 10)) +
    expand_limits(x = 4.5)+
    scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
    labs(x = NULL,
         y = "Dollars\nper acre",
         title = str_wrap("Three price scenarios* when reducing nitrogen application to corn by 50 units",
                          width = 40),
         caption = "*Nitrogen prices ranged from $0.60-$1.20/lb N\n Corn revenue ranged from $5.70-$7.48/bu")
  
  
  
  
  # nue change --------------------------------------------------------------
  
  
  #--arrows from old nue to new
  nue_dat <- 
    nue |> 
    pivot_wider(names_from = trt, values_from = nue_mn) |> 
    mutate(nue_sig = ifelse(nue_pval < 0.05, "*", "NS")) %>% 
    filter(last_name == the_names[i])
  
  nue_dat_arrows <- 
    nue_dat |> 
    filter(nue_pval < 0.05) %>% 
    filter(last_name == the_names[i])
  
  
  fig_nue <- 
    ggplot() +
    geom_point(
      data = nue_dat,
      aes(last_name, typ),
      fill = pfi_dkgreen,
      show.legend = F,
      color = "black",
      pch = 21,
      size = 5
    ) +
    geom_point(
      data = nue_dat,
      aes(last_name, red),
      fill = pfi_yellow,
      show.legend = F,
      color = "black",
      pch = 21,
      size = 5
    ) +
    geom_arrowsegment(
      data = nue_dat_arrows,
      aes(
        x = last_name,
        xend = last_name,
        y = typ,
        yend = red + 0.01
      ),
      arrows = arrow(type = "closed", length = unit(0.3, "cm"))
    ) +
    geom_text(data = nue_dat,
              aes(last_name,  typ + 0.15, label = nue_sig),
              size = 5) +
    geom_text(data = nue_dat,
              aes(1.3,  typ + 0.15, label = paste0(round(typ, 2), " lb N/bu")),
              size = 5, 
              fontface = "italic") +
    geom_text(data = nue_dat,
              aes(1.3,  red - 0.15, label = paste0(round(red, 2), " lb N/bu")),
              size = 5, 
              fontface = "italic") +
    scale_y_continuous(limits = c(0, 2.5)) +
    expand_limits(y = 2.5) +
    scale_fill_manual(values = c(pfi_green, pfi_tan)) +
    labs(
      x = NULL,
      y = "lb N/\nbu corn",
      title = str_wrap("Nitrogen (N) applied per bushel of corn at the reduced N rate", width = 40),
      caption = "* = Statistically significant change at a 95% confidence level\nNS = No significant change"
    )
  
  
  fig_money + fig_nue
  
  ggsave(paste0("figs/money-nue/", the_names[i], ".png"), 
         height = 4.5, width = 8)
  
}
