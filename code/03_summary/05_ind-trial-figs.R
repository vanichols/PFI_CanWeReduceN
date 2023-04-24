library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)


rm(list = ls())

source("code/00_fig-things.R")
theme_set(my_combo_theme)

# data --------------------------------------------------------------------

d <- read_csv("data_tidy/yields.csv")
s <- read_csv("data_tidy/stats.csv")

m <- read_csv("data_tidy/money.csv") %>% 
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
  ) 


#--sifnificances are the same
# s |> 
#   mutate(sig_fixed = ifelse(pval < 0.05, "y", "n")) |> 
#   select(-pval, -lsd) |> 
#   left_join(
#     m |> 
#       mutate(sig_rand = ifelse(pval < 0.05, "y", "n")) |> 
#       select(-pval, -lds)
#   ) |> 
#   mutate(dif = ifelse(sig_fixed == sig_rand, "n", "y"))

my_names <- 
  d |> 
  pull(last_name) |> 
  unique()


# yields/stats fig --------------------------------------------------------

#--need estimated reduction
y <-
  d |>  
  left_join(s) |> 
  #--get rid of bakehouse's NA rep
  filter(!is.na(yield_buac)) |> 
  mutate(trt = ifelse(trt == "typ", 
                      paste0("Typical,\n", round(nrate_lbac, 0), " lb N/ac"),
                      paste0("Reduced,\n", round(nrate_lbac, 0), " lb N/ac")),
         trt = as.factor(trt),
         trt = fct_rev(trt),
         sig = ifelse(pval < 0.05, 
                      "Significant", "Insignificant"),
         dir = ifelse(diff_est < 0, "increase", "reduction"),
         sig_lab = paste(sig, dir, "of", abs(round(diff_est, 0)), "bu/ac*"),
         yld_lab = paste(round(estimate, 0), "bu/ac")) |> 
  group_by(last_name) |> 
  mutate(yield_max = max(yield_buac, na.rm = T)) 


#--test

YieldFig <- function(data = tst) {
  
  fig <- 
    tst |>
    ggplot(aes(trt, yield_buac)) +
    geom_col(aes(group = rep, fill = trt),
             color = "black",
             position = position_dodge(),
             show.legend = F) +
    #--yields
    geom_text(aes(x = trt,
                  y = 1.15 * yield_max,
                  label = yld_lab),
              check_overlap = T,
              fontface = "italic") +
    #--reps, typ
    geom_text(x = 0.58, y = 2, label = "Rep 1", angle = 90, color = "gray", hjust = 0) +
    geom_text(x = 0.81, y = 2, label = "Rep 2", angle = 90, color = "gray", hjust = 0) +
    geom_text(x = 1.04, y = 2, label = "Rep 3", angle = 90, color = "gray", hjust = 0) +
    geom_text(x = 1.27, y = 2, label = "Rep 4", angle = 90, color = "gray", hjust = 0) +
    #--reps, red
    geom_text(x = 1.58, y = 2, label = "Rep 1", angle = 90, color = "gray", hjust = 0) +
    geom_text(x = 1.81, y = 2, label = "Rep 2", angle = 90, color = "gray", hjust = 0) +
    geom_text(x = 2.04, y = 2, label = "Rep 3", angle = 90, color = "gray", hjust = 0) +
    geom_text(x = 2.27, y = 2, label = "Rep 4", angle = 90, color = "gray", hjust = 0) +
    #--diff
    geom_text(aes(x = 1.5,
                  y = 1.4 * yield_max,
                  label = sig_lab),
              check_overlap = T,
              fontface = "bold") +
    scale_y_continuous(expand = expansion(0.1)) +
    scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) +
    labs(x = NULL,
         y = "bu per ac",
         title = "Corn yield response",
        caption = "*Significance at 95% confidence level\nNumbers may not match exactly due to rounding")
  
  #--testing new rep label
  
  # sub_lab <- 
  #   paste(tst %>% 
  #   pull(sig_lab) %>% 
  #   unique(), "with reduced N rate")
  # 
  # fig <- 
  #   tst |> 
  #   mutate(rep_lab = paste("Rep ", rep)) %>% 
  #   ggplot(aes(rep_lab, yield_buac)) + 
  #   geom_col(aes(group = rep, fill = trt),
  #            color = "black",
  #            width = 1,
  #            show.legend = F) + 
  #   #--yields
  #   geom_text(aes(x = 2.5, 
  #                 y = 1.15 * yield_max,
  #                 label = yld_lab),
  #             check_overlap = T,
  #             fontface = "italic") + 
  #   scale_y_continuous(expand = expansion(0.1)) +
  #   scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) +
  #   facet_grid(.~trt) +
  #   labs(x = NULL,
  #        y = "bu per ac",
  #        title = "Corn yield response",
  #        subtitle = sub_lab,
  #        caption = "*Significance at 95% confidence level\nNumbers may not match exactly due to rounding")
  # 
  return(fig)
  
}

tst <- y |> 
  filter(last_name == my_names[7])

fig1 <- YieldFig(data = tst)

fig1

# nue fig -----------------------------------------------------------------

nue <- read_csv("data_tidy/stats-nue.csv")

# calc NUE ----------------------------------------------------------------

y2 <- 
  d |> 
  #--get rid of bakehouse's NA rep
  filter(!is.na(yield_buac)) |> 
  left_join(nue |> 
              select(trt, estimate, last_name) 
  ) |> 
  mutate(
    nue = nrate_lbac/yield_buac,
    trt = ifelse(trt == "typ", 
                 paste0("Typical,\n", round(nrate_lbac, 0), " lb N/ac"),
                 paste0("Reduced,\n", round(nrate_lbac, 0), " lb N/ac")),
    trt = as.factor(trt),
    trt = fct_rev(trt),
    estimate = round(estimate, 2)) |> 
  group_by(last_name, trt) |> 
  mutate(nue_max = max(nue, na.rm = T))

tst2 <- 
  y2 |> filter(last_name == my_names[7])

NUEFig <- function(data = tst2) {
  
  fig <- 
    tst2 |> 
    ggplot(aes(trt, nue)) + 
    geom_jitter(aes(fill = trt),
               color = "black",
               show.legend = F,
               pch = 21,
               size = 4, 
               width = 0.1) + 
    geom_text(
      aes(trt, nue_max * 1.05, label = paste0("Mean = ", estimate)),
      check_overlap = T
    ) +
    scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) +
    labs(x = NULL,
         y = "lb N per bushel",
         title = str_wrap("Nitrogen applied per unit corn yield", width = 20))
  
  return(fig)
  
}

fig2 <- NUEFig(data = tst2)

fig2



# money fig ---------------------------------------------------------------

tst3 <- 
  m |> 
  filter(last_name == my_names[7])


MoneyFig <- function(data = tst3) {
  
  tst3 %>%
    ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_segment(
      aes(
        x = last_name,
        xend = last_name,
        y = least_savings,
        yend = most_savings,
        color = clr
      ),
      linewidth = 8,
      show.legend = F
    )  +
    geom_point(
      aes(x = last_name,
          y = avg_savings),
      color = "white",
      pch = 17,
      size = 2
    ) +
    #--arrows
    geom_segment(aes(
      xend = 1,
      x = 1.3,
      yend = most_savings + 1,
      y = most_savings + 12
    ),
    arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(aes(
      xend = 1,
      x = 1.2,
      yend = least_savings - 1,
      y = least_savings - 15
    ),
    arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(aes(
      xend = 1.05,
      x = 1.35,
      yend = avg_savings,
      y = avg_savings + 2
    ),
    arrow = arrow(length = unit(0.2, "cm"))) +
    #--text
    geom_text(
      aes(
        x = 1.35,
        y = most_savings + 12,
        label = paste0(
          "Best-case, ",
          most_savings_lab,
          "\n  (expensive N,\n  low corn revenue)"
        )
      ),
      check_overlap = T,
      hjust = 0,
      fontface = "italic",
      color = "gray50"
    ) +
    geom_text(
      aes(
        x = 1.25,
        y = least_savings - 15,
        label = paste0(
          "Worst-case, ",
          least_savings_lab,
          "\n  (cheap N,\n  high corn revenue)"
        )
      ),
      check_overlap = T,
      hjust = 0,
      fontface = "italic",
      color = "gray50"
    ) +
    geom_text(
      aes(
        x = 1.4,
        y = avg_savings + 2,
        label = paste0("Average, ", avg_savings_lab)
      ),
      check_overlap = T,
      hjust = 0,
      fontface = "italic",
      color = "gray50"
    ) +
    scale_y_continuous(labels = label_dollar(), expand = expansion(add = 10)) +
    expand_limits(x = 4.5) +
    scale_color_manual(values = c(
      "good" = pfi_blue,
      "neutral" = pfi_tan,
      "bad" = pfi_orange
    )) +
    labs(
      x = NULL,
      y = "dollar per acre",
      title = str_wrap(
        "Financial outcome**",
        width = 40
      ),
      caption = "**N prices ranged from $0.60-$1.20/lb N\n Corn revenue ranged from $5.70-$7.48/bu"
    )
  
  
  
}

fig3 <- MoneyFig(data = tst3)

fig3

# combine -----------------------------------------------------------------


fig1 + fig3 + fig2 + plot_layout(widths = c(0.7, 0.3, 0.3))

ggsave("figs/ind-figs/test.png", width = 10, height = 6)


# loop it -----------------------------------------------------------------

#--waldo needs done manually bc of reps

for (i in 1:length(my_names)){
  

  tst <- 
    y |> 
    filter(last_name == my_names[i]) 
  
  fig1 <- YieldFig(data = tst)
  
  tst2 <- 
    y2 |> 
    filter(last_name == my_names[i])
  
  fig2 <- NUEFig(data = tst2)
  
  tst3 <- 
    m %>% 
    filter(last_name == my_names[i])
  
  fig3 <- MoneyFig(data = tst3)
  
  
  fig1 + fig3 + fig2 + plot_layout(widths = c(0.7, 0.3, 0.3))
  ggsave(paste0("figs/ind-figs/", my_names[i], ".png"), 
         height = 7.19, width = 11.5)
  
  
}
