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


d_stats <- 
  read_csv("data_tidy/stats-savings.csv")


#--money

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

m <-
  d_money %>%
  select(last_name, value_min, value_max, fin_sig, clr) %>% 
  left_join(
    d_stats %>% 
      filter(var == "avg_savings") %>% 
      select(last_name, estimate) %>% 
      rename(avg_savings = estimate)
  ) %>% 
  distinct() %>% 
  mutate(
    value_max_lab = ifelse(value_max < 0, 
                              paste0("-$", abs(round(value_max, 0))), 
                              paste0("+$", abs(round(value_max, 0)))),
    value_min_lab = ifelse(value_min < 0, 
                               paste0("-$", abs(round(value_min, 0))), 
                               paste0("+$", abs(round(value_min, 0)))),
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
                      paste0("Typical"), #,\n", round(nrate_lbac, 0), " lb N/ac"),
                      paste0("Reduced")),#\n", round(nrate_lbac, 0), " lb N/ac")),
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
    mutate(rep_lab = paste0("Rep ", rep)) %>% 
    ggplot(aes(trt, yield_buac)) +
    geom_col(aes(group = rep, fill = trt),
             color = "black",
             position = position_dodge(width = .9),
             show.legend = F) +
    #--yields
    geom_text(aes(x = trt,
                  y = 1.15 * yield_max,
                  label = yld_lab),
              check_overlap = T,
              fontface = "italic") +
    #-rep labels
    geom_text(aes(x = trt, y = 5, label = rep_lab, group = rep),
              position = position_dodge(width = .9), 
              angle = 90,
              hjust = 0, 
              color = "gray") +
    #--diff
    geom_text(aes(x = 1.5,
                  y = 1.4 * yield_max,
                  label = sig_lab),
              check_overlap = T,
              fontface = "bold") +
    scale_y_continuous(expand = expansion(0.1)) +
    scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) +
    theme(axis.text.x = element_text(size = rel(1.2))) +
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
    theme(axis.text.x = element_text(size = rel(1.2))) +
    scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) +
    labs(x = NULL,
         y = "lb N per bushel",
         title = str_wrap("Nitrogen applied per unit corn yield", width = 20))
  
  return(fig)
  
}

fig2 <- NUEFig(data = tst2)

fig2



# money fig ---------------------------------------------------------------

#--the text is getting cut off in most figs, need to move down or adjust exapansion

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
        y = value_min,
        yend = value_max,
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
      yend = value_max + 1,
      y = value_max + 12
    ),
    arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(aes(
      xend = 1,
      x = 1.2,
      yend = value_min - 1,
      y = value_min - 15
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
        y = value_max + 12,
        label = paste0(
          "Best-case, ",
          value_max_lab,
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
        y = value_min - 15,
        label = paste0(
          "Worst-case, ",
          value_min_lab,
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
    theme(axis.text.x = element_text(size = rel(1.2))) +
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

fig1 + fig3 + plot_layout(widths = c(0.6, 0.4)) & 
  plot_annotation(theme = theme_border,
                  title = "test")

# fig1 + fig3 + fig2 + plot_layout(widths = c(0.7, 0.3, 0.3)) & 
#   theme(plot.title = element_text(size = rel(1.3)))

ggsave("figs/ind-figs/test.png", width = 10, height = 6)


# loop it -----------------------------------------------------------------
i <- 2

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
  
  tst.nlo <- tst %>% filter(grepl("Reduced", trt)) %>% pull(nrate_lbac) %>% 
    unique() %>% round()
  
  tst.nhi <- tst %>% filter(!grepl("Reduced", trt)) %>% pull(nrate_lbac) %>% 
    unique() %>% round()
  
  tmp.plot.title = paste0("Impact of reducing N from ", 
                          tst.nhi, 
                          " lb/ac to ",
                          tst.nlo, 
                          " lb/ac in 2022")
  
  fig1 + fig3 + plot_layout(widths = c(0.5, 0.5)) & 
    plot_annotation(theme = theme_border,
                    title = tmp.plot.title) &
    theme(plot.title = element_text(size = rel(1.3)))
  
  ggsave(paste0("figs/ind-figs/", my_names[i], ".png"), 
         height = 4.5, width = 7)
  
  
}

