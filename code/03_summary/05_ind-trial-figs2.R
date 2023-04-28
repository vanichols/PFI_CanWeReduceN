library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(extrafont)

# font_import() 
# loadfonts(device = "win")

rm(list = ls())

source("code/03_summary/01_weather-figs-fxns.R")
source("code/00_fig-things.R")

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


my_names <- 
  d |> 
  pull(last_name) |> 
  unique()


# yield by rep fig --------------------------------------------------------

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

YieldFig <- function(y.data = y) {
  
  fig <- 
    y.data |>
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
              fontface = "italic", 
              family = "Times New Roman",
              color = "gray50") +
    #-rep labels
    geom_text(aes(x = trt, y = 5, label = rep_lab, group = rep),
              position = position_dodge(width = .9), 
              angle = 90,
              hjust = 0, 
              family = "Times New Roman",
              color = "gray") +
    #--diff
    geom_text(aes(x = 1.5,
                  y = 1.4 * yield_max,
                  label = sig_lab),
              check_overlap = T,
              family = "Times New Roman",
              color = "gray50") +
    scale_y_continuous(expand = expansion(0.1)) +
    scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) +
    my_combo_theme +
    theme(axis.text.x = element_text(size = rel(1.1))) +
    labs(x = NULL,
         y = "bu per ac",
         title = "Corn yield response",
        caption = "*Significance at 95% confidence level\nNumbers may not match exactly due to rounding")
  
  return(fig)
  
}

# tst <- y |>
#    filter(last_name == my_names[7])
# 
# fig1 <- YieldFig(y.data = tst)
# 
# fig1


# money fig ---------------------------------------------------------------

#--the text is getting cut off in most figs, need to move down or adjust exapansion

MoneyFig <- function(m.data = tst3) {
  
  m.data %>%
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
    color= "gray50",
    arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(aes(
      xend = 1,
      x = 1.2,
      yend = value_min - 1,
      y = value_min - 15
    ),
    color= "gray50",
    arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(aes(
      xend = 1.05,
      x = 1.35,
      yend = avg_savings,
      y = avg_savings + 2
    ),
    color= "gray50",
    arrow = arrow(length = unit(0.2, "cm"))) +
    #--text
    geom_text(
      aes(
        x = 1.25,
        y = value_max + 12,
        label = paste0(
          "Best-case, ",
          value_max_lab,
          "\n  (expensive N, low corn revenue)"
        )
      ),
      check_overlap = T,
      hjust = 0,
      family = "Times New Roman",
      fontface = "italic",
      color = "gray50"
    ) +
    geom_text(
      aes(
        x = 1.4,
        y = value_min - 15,
        label = paste0(
          "Worst-case, ",
          value_min_lab,
          "\n  (cheap N,  high corn revenue)"
        )
      ),
      check_overlap = T,
      hjust = 0,
      family = "Times New Roman",
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
      family = "Times New Roman",
      fontface = "italic",
      color = "gray50"
    ) +
    scale_y_continuous(labels = label_dollar(), expand = expansion(mult = 0.2)) +
    expand_limits(x = 3.5) +
    my_combo_theme +
    theme(axis.text.x = element_text(size = rel(1.1))) +
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

 # tst3 <-
 #   m |>
 #   filter(last_name == my_names[7])
# 
# fig3 <- MoneyFig(m.data = tst3)
# 
# fig3

# combine results -----------------------------------------------------------------


# fig_res <- 
#   fig1 + fig3 + 
#   plot_layout(widths = c(0.5, 0.5)) & 
#   plot_annotation(theme = theme_border,
#                   title = "Test") &
#   theme(plot.title = element_text(size = rel(1.3)))
# 
# fig_res
# 
# ggsave("figs/ind-figs/test.png", width = 10, height = 6)



# weather? ----------------------------------------------------------------

# wfig1 <- 
#   TempFigInd(f.data = t, f.last_name = "anderson") +
#   labs(title = "Temperature")
# 
# wfig2 <- 
#   CumPrecipFigInd(f.data = cp, f.last_name = "anderson") + 
#   labs(title = "Precipitation")
# 
# fig_wea <- 
#   wfig1 + wfig2  



# patchwork a patchwork? --------------------------------------------------

# fig_wea / fig_res & 
#   plot_annotation(theme = theme_border,
#                   title = "Test big") &
#   theme(plot.title = element_text(size = rel(1.3)),
#         text = element_text(family = "Times New Roman"))


# loop it -----------------------------------------------------------------
i <- 1

for (i in 1:length(my_names)){
  
  #--yield/money
  tmp.ydat <- 
    y |> 
    filter(last_name == my_names[i]) 
  
  fig1 <- YieldFig(y.data = tmp.ydat)
  
 
  tmp.mdat <- 
    m %>% 
    filter(last_name == my_names[i])
  
  fig3 <- MoneyFig(m.data = tmp.mdat)
  
  fig_res <- 
    fig1 + fig3 + plot_layout(widths = c(0.5, 0.5)) & 
    plot_annotation(theme = theme_border) 
  
  
  
  #--weather
  tmp.name <- my_names[i] %>% str_to_lower()

    wfig1 <- 
    TempFigInd(f.data = t, f.last_name = tmp.name) +
    labs(title = "Temperature")
  
  wfig2 <- 
    CumPrecipFigInd(f.data = cp, f.last_name = tmp.name) + 
    labs(title = "Precipitation")
  
  fig_wea <- 
    wfig1 + wfig2  
  
  #--overall plot title
  tst.nlo <- tmp.ydat %>% filter(grepl("Reduced", trt)) %>% pull(nrate_lbac) %>% 
    unique() %>% round()
  
  tst.nhi <- tmp.ydat %>% filter(!grepl("Reduced", trt)) %>% pull(nrate_lbac) %>% 
    unique() %>% round()
  
  tmp.loc <- t %>% filter(last_name == tmp.name) %>% pull(city) %>% unique() %>% str_to_title()
  
  tmp.plot.title = paste0("Impact of reducing N from ", 
                          tst.nhi, 
                          " lb/ac to ",
                          tst.nlo, 
                          " lb/ac in ",
                          tmp.loc, " IA, 2022")
  
  fig_wea / fig_res + 
    plot_annotation(theme = theme_border,
                    title = tmp.plot.title) &
    theme(plot.title = element_text(size = rel(1.4)),
          text = element_text(family = "Times New Roman"))
  
  ggsave(paste0("figs/ind-figs/", my_names[i], ".png"), 
         height = 6.5, width = 8)
  
  
}

