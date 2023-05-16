#--make summary figure w/weather, money, yields
#--note the fonts aren't working, cufrently times new roman

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)


rm(list = ls())

source("code/04_figs/00_weather-figs-fxns.R")
source("code/04_figs/00_fig-colors.R")


# theme -------------------------------------------------------------------

my_combo_theme <-
  theme_bw() +
  theme(
    axis.title = element_text(size = rel(1.1)),
    strip.text = element_text(size = rel(1.2)),
    strip.background = element_rect(fill = pfi_tan),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.1)),
    panel.grid.minor = element_blank(),
    # panel.grid.major.y = element_blank(),
    plot.caption = element_text(hjust = 1),
    #panel.border = element_blank(),
    #plot.title.position = "plot",
    plot.caption.position =  "plot",
    text = element_text(family = "Times New Roman")
  )

#--for overal patchworked fig
theme_border <- 
  theme(
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3),
    text = element_text(family = "Times New Roman"))

# data --------------------------------------------------------------------

tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label)

y <- read_csv("data_tidy/td_cornyields.csv") %>% 
  left_join(tk)

y_st <- read_csv("data_stats/stat_cornyield.csv") %>% 
  left_join(tk)

#--money

m_st <- 
  read_csv("data_stats/stat_money.csv") %>% 
  left_join(tk)

m_raw <- 
  read_csv("data_tidy/td_money.csv") %>% 
  left_join(tk)

#--combine data and stats
m_dst <- 
  m_raw %>%
  select(trial_key, trial_label, everything()) %>% 
  pivot_longer(4:ncol(.))  %>% 
  left_join(st_m %>% select(-estimate) %>% rename(name = var)) %>% 
  group_by(trial_label) %>% 
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

#--no idea
m <-
  m_dst %>%
  select(trial_label, value_min, value_max, fin_sig, clr) %>% 
  left_join(
    m_st %>% 
      filter(var == "midsav_dolac") %>% 
      select(trial_label, estimate) %>% 
      rename(midsav_dolac = estimate)
  ) %>% 
  distinct() %>% 
  mutate(
    value_max_lab = ifelse(value_max < 0, 
                              paste0("-$", abs(round(value_max, 0)), "/ac"), 
                              paste0("+$", abs(round(value_max, 0)), "/ac")),
    value_min_lab = ifelse(value_min < 0, 
                               paste0("-$", abs(round(value_min, 0)), "/ac"), 
                               paste0("+$", abs(round(value_min, 0)), "/ac")),
    midsav_dolac_lab = ifelse(midsav_dolac < 0, 
                             paste0("-$", abs(round(midsav_dolac, 0)), "/ac"), 
                             paste0("+$", abs(round(midsav_dolac, 0)), "/ac"))
  ) 


my_names <- 
  m |> 
  pull(trial_label) |> 
  unique()


#---weather data

tkcity <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label, city)

w <- 
  read_csv("data_tidy/td_wea.csv") %>% 
  full_join(tkcity, by = "city")

t <- 
  w %>% 
  filter(grepl("temperature", wea_type))

cp <- 
  w %>% 
  filter(grepl("precip", wea_type))


#--need estimated reduction
y2 <-
  y |>  
  left_join(y_st) |> 
  #--get rid of bakehouse's NA rep
  filter(!is.na(yield_buac)) %>%  
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
  group_by(trial_label) |> 
  mutate(yield_max = max(yield_buac, na.rm = T)) 



# functions ---------------------------------------------------------------

#--test
y.tst <- y2 %>% filter(trial_key == "amun_22")

YieldFig <- function(y.data = y.tst) {
  
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
         y = "Bushels per ac",
         title = "Corn yield response",
        caption = "*Significance at 95% confidence level\nNumbers may not match exactly due to rounding")
  
  return(fig)
  
}

# tst <- y |>
#    filter(trial_label == my_names[7])
# 
# fig1 <- YieldFig(y.data = tst)
# 
# fig1

m.tst <- m %>% filter(trial_label == "Bakehouse")

MoneyFig <- function(m.data = m.tst) {
  
  m.data %>%
    ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    #--range, white
    geom_segment(
      aes(
        x = trial_label,
        xend = trial_label,
        y = value_min,
        yend = value_max,
      ),
      color = "white",
      linewidth = 8,
      show.legend = F
    )  +
    #--range alpha
    geom_segment(
      aes(
        x = trial_label,
        xend = trial_label,
        y = value_min,
        yend = value_max,
        color = clr
      ),
      alpha = 0.5,
      linewidth = 8,
      show.legend = F
    )  +
    #--top of range
    geom_segment(
      aes(
        x = trial_label,
        xend = trial_label,
        y = value_max - 5,
        yend = value_max,
        color = clr
      ),
      linewidth = 8,
      show.legend = F
    ) +
    #--bottom of range
    geom_segment(
      aes(
        x = trial_label,
        xend = trial_label,
        y = value_min,
        yend = value_min + 5,
        color = clr
      ),
      linewidth = 8,
      show.legend = F
    ) +
  #--midpoint
    geom_point(
      aes(x = trial_label,
          y = midsav_dolac,
          color = clr),
      pch = 17,
      show.legend = F,
      size = 4
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
      yend = midsav_dolac,
      y = midsav_dolac + 2
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
        y = midsav_dolac + 2,
        label = paste0("Midpoint, ", midsav_dolac_lab)
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
      y = "Dollars per acre",
      title = str_wrap(
        "Financial outcome**",
        width = 40
      ),
      caption = "**N prices ranged from $0.60-$1.20/lb N\n Corn revenue ranged from $5.70-$7.48/bu"
    )
  
  
  
}

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



# test - wea----------------------------------------------------------------

# wfig1 <-
#   TempFigInd(f.data = t, f.trial_label = "Veenstra1") +
#   labs(title = "Temperature")
# 
# wfig2 <-
#   CumPrecipFigInd(f.data = cp, f.trial_label = "Veenstra2") +
#   labs(title = "Precipitation")
# 
# fig_wea <-
#   wfig1 + wfig2



# test - patchwork a patchwork --------------------------------------------------

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
    y2 |> 
    filter(trial_label == my_names[i]) 
  
  fig1 <- YieldFig(y.data = tmp.ydat)
  
 
  tmp.mdat <- 
    m %>% 
    filter(trial_label == my_names[i])
  
  fig3 <- MoneyFig(m.data = tmp.mdat)
  
  fig_res <- 
    fig1 + fig3 + plot_layout(widths = c(0.5, 0.5)) & 
    plot_annotation(theme = theme_border) 
  
  
  
  #--weather
  tmp.name <- my_names[i]

  wfig1 <- 
    TempFigInd(f.data = t, f.trial_label = tmp.name) +
    labs(title = "Temperature")
  
  wfig2 <- 
    CumPrecipFigInd(f.data = cp, f.trial_label = tmp.name) + 
    labs(title = "Precipitation")
  
  fig_wea <- 
    wfig1 + wfig2  
  
  #--overall plot title
  tst.nlo <- 
    tmp.ydat %>% filter(grepl("Reduced", trt)) %>% pull(nrate_lbac) %>% 
    unique() %>% round()
  
  tst.nhi <- 
    tmp.ydat %>% filter(!grepl("Reduced", trt)) %>% pull(nrate_lbac) %>% 
    unique() %>% round()
  
  tmp.loc <- t %>% filter(trial_label == tmp.name) %>% pull(city) %>% unique() %>% str_to_title()
  
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

