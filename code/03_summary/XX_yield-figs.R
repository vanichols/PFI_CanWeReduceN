library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)


rm(list = ls())

source("code/00_fig-things.R")
theme_set(my_yld_theme)

# data --------------------------------------------------------------------

d <- read_csv("data_tidy/yields.csv")
s <- read_csv("data_tidy/stats.csv")
#m <- read_csv("data_tidy/stats-mixed.csv")

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
    geom_text(aes(x = 1.5, 
                  y = 1.4 * yield_max,
                  label = sig_lab),
              check_overlap = T,
              fontface = "bold") + 
    scale_y_continuous(expand = expansion(0.1)) +
    scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) +
    labs(x = NULL,
         y = "Corn yield (bu/ac)",
         title = "Corn Yield Response",
         caption = "*Significance at 95% confidence level\nNumbers may not match exactly due to rounding")
  
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
         y = "Nitrogen applied per\nbushel of corn yielded (lb N/bu)",
         title = "Nitrogen Use Metric")
  
  return(fig)
  
}

fig2 <- NUEFig(data = tst2)

fig2


# combine -----------------------------------------------------------------


fig1 + fig2 + plot_layout(widths = c(0.7, 0.3))

ggsave("figs/yields/test.png")


# loop it -----------------------------------------------------------------


#--loop it


for (i in 1:length(my_names)){
  

  tst <- 
    y |> 
    filter(last_name == my_names[i]) 
  
  fig1 <- YieldFig(data = tst)
  
  tst2 <- 
    y2 |> 
    filter(last_name == my_names[i])
  
  fig2 <- NUEFig(data = tst2)
  
  
  fig1 + fig2 + plot_layout(widths = c(0.7, 0.3))
  
  ggsave(paste0("figs/yields/", my_names[i], ".png"), 
         height = 4, width = 7)
  
  
}
