library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)

rm(list = ls())

source("code/00_fig-things.R")

# data --------------------------------------------------------------------

ln_key <- 
  read_csv("data_raw/byhand_cooperator-locations.csv", skip = 5) |> 
  select(last_name, city)

t <- 
  read_csv("data_wea/temperature-F.csv") |> 
  arrange(city) %>% 
  full_join(ln_key, by = "city")

ct <- read_csv("data_wea/cum-temperature-F.csv") |> 
  full_join(ln_key)

p <- read_csv("data_wea/precip-in.csv") |> 
  full_join(ln_key)

cp <- read_csv("data_wea/cum-precip-in.csv") |> 
  full_join(ln_key)


# temperature -------------------------------------------------------------

#--deviation from long term average, temp

TempFigInd <- function(f.data = t, f.last_name = "fredericks") {
  
  tmp.data <- 
    f.data |>
    group_by(last_name, city, mm, name) |> 
    summarise(value = mean(value, na.rm = T)) |> 
    pivot_wider(names_from = name, values_from = value) |> 
    mutate(dev_t = t_22 - t_lt,
           date = paste("2022", mm, "01", sep = "/"),
           date_mm = as_date(date),
           f_order = ifelse(last_name == f.last_name, 2, 1)) |> 
    arrange(f_order) |> 
    mutate(f_order = as_factor(f_order),
           f_order = fct_rev(f_order))
  
  ggplot() + 
    geom_hline(yintercept = 0) +
    geom_line(data = tmp.data, 
              aes(date_mm, dev_t, group = last_name),
              color = pfi_tan,
              size = 1,
              show.legend = F) + 
    geom_line(data = tmp.data %>% filter(last_name == f.last_name),
              aes(date_mm, dev_t, group = last_name),
              color = pfi_red, 
              size = 3,
              show.legend = F) + 
    geom_text(aes(x = as_date("2022/07/01"),
                  y = 5.5,
                  #hjust = 0,
                  label = "Warmer than average"),
              check_overlap = T,
              fontface = "italic",
              color = "gray50") +
    geom_text(aes(x = as_date("2022/07/01"),
                  y = -5.5,
                  #hjust = 0,
                  label = "Cooler than average"),
              check_overlap = T,
              fontface = "italic",
              color = "gray50") +
    scale_y_continuous(limits = c(-6, 6)) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    labs(x = NULL,
         y = "Monthly average temperature,\ndeviation from average (deg F)")
  
  
}  



TempFig <- function(f.data = t) {
  
  tmp.data <- 
    f.data |>
    group_by(last_name, city, mm, name) |> 
    summarise(value = mean(value, na.rm = T)) |> 
    pivot_wider(names_from = name, values_from = value) |> 
    mutate(dev_t = t_22 - t_lt,
           date = paste("2022", mm, "01", sep = "/"),
           date_mm = as_date(date))
  
  ggplot() + 
    geom_hline(yintercept = 0) +
    geom_line(data = tmp.data, 
              aes(date_mm, dev_t, group = last_name),
              color = pfi_red,
              size = 1,
              show.legend = F) + 
     geom_text(aes(x = as_date("2022/07/01"),
                  y = 5.5,
                  #hjust = 0,
                  label = "Warmer than average"),
              check_overlap = T,
              fontface = "italic",
              color = "gray50") +
    geom_text(aes(x = as_date("2022/07/01"),
                  y = -5.5,
                  #hjust = 0,
                  label = "Cooler than average"),
              check_overlap = T,
              fontface = "italic",
              color = "gray50") +
    scale_y_continuous(limits = c(-6, 6)) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    labs(x = NULL,
         y = "Monthly average temperature,\ndeviation from average (deg F)")
  
  
}  


# precipitation -----------------------------------------------------------

#--deviation from long term average, cum precip

CumPrecipFigInd <- function(f.data = cp, f.last_name = "fredericks") {
  
  tmp.data <- 
    f.data |>
    mutate(dd_date = as.Date(doy - 1, 
                             origin = "2022/01/01")) |> 
    pivot_wider(names_from = name, values_from = value) |> 
    mutate(dev_cp = cp_22 - cp_lt)
  
  ggplot() + 
    geom_hline(yintercept = 0) +
    geom_line(data = tmp.data, 
              aes(dd_date, dev_cp, group = last_name),
              color = pfi_tan,
              size = 1,
              show.legend = F) + 
    geom_line(data = tmp.data %>% filter(last_name == f.last_name),
              aes(dd_date, dev_cp, group = last_name),
              color = pfi_blue, 
              size = 3,
              show.legend = F) + 
    geom_text(aes(x = as.Date("2022/07/01"), 
                  y = 13, 
                  #hjust = 0,
                  label = "Wetter than average"),
              check_overlap = T,
              color = "gray50",
              fontface = "italic") +
    geom_text(aes(x = as.Date("2022/07/01"), 
                  y = -13, 
                  #hjust = 0,
                  label = "Drier than average"),
              check_overlap = T,
              color = "gray50",
              fontface = "italic") +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    scale_y_continuous(limits = c(-15, 15), breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
    labs(x = NULL,
         y = "Cumulative precipitation,\ndeviation from average (inches)")
  
}


CumPrecipFig <- function(f.data = cp) {
  
  tmp.data <- 
    f.data |>
    mutate(dd_date = as.Date(doy - 1, 
                             origin = "2022/01/01")) |> 
    pivot_wider(names_from = name, values_from = value) |> 
    mutate(dev_cp = cp_22 - cp_lt)
  
  ggplot() + 
    geom_hline(yintercept = 0) +
    geom_line(data = tmp.data, 
              aes(dd_date, dev_cp, group = last_name),
              color = pfi_blue,
              linewidth = 1,
              show.legend = F) + 
    geom_text(aes(x = as.Date("2022/07/01"), 
                  y = 13, 
                  #hjust = 0,
                  label = "Wetter than average"),
              check_overlap = T,
              color = "gray50",
              fontface = "italic") +
    geom_text(aes(x = as.Date("2022/07/01"), 
                  y = -13, 
                  #hjust = 0,
                  label = "Drier than average"),
              check_overlap = T,
              color = "gray50",
              fontface = "italic") +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    scale_y_continuous(limits = c(-15, 15), breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
    labs(x = NULL,
         y = "Cumulative precipitation,\ndeviation from average (inches)")
  
}


# put together ------------------------------------------------------------


my_wea_theme <- 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = "gray"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family = "Chaparral")) 


theme_set(my_wea_theme)


fig_cp <-
  CumPrecipFig(f.data = cp)

fig_cp

fig_t <- 
  TempFig(f.data = t) + 
    geom_text(aes(x = as.Date("2022-06-28"), y = -2.5, label = "Cool Aprils"),
              check_overlap = T) + 
    geom_segment(aes(xend = as.Date("2022-04-15"),
                     yend = -3.5, 
                     x = as.Date("2022-06-05"),
                     y = -2.7),
                 arrow = arrow())
  

fig_t



fig_t + fig_cp + 
  plot_annotation(
    theme = theme_border,
    title = str_wrap("Weather deviations from historical averages", width = 80),
    subtitle = str_wrap("Cool Aprils, dry growing seasons at all 17 trials", width = 80))

ggsave("figs/wea.png", width = 7, height = 5)


# loop it through sites-----------------------------------------------------------------


for (i in (1:nrow(ln_key))) {
  
  my_coop <- 
    ln_key |> 
    slice(i) 
  
  my_last_name <- my_coop %>% pull(last_name)
  
  my_location <- my_coop %>% pull(city) %>% str_to_title()
  
  f.fig1 <- 
    TempFigInd(f.data = t, f.last_name = my_last_name) +
    scale_color_manual(values = c(pfi_tan, pfi_red)) +
    scale_size_manual(values = c(1, 3)) 
  
  f.fig2 <- 
    CumPrecipFigInd(f.data = cp,
               f.last_name = my_last_name) +
    scale_size_manual(values = c(1, 3)) +
    scale_color_manual(values = c(pfi_tan, pfi_blue))

  f.fig1 + f.fig2  + 
    plot_annotation(theme = theme_border,
                    title = paste(my_location, "weather")) & 
    theme(plot.title = element_text(size = rel(1.3)))

  ggsave(paste0("figs/wea/", my_last_name, ".png"), width = 6, height = 4)  
  
}


