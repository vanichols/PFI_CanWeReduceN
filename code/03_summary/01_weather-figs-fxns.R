library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)

rm(list = ls())

source("code/00_fig-things.R")


my_wea_theme <- 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = "gray50"),
        panel.background = element_blank(),
        panel.border = element_blank()) 



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
              family = "Times New Roman",
              color = "gray50") +
    geom_text(aes(x = as_date("2022/07/01"),
                  y = -5.5,
                  #hjust = 0,
                  label = "Cooler than average"),
              check_overlap = T,
              fontface = "italic",
              family = "Times New Roman",
              color = "gray50") +
    scale_y_continuous(limits = c(-6, 6)) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    labs(x = NULL,
         y = "Monthly average temperature,\ndeviation from average (deg F)") + 
    my_wea_theme
  
  
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
              family = "Times New Roman",
              color = "gray50") +
    geom_text(aes(x = as_date("2022/07/01"),
                  y = -5.5,
                  #hjust = 0,
                  label = "Cooler than average"),
              check_overlap = T,
              fontface = "italic",
              family = "Times New Roman",
              color = "gray50") +
    scale_y_continuous(limits = c(-6, 6)) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    labs(x = NULL,
         y = "Monthly average temperature,\ndeviation from average (deg F)") + 
    my_wea_theme
  
  
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
              family = "Times New Roman",
              fontface = "italic") +
    geom_text(aes(x = as.Date("2022/07/01"), 
                  y = -13, 
                  #hjust = 0,
                  label = "Drier than average"),
              check_overlap = T,
              color = "gray50",
              family = "Times New Roman",
              fontface = "italic") +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    scale_y_continuous(limits = c(-15, 15), breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
    labs(x = NULL,
         y = "Cumulative precipitation,\ndeviation from average (inches)") + 
    my_wea_theme
  
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
              size = 1,
              show.legend = F) + 
    geom_text(aes(x = as.Date("2022/07/01"), 
                  y = 13, 
                  #hjust = 0,
                  label = "Wetter than average"),
              check_overlap = T,
              color = "gray50",
              family = "Times New Roman",
              fontface = "italic") +
    geom_text(aes(x = as.Date("2022/07/01"), 
                  y = -13, 
                  #hjust = 0,
                  label = "Drier than average"),
              check_overlap = T,
              color = "gray50",
              family = "Times New Roman",
              fontface = "italic") +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    scale_y_continuous(limits = c(-15, 15), breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
    labs(x = NULL,
         y = "Cumulative precipitation,\ndeviation from average (inches)") + 
    my_wea_theme
  
}

