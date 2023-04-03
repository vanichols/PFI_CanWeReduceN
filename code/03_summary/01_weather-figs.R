library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)

rm(list = ls())

source("code/00_fig-things.R")
theme_set(my_wea_theme)

# data --------------------------------------------------------------------

ln_key <- 
  read_csv("data_raw/byhand_cooperator-locations.csv", skip = 5) |> 
  select(last_name, city)

t <- read_csv("data_wea/temperature-F.csv") |> 
  left_join(ln_key)

ct <- read_csv("data_wea/cum-temperature-F.csv") |> 
  left_join(ln_key)

p <- read_csv("data_wea/precip-in.csv") |> 
  left_join(ln_key)

cp <- read_csv("data_wea/cum-precip-in.csv") |> 
  left_join(ln_key)


# precipitation -----------------------------------------------------------

#--monthly deviation from long term average, precip
#--I don't like this one
p |>
  group_by(city, mm, name) |>
  summarise(value = mean(value, na.rm = T)) |>
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(dev_p = p_22-p_lt,
         date = paste("2022", mm, "01", sep = "/"),
         date_mm = as_date(date)) |> 
  ggplot(aes(date_mm, dev_p)) + 
  geom_line(aes(group = city, 
                color = city == "osage", 
                size = city == "osage", 
                alpha = city == "osage"),
            show.legend = F) + 
  geom_hline(yintercept = 0) +
  geom_text(aes(x = as_date("2022/01/01"),
                y = 0.15,
                hjust = 0,
                label = "Wetter than average"),
            check_overlap = T,
            fontface = "italic") +
  geom_text(aes(x = as_date("2022/01/01"),
                y = -0.07,
                hjust = 0,
                label = "Drier than average"),
            check_overlap = T,
            fontface = "italic") +
  scale_color_manual(values = c(pfi_blue, pfi_orange)) + 
  scale_size_manual(values = c(0.5, 2)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_x_date() +
  labs(x = NULL,
       y = "Difference in\nmonthly precipitation\nfrom 30-year average\n(inches)")


#--deviation from long term average, cum precip

CumPrecipFig <- function(f.data = cp, f.last_name = "bakehouse") {
    
    f.data |>
      mutate(dd_date = as.Date(doy - 1, 
                               origin = "2022/01/01")) |> 
      pivot_wider(names_from = name, values_from = value) |> 
      mutate(dev_cp = cp_22 - cp_lt) |> 
      ggplot(aes(dd_date, dev_cp)) + 
      geom_hline(yintercept = 0) +
    geom_line(aes(group = last_name, 
                  color = last_name == f.last_name, 
                  size = last_name == f.last_name,
                  alpha = last_name == f.last_name),
              show.legend = F) + 
      geom_text(aes(x = as.Date("2022/01/01"), 
                    y = 13, 
                    hjust = 0,
                    label = "Wetter than average"),
                check_overlap = T,
                color = "gray50",
                fontface = "italic") +
      geom_text(aes(x = as.Date("2022/01/01"), 
                    y = -5, 
                    hjust = 0,
                    label = "Drier than average"),
                check_overlap = T,
                color = "gray50",
                fontface = "italic") +
      scale_x_date(date_breaks = "1 month", 
                   date_labels = "%b") +
      labs(x = NULL,
           y = "Cumulative precipitation deviation (inches)")
    
  }
  
fig_cp <- 
  CumPrecipFig(f.data = cp, f.last_name = "bakehouse")  +
  scale_size_manual(values = c(0.6, 0.6)) +
  scale_alpha_manual(values = c(1, 1)) + 
  scale_color_manual(values = c(pfi_blue, pfi_blue))

fig_cp

#--single place highlighted test

CumPrecipFig(data = cp) +
  scale_size_manual(values = c(0.5, 2)) +
  scale_alpha_manual(values = c(0.5, 1)) + 
  scale_color_manual(values = c(pfi_blue, pfi_yellow))



# temperature -------------------------------------------------------------

#--deviation from long term average, temp


#--I can't figure out how to make it draw the 'selceted one' last

TempFig <- function(f.data = t, f.last_name = "bakehouse") {
  
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
           f_order = fct_rev(f_order)) |> 
    ggplot(aes(date_mm, dev_t)) + 
    geom_line(aes(group = last_name, 
                  color = last_name == f.last_name, 
                  size = last_name == f.last_name,
                  alpha = last_name == f.last_name),
              show.legend = F) + 
    geom_hline(yintercept = 0) +
    geom_text(aes(x = as_date("2022/01/01"),
                  y = 3,
                  hjust = 0,
                  label = "Hotter than average"),
              check_overlap = T,
              fontface = "italic",
              color = "gray50") +
    geom_text(aes(x = as_date("2022/01/01"),
                  y = -6,
                  hjust = 0,
                  label = "Cooler than average"),
              check_overlap = T,
              fontface = "italic",
              color = "gray50") +
  
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    labs(x = NULL,
         y = "Monthly average temperature deviation (deg F)")
  
  
}  

fig_t <- 
  TempFig(f.data = t, f.last_name = "Bakehouse") +
  scale_size_manual(values = c(0.6, 0.6)) +
  scale_alpha_manual(values = c(1, 1)) + 
  scale_color_manual(values = c(pfi_red, pfi_red))


#--test single coop
TempFig(f.data = t, f.last_name = "bakehouse") +
  scale_size_manual(values = c(0.5, 2)) +
  scale_alpha_manual(values = c(0.5, 1)) + 
  scale_color_manual(values = c(pfi_red, pfi_yellow))



# put together ------------------------------------------------------------

fig_t + fig_cp

ggsave("figs/wea.png", width = 7, height = 4)



# loop it -----------------------------------------------------------------

my_ln <- 
  ln_key |> 
  pull(last_name)


for (i in (1:length(my_ln))) {
  
  my_last_name <- my_ln[i]
  
  f.fig1 <- 
    TempFig(f.data = t, f.last_name = my_last_name) +
    scale_size_manual(values = c(0.5, 2)) +
    scale_alpha_manual(values = c(0.5, 1)) + 
    scale_color_manual(values = c(pfi_red, pfi_red))
  
  f.fig2 <- 
    CumPrecipFig(f.data = cp,
               f.last_name = my_last_name) +
    scale_size_manual(values = c(0.5, 2)) +
    scale_alpha_manual(values = c(0.5, 1)) + 
    scale_color_manual(values = c(pfi_blue, pfi_blue))

  f.fig1 + f.fig2  

  ggsave(paste0("figs/wea/", my_last_name, ".png"), width = 7, height = 4)  
  
}

# scratch -----------------------------------------------------------------


pc_final |> 
  ggplot(aes(doy, value)) +
  geom_line(aes(color = name), size = 3) + 
  facet_wrap(~city, scales = "free")

t_final |> 
  filter(city == "albion") |> 
  ggplot(aes(doy, value)) +
  geom_line(aes(color = name), size = 3)

ct_final |> 
  filter(city == "albion") |> 
  ggplot(aes(doy, value)) +
  geom_line(aes(color = name), size = 3)


