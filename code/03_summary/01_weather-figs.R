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

t <- read_csv("data_wea/temperature-F.csv") |> 
  left_join(ln_key)

ct <- read_csv("data_wea/cum-temperature-F.csv") |> 
  left_join(ln_key)

p <- read_csv("data_wea/precip-in.csv") |> 
  left_join(ln_key)

cp <- read_csv("data_wea/cum-precip-in.csv") |> 
  left_join(ln_key)

t %>% 
  filter(city == "galva") %>% 
  filter(mm == 4) %>% 
  group_by(name) %>% 
  summarise(value = mean(value))
  
cp %>% 
  filter(last_name == "anderson") %>% 
  filter(doy == max(doy))

# temperature -------------------------------------------------------------

#--deviation from long term average, temp

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
              show.legend = F, 
              size = 0.5) + 
    geom_hline(yintercept = 0) +
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

#--cumulative precip, lt by location
cp |> 
  filter(name == "cp_lt") |> 
  group_by(last_name) |> 
  summarise(cp = max(value))

cp |> 
  mutate(dd_date = as.Date(doy - 1, 
                           origin = "2022/01/01")) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(dev_cp = cp_22 - cp_lt) |> 
  group_by(last_name) |> 
  filter(doy == 305) |> 
  summarise(cp = max(dev_cp)) |> 
  arrange(cp)


ln_key

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
              show.legend = F, 
              size = 0.5) + 
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
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.background = element_blank(),
        panel.border = element_blank()) 


theme_set(my_wea_theme)


fig_cp <-
CumPrecipFig(f.data = cp, f.last_name = "bakehouse") +
  scale_color_manual(values = c(pfi_blue, pfi_blue)) +
  scale_size_manual(values = c(1, 1)) +
  scale_alpha_manual(values = c(1, 1)) +
  geom_text(aes(
    x = as.Date("2022-05-01"),
    y = -8,
    label = "Dry springs"
  ),
  check_overlap = T) +
  geom_segment(aes(
    xend = as.Date("2022-03-01"),
    yend = -2,
    x = as.Date("2022-05-01"),
    y = -7
  ),
  arrow = arrow()) +
  geom_text(aes(
    x = as.Date("2022-02-15"),
    y = 9, 
    label = "Wet summers"
  ),
  check_overlap = T) +
  geom_segment(aes(
    xend = as.Date("2022-06-1"),
    yend = 7.5,
    x = as.Date("2022-03-24"),
    y = 8
  ),
  arrow = arrow())


fig_cp


fig_t <- 
  TempFig(f.data = t, f.last_name = "bakehouse") +
  scale_color_manual(values = c(pfi_red, pfi_red)) + 
  scale_size_manual(values = c(1, 1)) + 
  scale_alpha_manual(values = c(1, 1)) + 
    geom_text(aes(x = as.Date("2022-06-28"), y = -2.5, label = "Cool Aprils"),
              check_overlap = T) + 
    geom_segment(aes(xend = as.Date("2022-04-15"),
                     yend = -4.5, 
                     x = as.Date("2022-06-05"),
                     y = -2.7),
                 arrow = arrow())
  

fig_t

fig_t + fig_cp + 
  plot_annotation(
    title = str_wrap("Weather deviations from historical averages", width = 80),
    subtitle = str_wrap("Cool Aprils, dry springs and wet summers at all 17 trials", width = 80))

ggsave("figs/wea.png", width = 8, height = 6)


# loop it through sites-----------------------------------------------------------------

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


