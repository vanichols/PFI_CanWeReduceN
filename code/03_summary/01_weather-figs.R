library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)

rm(list = ls())

source("code/00_fig-things.R")
theme_set(my_wea_theme)

# data --------------------------------------------------------------------

t <- read_csv("data_wea/temperature-F.csv")
ct <- read_csv("data_wea/cum-temperature-F.csv")
p <- read_csv("data_wea/precip-in.csv")
cp <- read_csv("data_wea/cum-precip-in.csv")


# temperature -------------------------------------------------------------

t |> 
  filter(is.na(value))

#--deviation from long term average, temp

fig_t <- 
  t |>
  group_by(city, mm, name) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(dev_t = t_22 - t_lt,
         date = paste("2022", mm, "01", sep = "/"),
         date_mm = as_date(date)) |> 
  ggplot(aes(date_mm, dev_t)) + 
  geom_line(aes(group = city, 
                color = city == "osage", 
                size = city == "osage", 
                alpha = city == "osage"),
            show.legend = F) + 
  geom_hline(yintercept = 0) +
  geom_text(aes(x = as_date("2022/01/01"),
                y = 3,
                hjust = 0,
                label = "Hotter than average"),
            check_overlap = T,
            fontface = "italic") +
  geom_text(aes(x = as_date("2022/01/01"),
                y = -6,
                hjust = 0,
                label = "Cooler than average"),
            check_overlap = T,
            fontface = "italic") +
  scale_color_manual(values = c(pfi_red, pfi_red)) + 
  scale_size_manual(values = c(0.5, 2)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b") +
  labs(x = NULL,
       y = "Monthly average temperature deviation (deg F)")


fig_t2 <- 
  fig_t +
  scale_size_manual(values = c(0.6, 0.6)) +
  scale_alpha_manual(values = c(1, 1))
  

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

fig_cp <- 
  cp |>
  mutate(dd_date = as.Date(doy - 1, 
                           origin = "2022/01/01")) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(dev_cp = cp_22 - cp_lt) |> 
  ggplot(aes(dd_date, dev_cp)) + 
  geom_hline(yintercept = 0) +
  geom_line(aes(group = city, 
                color = city == "osage", 
                size = city == "osage", 
                alpha = city == "osage"),
            show.legend = F) + 
  geom_text(aes(x = as.Date("2022/01/01"), 
                y = 13, 
                hjust = 0,
                label = "Wetter than average"),
            check_overlap = T,
            fontface = "italic") +
  geom_text(aes(x = as.Date("2022/01/01"), 
                y = -5, 
                hjust = 0,
                label = "Drier than average"),
            check_overlap = T,
            fontface = "italic") +
  scale_color_manual(values = c(pfi_blue, pfi_blue)) + 
  scale_size_manual(values = c(0.5, 2)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b") +
  labs(x = NULL,
       y = "Cumulative precipitation deviation (inches)") 

fig_cp2 <- 
  fig_cp + 
  scale_size_manual(values = c(0.6, 0.6)) +
  scale_alpha_manual(values = c(1, 1))

# put together ------------------------------------------------------------

fig_t + fig_cp
ggsave("figs/wea_osage.png")

fig_t2 + fig_cp2
ggsave("figs/wea.png")


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


