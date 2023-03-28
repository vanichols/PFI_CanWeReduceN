#--process stefan's data
#--they all have different column names

library(tidyverse)
library(janitor)

rm(list = ls())

# who should we have data for? --------------------------------------------

d <- read_csv("data_raw/byhand_cooperator-locations.csv", skip = 5)

coops <- 
  d |> 
  arrange(last_name) |> 
  pull(last_name) |> 
  str_to_sentence()



# functions ---------------------------------------------------------------

CleanData <- function(i = 1){
  
  f.data <- read_csv(paste0("data_stefan/", coops[i], ".ReduceN.yield.final.csv")) 
  
  tmp.data <- 
    f.data |> 
    clean_names() |> 
    mutate_if(is.character, str_to_lower) |> 
    mutate(last_name = coops[i]) 
  return(tmp.data)
  
}


# 1.amundson ----------------------------------------------------------------
# added n rates manually
d1 <- 
  CleanData(i = 1) |> 
  rename(
    yield_buac = 4,
    trt = 3,
    nrate_lbac = 5)  |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)



# 2. anderson ----------------------------------------------------------------------
# added n rates manually
d2 <- 
  CleanData(i = 2) |> 
  rename(
    trt = 2, 
    yield_buac = 4)  |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)


# 3. bakehouse----------------------------------------------------------------------

#--rep 1 typical received less n than it should have

d3 <- 
  CleanData(i = 3) |> 
  rename(
    yield_buac = 5,
       trt = 2,
       nrate_lbac = 4)  |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)
  
# 4. bardole----------------------------------------------------------------------

#--bardole was completely randomized, no true reps...
#--make them up

d4 <- 
  CleanData(i =4) |>
  mutate(rep = case_when(
    strip %in% c(2, 3) ~ 1,
    strip %in% c(4, 5) ~ 2,
    strip %in% c(1, 6) ~ 3,
    strip %in% c(7, 8) ~ 4
  )) |> 
  rename(trt = 2,
         nrate_lbac = 3,
         yield_buac = 4) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)


# 5. bennett----------------------------------------------------------------------


d5 <- 
  CleanData(i = 5) |> 
  rename(trt = 3,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)


#6.  borchardt----------------------------------------------------------------------
# added n rates manually

d6 <- 
  CleanData(i = 6) |> 
  rename(trt = 3,
         nrate_lbac = 5,
         yield_buac = 4) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)


# 7. boyer----------------------------------------------------------------------


d7 <- 
  CleanData(i = 7) |> 
  rename(trt = 2,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)


# 8. deal----------------------------------------------------------------------

d8 <- 
  CleanData(i = 8) |> 
  rename(trt = 3,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)



# 9. dooley----------------------------------------------------------------------

d9 <- 
  CleanData(i = 9) |> 
  rename(trt = 3,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)


# 10. frederick----------------------------------------------------------------------

d10 <- 
  CleanData(i = 10) |> 
  rename(trt = 2,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)


# 11. fredericks----------------------------------------------------------------------

d11 <- 
  CleanData(i = 11) |> 
  rename(trt = 2,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)

# 12. harvey----------------------------------------------------------------------

d12 <- 
  CleanData(i = 12) |> 
  rename(trt = 2,
         nrate_lbac = 3,
         yield_buac = 4) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)


# 13. prevo----------------------------------------------------------------------

# added n rates manually
d13 <- 
  CleanData(i = 13) |> 
  rename(trt = 2,
         nrate_lbac = 5,
         yield_buac = 4) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)



# 14.  sieren----------------------------------------------------------------------

# added n rates
d14 <- 
  CleanData(i = 14) |> 
  rename(trt = 2,
         nrate_lbac = 4,
         yield_buac = 3) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)


# 15. veenstra 1 and 2---------------------------------------------------------------------
#--added n rates
#--veenstra did 2 fields

d15 <- 
  CleanData(i = 15) |> 
  rename(trt = 3,
         nrate_lbac = 5,
         yield_buac = 4) |>
  mutate(last_name = ifelse(field == 1, 
                            paste0(last_name, "_1"),
                            paste0(last_name, "_2"))) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)

# 16. waldo---------------------------------------------------------------------
#--added n rates
#--there was no rep 6, but there was 7 and 8

d16 <- 
  CleanData(i = 16) |> 
  rename(trt = 3,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(last_name, rep, trt, nrate_lbac, yield_buac)


# combine -----------------------------------------------------------------

d_all <- NULL

for (i in 1:length(coops)){

  d.tmp <- eval(parse(text = paste0("d", i)))
  
  d_all <- 
    d_all |> 
    bind_rows(d.tmp)
  
}

d_all2 <- 
  d_all |> 
  mutate(trt = ifelse(trt == "typical", "typ", "red")) |> 
  arrange(last_name, rep, trt)

d_all2 |> 
  ggplot(aes(rep, yield_buac)) +
  geom_point(aes(fill = nrate_lbac), 
             color = "black", 
             pch = 21, 
             size = 4) +
  facet_wrap(~last_name, scale = "free_x") +
  scale_fill_viridis_c()

d_all2 |> 
  ggplot(aes(trt, yield_buac, group = rep)) +
  geom_col(aes(fill = nrate_lbac),
           position = position_dodge2(),
             color = "black") +
  facet_wrap(~last_name, scale = "free_x") +
  scale_fill_viridis_c()


#--what if we did it as a % of maximum yield?

#--get means for each group
yld_avg <- 
  d_all2 |>
  group_by(last_name, trt) |>
  summarise(avg_yield = mean(yield_buac, na.rm = T),
            nrate_lbac = mean(nrate_lbac, na.rm = T))
  


d_all2 |>
  group_by(last_name) |> 
  mutate(max_yield = max(yield_buac, na.rm = T),
         yield_pct = yield_buac/max_yield) |> 
  ggplot(aes(trt, yield_pct, group = rep)) +
  geom_col(aes(fill = nrate_lbac),
           position = position_dodge2(),
           color = "black") +
  facet_wrap(~last_name, scale = "free_x") +
  scale_fill_viridis_c()



d_all2 |>
  ggplot(aes(trt, yield_buac, group = rep)) +
  geom_col(aes(fill = nrate_lbac),
           position = position_dodge2(),
           color = "black") +
  geom_hline(data = yld_avg, 
             aes(yintercept = avg_yield,
                 color = nrate_lbac),
             size = 2) +
  facet_wrap(~last_name, scale = "free_x") +
  scale_fill_viridis_c() +
  scale_color_viridis_c()


d_all2 |>
  ggplot(aes(trt, yield_buac, group = rep)) +
  geom_hline(data = yld_avg, 
             aes(yintercept = avg_yield,
                 color = trt),
             size = 2) +
  geom_col(aes(fill = trt),
           position = position_dodge2(),
           color = "black") +
  facet_wrap(~last_name, scale = "free_x") +
  scale_fill_manual(values = c("lightgreen", "darkgreen")) +
  scale_color_manual(values = c("lightgreen", "darkgreen")) +
  theme_bw()
  
