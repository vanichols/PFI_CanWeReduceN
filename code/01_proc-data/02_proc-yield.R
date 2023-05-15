#--process stefan's data
#--they all have different column names

library(tidyverse)
library(janitor)

rm(list = ls())

# who should we have data for? --------------------------------------------

d <- read_csv("data_tidy/td_trialkey.csv")

#--Stefan's file names use the 'trial label' capitalization/notation (except Veenstra, fix)
coops <- 
  d |> 
  arrange(trial_label) %>% 
  select(trial_key, trial_label) %>% 
  mutate(trial_label = ifelse(grepl("Veenstra", trial_label), "Veenstra", trial_label))

# functions ---------------------------------------------------------------

CleanData <- function(i = 1){

  tmp.key <- coops %>% slice(i)
    
  f.data <- read_csv(paste0("data_stefan/", tmp.key$trial_label, ".ReduceN.yield.final.csv")) 
  
  tmp.data <- 
    f.data |> 
    clean_names() |> 
    mutate_if(is.character, str_to_lower) |> 
    mutate(trial_key = tmp.key$trial_key) 
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
  select(trial_key, rep, trt, nrate_lbac, yield_buac)



# 2. anderson ----------------------------------------------------------------------
# added n rates manually
d2 <- 
  CleanData(i = 2) |> 
  rename(
    trt = 2, 
    yield_buac = 4)  |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)


# 3. bakehouse----------------------------------------------------------------------

#--rep 1 typical received less n than it should have

d3 <- 
  CleanData(i = 3) |> 
  rename(
    yield_buac = 5,
       trt = 2,
       nrate_lbac = 4)  |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)
  
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
  select(trial_key, rep, trt, nrate_lbac, yield_buac)


# 5. bennett----------------------------------------------------------------------


d5 <- 
  CleanData(i = 5) |> 
  rename(trt = 3,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)

#6.  borchardt----------------------------------------------------------------------
# added n rates manually

d6 <- 
  CleanData(i = 6) |> 
  rename(trt = 3,
         nrate_lbac = 5,
         yield_buac = 4) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)


# 7. boyer----------------------------------------------------------------------


d7 <- 
  CleanData(i = 7) |> 
  rename(trt = 2,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)

# 8. deal----------------------------------------------------------------------

d8 <- 
  CleanData(i = 8) |> 
  rename(trt = 3,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)


# 9. dooley----------------------------------------------------------------------

d9 <- 
  CleanData(i = 9) |> 
  rename(trt = 3,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)

# 10. frederick----------------------------------------------------------------------

d10 <- 
  CleanData(i = 10) |> 
  rename(trt = 2,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)

# 11. fredericks----------------------------------------------------------------------

d11 <- 
  CleanData(i = 11) |> 
  rename(trt = 2,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  mutate(rep = ifelse(hybrid == 1, rep, rep + 4)) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)

# 12. harvey----------------------------------------------------------------------

d12 <- 
  CleanData(i = 12) |> 
  rename(trt = 2,
         nrate_lbac = 3,
         yield_buac = 4) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)

# 13. prevo----------------------------------------------------------------------

# added n rates manually
d13 <- 
  CleanData(i = 13) |> 
  rename(trt = 2,
         nrate_lbac = 5,
         yield_buac = 4) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)


# 14.  sieren----------------------------------------------------------------------

# added n rates
d14 <- 
  CleanData(i = 14) |> 
  rename(trt = 2,
         nrate_lbac = 4,
         yield_buac = 3) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)

# 15 and 16. veenstra 1 and 2---------------------------------------------------------------------
#--added n rates
#--veenstra did 2 fields

d15_tmp <- 
  CleanData(i = 15) |> 
  rename(trt = 3,
         nrate_lbac = 5,
         yield_buac = 4) |>
  mutate(trial_key = ifelse(field == 1, 
                            paste0("veen1_22"),
                            paste0("veen2_22"))) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)

#--just to make it easy later
d15 <- 
  d15_tmp %>% 
  filter(trial_key == "veen1_22")

d16 <- 
  d15_tmp %>% 
  filter(trial_key == "veen2_22")

# 17. waldo---------------------------------------------------------------------
#--added n rates
#--there was no rep 6, but there was 7 and 8

d17 <- 
  CleanData(i = 17) |> 
  rename(trt = 3,
         nrate_lbac = 4,
         yield_buac = 5) |> 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)


# combine -----------------------------------------------------------------

d_all <- NULL

for (i in 1:nrow(coops)){

  d.tmp <- eval(parse(text = paste0("d", i)))
  
  d_all <- 
    d_all |> 
    bind_rows(d.tmp)
  
}

d_all %>%
  dplyr::group_by(trial_key, rep, trt) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

d_all2 <- 
  d_all |> 
  mutate(trt = ifelse(trt == "typical", "typ", "red")) |> 
  arrange(trial_key, rep, trt)

d_all2 |> 
  write_csv("data_tidy/td_cornyields.csv")



# make df with diffs ------------------------------------------------------

d_diff <- 
  d_all2 %>% 
  pivot_longer(nrate_lbac:yield_buac) %>% 
  pivot_wider(names_from = trt, values_from = value) %>% 
  mutate(dif = typ - red,
         name = paste0("dif_", name)) %>% 
  select(trial_key, rep, name, dif) %>% 
  pivot_wider(names_from = name, values_from = dif) %>% 
  filter(!is.na(dif_yield_buac)) 

d_diff %>% 
  write_csv("data_tidy/td_trtdiffs.csv")
