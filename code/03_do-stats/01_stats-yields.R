#--do stats on yields
#--did mixed and fixed effect for rep (block), results are the same

library(tidyverse)
library(lubridate)
library(agricolae)
library(broom)
library(emmeans)

rm(list = ls())


# yields ------------------------------------------------------------------

d_raw <- read_csv("data_tidy/td_cornyields.csv")

d_raw %>% 
  filter(is.na(yield_buac))

#--get rid of the NA
y <-
  d_raw  |> 
  filter(!(trial_key == "bake_22" & rep == 1))

#--get the trial keys to loop through 
tk <- 
  y |> 
  pull(trial_key) |> 
  unique()



#--test individual
tst <- y |> filter(trial_key== "dool_22")


# 1. fixed effect stats -------------------------------------------------------------------

mf <- lm(yield_buac ~ trt, data = tst)

summary(mf) %>% 
  tidy()  %>%
  filter(term == "trttyp")

emmeans(mf, "trt") |> 
  tidy()

res_f <- NULL

i <- 1

for (i in 1:length(tk)) {

  tmp.tk <- tk[i]
  d.tmp <- y |> filter(trial_key== tmp.tk) 
  
  tmp.mod <- lm(yield_buac ~ trt, data = d.tmp)
  
  tmp.em <- 
    emmeans(tmp.mod, "trt") |> 
    tidy() |> 
    select(-std.error, -df, -p.value)
  
  tmp.smy <- 
    summary(tmp.mod) %>% 
    tidy()  %>%
    filter(term == "trttyp")
  
  tmp.lsd <- 
    (LSD.test(tmp.mod, "trt"))$statistics |> 
    as_tibble() |> 
    pull(LSD)

  tmp.res <-
    tmp.em |>
    mutate(
      trial_key= tmp.tk,
      diff_est = tmp.smy |> pull(estimate),
      pval = tmp.smy |> pull(p.value),
      lsd = tmp.lsd
    )
  
  res_f <- bind_rows(res_f, tmp.res)
    
}

res_f


# 2. random effect stats -------------------------------------------------------------------

library(LSDer)
library(lme4)
library(lmerTest)
library(emmeans)


mr <- lmer(yield_buac ~ trt + (1|rep), data = tst)

pval2 <- 
  anova(mr) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  pull(pr_f)

lsd2 <- 
  LSDer(mr, "trt", comps = NULL, level = 0.95) |> 
  as.numeric() 


res_r <- NULL

i <- 1

for (i in 1:length(tk)) {
  
  tmp.tk <- tk[i]
  d.tmp <- y |> filter(trial_key== tmp.tk) 
  
  tmp.mod <- lmer(yield_buac ~ trt + (1|rep), data = d.tmp)
  
  tmp.pval <- 
    anova(tmp.mod) |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    pull(pr_f)
  
  tmp.lsd <- 
    LSDer(tmp.mod, "trt", comps = NULL, level = 0.95) |> 
    as.numeric() 
  
  tmp.res <- 
    tibble(trial_key= tmp.tk,
           pval = tmp.pval,
           lds = tmp.lsd)
  
  res_r <- bind_rows(res_r, tmp.res)
  
}

res_r


# 3. compare -----------------------------------------------------------------

res_comp <-
  res_f |>
  mutate(sig_fixed = ifelse(pval < 0.05, "y", "n")) |>
  select(-pval, -lsd) |>
  left_join(
    res_r |>
      mutate(sig_rand = ifelse(pval < 0.05, "y", "n")) |>
      select(-pval, -lds)
  ) |>
  mutate(dif = ifelse(sig_fixed == sig_rand, "n", "y"))

#--sifnificances are the same
res_comp %>% 
  filter(dif == "y")


# write fixed effect results ----------------------------------------------

res_f |> 
  select(trial_key, trt, everything()) %>% 
  write_csv("data_stats/stat_cornyield.csv")

