library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(agricolae)
library(broom)
library(emmeans)

rm(list = ls())


# yields ------------------------------------------------------------------

d <- read_csv("data_tidy/yields.csv")


y <-
  d  |> 
  filter(!(last_name == "Bakehouse" & rep == 1)) |> 
  mutate(nue = nrate_lbac/yield_buac)

 
ln <- 
  y |> 
  pull(last_name) |> 
  unique()



#--test individual
tst <- y |> filter(last_name == "Boyer")


# 1. fixed effect stats -------------------------------------------------------------------

mod <- lm(yield_buac ~ trt, data = tst)

summary(mod) %>% 
  tidy()  %>%
  filter(term == "trttyp")

emmeans(mod, "trt") |> 
  tidy()

res <- NULL

for (i in 1:length(ln)) {

  tmp.ln <- ln[i]
  d.tmp <- y |> filter(last_name == tmp.ln) 
  
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
      last_name = tmp.ln,
      diff_est = tmp.smy |> pull(estimate),
      pval = tmp.smy |> pull(p.value),
      lsd = tmp.lsd
    )
  
  res <- bind_rows(res, tmp.res)
    
}

res


res |> 
  write_csv("data_tidy/stats.csv")


# 2. random effect stats -------------------------------------------------------------------

library(LSDer)
library(lme4)
library(lmerTest)
library(emmeans)


mod2 <- lmer(yield_buac ~ trt + (1|rep), data = tst)

pval2 <- 
  anova(mod2) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  pull(pr_f)

lsd2 <- 
  LSDer(mod2, "trt", comps = NULL, level = 0.95) |> 
  as.numeric() 


res2 <- NULL

for (i in 1:length(ln)) {
  
  tmp.ln <- ln[i]
  d.tmp <- y |> filter(last_name == tmp.ln) 
  
  
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
    tibble(last_name = tmp.ln,
           pval = tmp.pval,
           lds = tmp.lsd)
  
  res2 <- bind_rows(res2, tmp.res)
  
}

res2


res2 |> 
  write_csv("data_tidy/stats-mixed.csv")



# 3. NUE ---------------------------------------------------------------------

mod3 <- lm(nue ~ trt, data = tst)

res3 <- NULL

for (i in 1:length(ln)) {
  
  tmp.ln <- ln[i]
  d.tmp <- y |> filter(last_name == tmp.ln) 
  
  tmp.mod <- lm(nue ~ trt, data = d.tmp)
  
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
      last_name = tmp.ln,
      diff_est = tmp.smy |> pull(estimate),
      pval = tmp.smy |> pull(p.value),
      lsd = tmp.lsd
    )
  
  res3 <- bind_rows(res3, tmp.res)
  
}

res3


res3 |> 
  write_csv("data_tidy/stats-nue.csv")



# 4. money -------------------------------------------------------------------

m <- read_csv("data_tidy/money.csv")

tst4 <- m %>% filter(last_name == "Amundson")

mod4 <- t.test(tst4$avg_savings)

tidy(mod4)

res4 <- NULL

for (i in 1:length(ln)) {
  
  tmp.ln <- ln[i]
  d.tmp <- m |> filter(last_name == tmp.ln) 
  
  #--most savings
  tmp.mod1 <- t.test(d.tmp$most_savings)
  tmp.p1 <- 
    tidy(tmp.mod1) |> 
    select(estimate, p.value) %>% 
    mutate(var = "most_savings")

  #--avg savings
  tmp.mod2 <- t.test(d.tmp$avg_savings)
  tmp.p2 <- 
    tidy(tmp.mod2) |> 
    select(estimate, p.value) %>% 
    mutate(var = "avg_savings")
  
  #--least savings
  tmp.mod3 <- t.test(d.tmp$least_savings)
  tmp.p3 <- 
    tidy(tmp.mod3) |> 
    select(estimate, p.value) %>% 
    mutate(var = "least_savings")
  
  tmp.res <-
    tmp.p1 |>
    bind_rows(tmp.p2) %>% 
    bind_rows(tmp.p3) %>% 
    rename(pval = p.value) %>% 
    mutate(
      last_name = tmp.ln)
  
  res4 <- bind_rows(res4, tmp.res)
  
}

res4


res4 |> 
  write_csv("data_tidy/stats-savings.csv")


