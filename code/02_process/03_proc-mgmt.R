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
  
  f.data <- read_csv(paste0("data_stefan/", coops[i], ".ReduceN.MGMT.csv")) 
  
  tmp.data <- 
    f.data |> 
    clean_names() |> 
    mutate_if(is.character, str_to_lower) |> 
    mutate(last_name = coops[i],
           date_ymd = mdy(date_yyyy_mm_dd)) %>% 
    select(-cooperator, - project_title, -date_yyyy_mm_dd)
  
  return(tmp.data)
  
}


# 1.amundson ----------------------------------------------------------------

d1 <- 
  CleanData(i = 1) 


# 2. anderson ----------------------------------------------------------------------

d2 <- CleanData(i = 2) 

# 3. bakehouse----------------------------------------------------------------------

d3 <- CleanData(i = 3) 

# 4. bardole----------------------------------------------------------------------

d4 <- CleanData(i =4) 

# 5. bennett----------------------------------------------------------------------

d5 <- CleanData(i = 5) 

#6.  borchardt----------------------------------------------------------------------

d6 <- CleanData(i = 6) 

# 7. boyer----------------------------------------------------------------------

d7 <- CleanData(i = 7) 

# 8. deal----------------------------------------------------------------------

d8 <- CleanData(i = 8) 

# 9. dooley----------------------------------------------------------------------

d9 <- CleanData(i = 9) 

# 10. frederick----------------------------------------------------------------------

d10 <-  CleanData(i = 10) 

# 11. fredericks----------------------------------------------------------------------

d11 <-  CleanData(i = 11) 

# 12. harvey----------------------------------------------------------------------

d12 <-  CleanData(i = 12)

# 13. prevo----------------------------------------------------------------------

d13 <- CleanData(i = 13)

# 14.  sieren----------------------------------------------------------------------

d14 <- CleanData(i = 14) 

# 15. veenstra 1 and 2---------------------------------------------------------------------
#--veenstra did 2 fields

coops[15] <- "Veenstra1"
coops[17] <- "Veenstra2"


d15 <- CleanData(i = 15)
d17 <- CleanData(i = 17) 

# 16. waldo---------------------------------------------------------------------
#--there was no rep 6, but there was 7 and 8

d16 <- CleanData(i = 16) 

# combine -----------------------------------------------------------------

d_all <- NULL

for (i in 1:length(coops)){
  
  d.tmp <- eval(parse(text = paste0("d", i)))
  
  d_all <- 
    d_all |> 
    bind_rows(d.tmp)
  
}

d_all |> 
  filter(last_name == "Fredericks")

d_all2 <- 
  d_all %>% 
  select(last_name, activity_measurement, date_ymd, notes)

d_all2 |> 
  write_csv("data_tidy/mgmt.csv")
