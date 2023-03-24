library(googlesheets4)
library(googledrive)
library(dplyr)
library(tidyr)
library(stringr)

#--using a direct link works
read_sheet("https://docs.google.com/spreadsheets/d/1d1j28pilB_fKkBpC4A-zKpiYfCNWBRcwpKQe7ZD5NMY")

#--but I want to automate this
a <- drive_ls("https://drive.google.com/drive/folders/1PdscOkdv45f5B8TLuum1aPR3tOAOdcS0/")

a1 <- 
  a |> 
  filter(name == "Cooperator info & data") 

#--cooperators list
b <- drive_ls(a1$id)

#--filter out the xlsx, simplify names
b1 <- 
  b |> 
  filter(!grepl("xlsx", name)) |> #
  #--keep only 3 for now
  slice(1:3) |> 
  mutate(name = str_remove_all(pattern = "[^[:alnum:]]", name))

dat <- NULL

for (i in 1:nrow(b1)){
  
  i <- 3
  
  who.tmp <- 
    b1 |> 
    slice(i)
  
  nm.tmp <- who.tmp |> pull(name)
  
  dat.tmp <- 
    ReadCornYield(name_slice = who.tmp) |> 
    mutate(coop_name = nm.tmp)

  dat <- 
    bind_rows(dat, dat.tmp)
  
}

ReadCornYield <- function(name_slice = who.tmp) {
  
  #--inside single coop folder
  t1 <- 
    drive_ls(name_slice$id) |> 
    mutate(name = str_to_lower(name)) |> 
    filter(name == "response data")
  
  #--inside rseponse data folder
  t2 <- 
    drive_ls(t1$id) |> 
    mutate(name = str_to_lower(name)) |> 
    filter(grepl("yield", name))
  
  #--yield, final data
  t3 <- 
    drive_ls(t2$id) |> 
    mutate(name = str_to_lower(name)) |> 
    filter(grepl("final", name))
  
  #--this package doesn't like the csv for some reason
  #UGHGHGHGH these file names and formats are not standardized
  # three differences in first three names....
  t4 <- 
    drive_ls(t3$id) |> 
    filter(grepl("yield.fin", name)) #|>
    #filter(!grepl("csv", name)) 
  
  dat <- 
    read_sheet(t4$id) |> 
    janitor::clean_names()
  
  return(dat)
}

b1 <- 
  b |> 
  slice(1) |> pull(id)

#--inside single coop folder
c <- drive_ls(paste0("https://drive.google.com/drive/folders/", b1, "/"))

c1 <- 
  c |> 
  filter(name == "Response Data") |> 
  pull(id)

#--inside rseponse data folder
d <- drive_ls(paste0("https://drive.google.com/drive/folders/", c1, "/"))

#--corn yield
d1 <- 
  d |> 
  filter(name == "Corn yield") |> 
  pull(id)

#--field history
d2 <- 
  d |> 
  filter(name == "Field history") |> 
  pull(id)

#--corn yield
e <- drive_ls(paste0("https://drive.google.com/drive/folders/", d1, "/"))

e1 <- 
  e |> 
  filter(name == "3.Final") |> 
  pull(id)

f <- drive_ls(paste0("https://drive.google.com/drive/folders/", e1, "/"))


f1 <- 
  f |> 
  filter(grepl("yield.final", name)) |>
  filter(!grepl("csv", name)) 

read_sheet(as_id(f1$id))

#--field history
g <- drive_ls(paste0("https://drive.google.com/drive/folders/", d2, "/"))

g1 <- 
  g |> 
  filter(name == "3.Final")

f <- drive_ls(g1$id)


f1 <- 
  f |> 
  filter(grepl("yield.final", name)) |>
  filter(!grepl("csv", name)) 

read_sheet(as_id(f1$id))

           