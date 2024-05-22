setwd("C:/Users/esa/OneDrive - Eurofound/Youth survey 2023/R scripts")

library(dplyr)
library(weights)
library(questionr)
library(tidyr)
library(naniar)
library(rlist)
library(foreign)
library(haven)
library(rgdal)
library(labelled)



# -------- Import file from API --------

source("import_lweu_2023-04-24_14-36.R", local=TRUE)

save (ds, file="data/ds_raw.Rda")


# -------- Cleaning -------------------- 

source("cleaning_2023.R", local=TRUE)

ds_clean <- cleaning_2023(ds)

save(ds_clean, file="data/ds_clean.Rda")

# -------- Recoding  -------------------

load("data/ds_clean.Rda")

source("recoding_2023.R", local=TRUE)

ds_recoded <- recode_2023(ds_clean)

save(ds_recoded, file="data/ds_recoded.Rda")

# -------- Weighting ------------------


load("data/ds_recoded.Rda")
ds <- ds_recoded
ds<-as.data.frame(ds)
rm(ds_recoded)

source("weighting_2023.R", local=TRUE)

weights_2023 <- lapply(1:3, function(i) {
  
  weights <- ds %>% 
    weigh_data(minimum_weight = 0.03,
               trim_lower = 0.16,
               trim_upper = 6)
  
})

#Checking if the weights are identical in each of the three runs
stopifnot(weights_2023[[1]]$w_country == weights_2023[[2]]$w_country)
stopifnot(weights_2023[[2]]$w_country == weights_2023[[3]]$w_country)

#This selects the weights of the second run (as second and third are the same)
weights_2023 <- weights_2023[[2]]

#saving the weights
save(weights_2023, file="data/weights.rda")

#merge back in
ds$CASE<- as.character(ds$CASE)
ds<- left_join(ds, weights_2023, by=c("CASE"))

save(ds, file="data/ds_weighted.Rda")


# -------- Labelling ------------------

load("data/ds_weighted.Rda")
source("labelling_2023.R", local=TRUE)

ds_labelled <- label_2023(ds)

save(ds_labelled, file="data/ds_labelled.Rda")

write.dta(ds_labelled, "data/lweu_2023_full.dta")
write_sav(ds_labelled, "data/lweu_2023_full.sav")
write.csv(ds_labelled, "data/lweu_2023_full.csv")


#--------- Public ---------------

remove <- c("QUESTNNR", "REF", "SERIAL", "STARTED", "LASTDATA", "F021", "F022_01")
ds_public <- ds_labelled %>%select(!remove)

save(ds_public, file="data/ds_public.Rda")

write.dta(ds_public, "data/lweu_2023_public.dta")
write_sav(ds_public, "data/lweu_2023_public.sav")
write.csv(ds_public, "data/lweu_2023_public.csv")


