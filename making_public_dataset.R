#----- Sixth wave -----------

setwd("C:/Users/esa/OneDrive - Eurofound/Youth survey 2023/R scripts")


load ("data/ds_analysis_esa.Rda")

names(ds)

drop <- c("SERIAL", "REF", "QUESTNNR", "F021", "F022_01")

ds_public <- ds %>%select(!drop)

save (ds_public, file = "data/lweu_r6_public.Rda")


setwd("C:/Users/esa/OneDrive - Eurofound/Living, Working and COVID-19 survey 2020-2022/Round 5/R scripts")

#---- First 5 waves -------

load("data/merged_5waves_withF555.Rda")
ds<- merge5%>%filter(wave!=4) %>%droplevels()

names(ds)

drop <- c("SERIAL", "QUESTNNR", "F021", "F022_01",  "F453",  "F555")
ds <- ds %>%select(!drop)


write.csv(ds, file = "data/lweu_1_2_3_5_public.csv")