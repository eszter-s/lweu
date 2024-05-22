setwd("C:/Users/esa/OneDrive - Eurofound/LWEU survey/Round 6 2023/R scripts")

library(dplyr)
library(weights)
library(questionr)
library(tidyr)
library(naniar)
library(rlist)
library(foreign)
library(haven)
#library(rgdal)
library(labelled)
library(ggplot2)


load ("data/ds_analysis_esa.Rda")

day <- table(ds$QUESTNNR, ds$LASTDATA)
write.table(day, "clipboard", sep="\t", row.names=TRUE)

vars <- names(ds)
write.table(vars, "clipboard", sep="\t", row.names=FALSE)

median(ds$TIME_SUM)

# ----- Age -----------

#single age unweighed
age <-  data.frame(table(ds$B003_01))
age <- age %>% rename(
  Age = Var1
)
ageplot <- ggplot(age, aes(Age, Freq, colour="firebrick1")) + 
  geom_point()
ageplot + theme(legend.position = "none")

table(ds$B001, ds$QUESTNNR)


#single age weighted
#age <-  data.frame(wtd.table(ds$B003_01, weights=ds$w_gross_trim))
#age <- age %>% rename(
#  Age = Var1
#)
#ageplot <- ggplot(age, aes(Age, Freq, colour="firebrick1")) + 
#  geom_point()
#ageplot + theme(legend.position = "none")

table(ds$age1624)

# ----- Mental wellbeing -----------

who <- ds %>%
  group_by(youth)%>%
  summarise_at(c(
    "who5"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(who, "clipboard", sep="\t", row.names=FALSE)

negative <- ds %>%
  group_by(youth, B002, B001)%>%
  summarise_at(c(
    "tense",
    "lonely",
    "depressed",
    "riskdepr",
    "C001_01"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(negative, "clipboard", sep="\t", row.names=FALSE)

#--------- Housing insecurity ------------

insec <- ds %>%
  group_by(youth, liveswithparent)%>%
  summarise_at(c(
    "hou_insec"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(insec, "clipboard", sep="\t", row.names=FALSE)

#------ Arrears -----------

arrears <- ds %>%
  bind_rows(mutate(., B001 = "EU27"))%>%
  group_by(B001)%>%
  summarise_at(c(
    "E003_01",
    "E003_02",
    "E003_03",
    "E003_04",
    "E003_05",
    "E003_06",
    "E203_07",
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(arrears, "clipboard", sep="\t", row.names=FALSE)

lifesat <- ds %>%
  group_by(B001)  %>%
    summarise_at(c(
    "C001_01"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(lifesat, "clipboard", sep="\t", row.names=FALSE)

tgov <- ds %>%
  summarise_at(c(
    "C007_03"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(tgov, "clipboard", sep="\t", row.names=FALSE)

teu <- ds %>%
  summarise_at(c(
    "C007_04"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(teu, "clipboard", sep="\t", row.names=FALSE)

tsocmed <- ds %>%
  summarise_at(c(
    "C007_06"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(tsocmed, "clipboard", sep="\t", row.names=FALSE)

# ----- Platform work youth -----------

platform <- ds %>%
  filter(employed==1 & age1634==1) %>%
  summarise_at(c(
    "D654_01",
    "D654_02",
    "D654_03",
    "D654_04",
    "D654_05",
    "D654_06"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(platform, "clipboard", sep="\t", row.names=FALSE)

platform <- ds %>%
  filter(age1634==1) %>%
  summarise_at(c(
    "D654_01",
    "D654_02",
    "D654_03",
    "D654_04",
    "D654_05",
    "D654_06"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(platform, "clipboard", sep="\t", row.names=FALSE)


platform <- ds %>%
  filter(age1634==1) %>%
  group_by(B002) %>%
  summarise_at(c(
    "D654_01",
    "D654_02",
    "D654_03",
    "D654_04",
    "D654_05",
    "D654_06"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(platform, "clipboard", sep="\t", row.names=FALSE)



platform <- ds %>%
  filter((age1634==1)&((D654_05==1)|(D654_06==1))&(D655_01<3)) %>%
  summarise_at(c(
    "employed",
    "selfemp"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(platform, "clipboard", sep="\t", row.names=FALSE)





# ----- Youth

young <- table(ds$B001,ds$agecat)
write.table(young, "clipboard", sep="\t", row.names=FALSE)


youth <- ds %>%
  summarise_at(c(
    "H613_01",
    "H613_02",
    "H613_03",
    "H613_04",
    "H613_05",
    "H613_06",
    "H613_07",
    "H613_08",
    "H613_09",
    "H613_10",
    "H613_11",
    "H613_12",
    "H614_01",
    "H614_02",
    "H614_03",
    "H614_04",
    "H614_05",
    "H614_06",
    "H614_07",
    "H614_08",
    "H614_09",
    "H614_10",
    "H614_11",
    "H614_12",
    "H615_01",
    "H615_02",
    "H615_03",
    "H615_04",
    "H615_05",
    "H615_06",
    "H615_07",
    "H615_08",
    "H615_09",
    "H615_10",
    "H615_11",
    "H615_12",
    "H616_01",
    "H616_02",
    "H616_03",
    "H616_04",
    "H616_05",
    "H616_06",
    "H616_07",
    "H616_08",
    "H616_09",
    "H616_10",
    "H616_11",
    "H616_12"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(youth, "clipboard", sep="\t", row.names=FALSE)

# ----- Weighted/unweighted comparisons -----------

unweighted <- ds %>%
  filter(QUESTNNR=="EF_2023_panel") %>%
  summarise_at(c(
    "male",
    "age1634",
    "age3549",
    "age5064",
    "age65",
    "tertiary",
    "employed",
    "urban",
    "badhealth",
    "disab",
    "who5",
    "riskdepr",
    "C001_01",
    "optimistic",
    "C312_01",
    "C007_04",
    "voted",
    "D211_01",
    "mem_diff2",
    "homeowner",
    "E003_02",
    "depr_goingout",
    "F662_01",
    "F662_05"
  ), ~ mean(.x, na.rm=TRUE))
write.table(unweighted, "clipboard", sep="\t", row.names=FALSE)

weighted <- ds %>%
  filter(QUESTNNR=="EF_2023_panel") %>%
  summarise_at(c(
    "male",
    "age1634",
    "age3549",
    "age5064",
    "age65",
    "tertiary",
    "employed",
    "urban",
    "badhealth",
    "disab",
    "who5",
    "riskdepr",
    "C001_01",
    "optimistic",
    "C312_01",
    "C007_04",
    "voted",
    "D211_01",
    "mem_diff2",
    "homeowner",
    "E003_02",
    "depr_goingout",
    "F662_01",
    "F662_05"
      ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(weighted, "clipboard", sep="\t", row.names=FALSE)

table(ds$lonepar, ds$B002)


enpov <- ds %>%
  group_by(B002, lonepar)%>%
  summarise_at(c(
    "E003_02",
    "utility_enpov",
    "car_enpov"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(enpov, "clipboard", sep="\t", row.names=FALSE)

#-------------- Youth q - Employment chapter -----

youth <- ds %>%
  summarise_at(c(
    "H613_01",
    "H613_02",
    "H613_03",
    "H613_04",
    "H613_05",
    "H614_01",
    "H614_02",
    "H614_03",
    "H614_04",
    "H614_05",
    "H615_01",
    "H615_02",
    "H615_03",
    "H615_04",
    "H615_05",
    "H616_01",
    "H616_02",
    "H616_03",
    "H616_04",
    "H616_05"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(youth, "clipboard", sep="\t", row.names=FALSE)

gapemp <- ds%>%
  filter (gapemp==1)%>%
  summarise_at(c(
    "student")
    , ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))

changejob <- ds%>%
  group_by (H613_02)%>%
  summarise_at(c(
    "permcont",
    "D210_01",
    "teleworkable")
    , ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(changejob, "clipboard", sep="\t", row.names=FALSE)

changejob <- ds%>%
  group_by (B002)%>%
  summarise_at(c(
    "H613_02")
    , ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(changejob, "clipboard", sep="\t", row.names=FALSE)

business <- ds%>%
  group_by (B002)%>%
  summarise_at(c(
    "H613_03",
    "H614_03",
    "H615_03",
    "H616_03")
    , ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(business, "clipboard", sep="\t", row.names=FALSE)


#------- Youth q - housing chapter ------

youth <- ds %>%
  summarise_at(c(
    "H613_06",
    "H613_07",
    "H613_09",
    "H614_06",
    "H614_07",
    "H614_09",
    "H615_06",
    "H615_07",
    "H615_09",
    "H616_06",
    "H616_07",
    "H616_09"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(youth, "clipboard", sep="\t", row.names=FALSE)



# ----------------------- FOR PUBLIC VOICE -------------------------

setwd("C:/Users/esa/OneDrive - Eurofound/Youth survey 2023/R scripts")

load ("data/ds_analysis_esa.Rda")
ds <- ds %>% filter ((B001=="Czechia")|(B001=="France")|
                       (B001=="Germany")|(B001=="Greece")|
                       (B001=="Ireland")|(B001=="Poland")) %>% droplevels()
                       

weighted <- ds %>%
  group_by(B001)  %>%
  summarise_at(c(
    "C001_01",
    "C209",
    "C007_04",
    "C004_01"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(weighted, "clipboard", sep="\t", row.names=FALSE)

#unweighted ls
unweighted <- ds %>%
  group_by(B001)  %>%
  summarise_at(c(
    "C001_01",
    "C209",
    "C007_04",
    "C004_01"
  ), ~ mean(.x, na.rm=TRUE))
write.table(unweighted, "clipboard", sep="\t", row.names=FALSE)

#--------------- Training check ----------------

table(ds$D544)
table(ds$D545_01)
table(ds$D545_02)
table(ds$D545_03)
table(ds$D658)
table(ds$D543)
table(ds$D647)

#------------- Arrears check ---------------

setwd("C:/Users/stsac/OneDrive - Eurofound/LWEU survey/Round 6 2023/R scripts")

library(dplyr)
library(weights)
library(questionr)
library(tidyr)
library(naniar)
library(rlist)
library(foreign)
library(haven)
library(labelled)
library(ggplot2)
library(margins)
library(survey)

#wave 1 to 5 dataset
load ("data/lwc_r1_r2_r3_r5_public_140922.Rda")
w15 <-ds
rm(ds)

load ("data/ds_analysis_esa.Rda")

arrears <- ds %>%
  bind_rows(mutate(., B001 = "EU27"))%>%
  group_by(B001)%>%
  summarise_at(c(
    "E003_01",
    "E003_02",
    "E003_03",
    "E003_04",
    "E003_05",
    "E003_06",
    "E203_07"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(arrears, "clipboard", sep="\t", row.names=FALSE)

w15$B001 <- as.character(w15$B001)
arrears <- w15 %>%
  bind_rows(mutate(., B001 = "EU27"))%>%
  group_by(wave, B001)%>%
  summarise_at(c(
    "E003_01",
    "E003_02",
    "E003_03",
    "E003_04",
    "E003_05",
    "E003_06",
    "E203_07"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(arrears, "clipboard", sep="\t", row.names=FALSE)


insec <- ds %>%
  bind_rows(mutate(., B001 = "EU27"))%>%
  group_by(B001)%>%
  summarise_at(c(
    "hou_insec"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(insec, "clipboard", sep="\t", row.names=FALSE)

insec <- w15 %>%
  bind_rows(mutate(., B001 = "EU27"))%>%
  group_by(wave, B001)%>%
  summarise_at(c(
    "hou_insec"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(insec, "clipboard", sep="\t", row.names=FALSE)

names(ds)


uk <- wtd.table(ds$B001, ds$G502_01, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$B001, ds$G502_02, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)


uk <- wtd.table(ds$B001, ds$G502_03, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$B001, ds$G602_04, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$B001, ds$G503_01, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$B001, ds$G503_02, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$B001, ds$G503_03, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$B001, ds$G503_04, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

table(ds$G503_01)
uk <- ds %>%
  group_by(B001)%>%
  summarise_at(c(
    "G601_01"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(uk, "clipboard", sep="\t", row.names=FALSE)

uk <- ds %>%
  group_by(trusteu)%>%
  summarise_at(c(
    "G601_01"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(uk, "clipboard", sep="\t", row.names=FALSE)

uk <- wtd.table(ds$trustgov, ds$G503_01, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$trusteu, ds$G503_01, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$G503_01, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$trustgov, ds$G503_02, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$trusteu, ds$G503_02, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$G503_02, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$trustgov, ds$G503_03, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$trusteu, ds$G503_03, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$G503_03, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)


uk <- wtd.table(ds$trustgov, ds$G503_04, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$trusteu, ds$G503_04, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

uk <- wtd.table(ds$G503_04, weights = ds$w_gross_trim,na.rm = TRUE)
write.table(uk, "clipboard", sep="\t", row.names=TRUE)

#----------- Traineeship ---------------

train <- ds %>%
  group_by(F004)%>%
  summarise_at(c(
    "D664_01",
    "D666"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(train, "clipboard", sep="\t", row.names=FALSE)

#------ Working hours ---------

wh <- ds %>%
  group_by(B001)%>%
  summarise_at(c(
    "workinghours"
  ), ~ weighted.mean(.x, w_gross_trim, na.rm=TRUE))
write.table(wh, "clipboard", sep="\t", row.names=FALSE)



