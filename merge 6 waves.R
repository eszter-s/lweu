#----- Sixth wave -----------

setwd("C:/Users/esa/OneDrive - Eurofound/LWEU survey/Round 6 2023/R scripts/")

library(dplyr)
library(weights)
library(questionr)
library(tidyr)
library(naniar)
library(rlist)
library(foreign)
library(haven)

load("data/ds_analysis_esa.Rda")

names(ds)

drop <- c("population","cheerful","calm","active","rested","interested","male","agecat",
          "agecat2","youth","youngcat","age1634","age3549","age5064","age65","age1624",
          "tertiary","optimistic","lonely","tense","depressed","riskdepr","disab","badhealth",
          "urbanisation","urban","socex","selfemp","employed","unemployed","inactive",
          "student","ecostat","permcont","tempcont","agencycont","trainingcont",
          "nocont","lostjob","job_insecure","telework", "wfh","teleworkable","tr_app",
          "voted","mem_diff2","mem_diff3","homeowner","renter","hou_insec","depr_goingout",
          "utility_enpov","car_enpov","lonepar","haspartner","haschild","liveswithparent",
          "ownhomenopar","rentnopar","youthhousing","income","incomecat","quintile",
          "gap_emp_1y","gap_change_1y","gap_business_1y","gap_uni_1y","gap_trai_1y",
          "gap_move_1y","gap_moveout_1y","gap_country_1y","gap_buy_1y","gap_marry_1y", 
          "gap_partner_1y","gap_child_1y","gap_child_3y")

ds <- ds %>% select((!drop))



#---- First 5 waves -------

setwd("C:/Users/esa/OneDrive - Eurofound/Living, Working and COVID-19 survey 2020-2022/Round 5/R scripts")


load("data/merged_5waves_withF555.Rda")
merge5<- merge5%>%filter(wave!=4) %>%droplevels()

names(merge5)

drop <- c("lastdate", "id")
merge5 <- merge5 %>%select(!drop)


#-------------- MERGE --------


ds$wave <- 6
ds$B001 <-as.numeric(ds$B001)
merge5$email_id <- case_when(
  merge5$wave<5 ~ merge5$F021,
  merge5$wave==5 ~ merge5$F555
)
merge5$F021<- merge5$email_id
drop <- c("email_id")
merge5 <- merge5 %>%select(!drop)


ds <- ds %>%  
  mutate(CASE = as.numeric(CASE)) %>%  
  mutate(SERIAL = as.character(SERIAL)) %>%
  mutate(QUESTNNR = as.character(QUESTNNR)) %>%
  mutate(LASTDATA = as.character(LASTDATA)) %>%
  mutate(F021 = as.character(F021))

#recode CASE as there are duplicates
ds$CASE <- ds$CASE+6000000

tab <- sapply(merge5, class)
write.table(tab, "clipboard", sep="\t", row.names=TRUE)

tab <- sapply(ds, class)
write.table(tab, "clipboard", sep="\t", row.names=TRUE)

ds <- ds %>%  
  mutate(LANGUAGE = as.character(LANGUAGE)) %>%  
  mutate(STARTED = as.character(STARTED)) %>%
  mutate(TIME_SUM = as.numeric(TIME_SUM)) %>%
  mutate(FINISHED = as.numeric(FINISHED)) %>%
  mutate(F004 = as.character(F004)) %>%
  mutate(D210_01 = as.numeric(D210_01)) %>%
  mutate(D210_02 = as.numeric(D210_02)) %>%
  mutate(D210_03 = as.numeric(D210_03)) %>%
  mutate(D210_04 = as.numeric(D210_04)) %>%
  mutate(F346_01 = as.numeric(F346_01)) %>%
  mutate(F346_02 = as.numeric(F346_02)) %>%
  mutate(F346_03 = as.numeric(F346_03)) %>%
  mutate(F346_04 = as.numeric(F346_04)) %>%
  mutate(F346_05 = as.numeric(F346_05)) %>%
  mutate(F346_06 = as.numeric(F346_06)) %>%
  mutate(F346_07 = as.numeric(F346_07)) %>%
  mutate(F346_08 = as.numeric(F346_08)) %>%
  mutate(H411_01 = as.numeric(H411_01)) %>%
  mutate(H411_02 = as.numeric(H411_02)) %>%
  mutate(H411_03 = as.numeric(H411_03)) %>%
  mutate(H411_04 = as.numeric(H411_04)) %>%
  mutate(C530 = as.factor(C530)) %>%
  mutate(D542 = as.factor(D542)) %>%
  mutate(F556 = as.factor(F556))
  
ds$D008_01 <- ds$D008
ds$C209_01 <- ds$C209

drop <- c("D008", "C209")
ds <- ds %>%select(!drop)





matchColClasses <- function(df1, df2) {
  
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  return(df2)
}

ds6_matched <- matchColClasses(merge5, ds)




m6 <- merge5 %>% full_join(ds6_matched, by = intersect(names(merge5), names(ds6_matched)))

names <-names(m6)
write.table(names, "clipboard", sep="\t", row.names=FALSE)

#checks
ds <- m6
rm(ds6_matched, merge5, m6, tab)

table(ds$B001)
ds$B001 = factor(ds$B001, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27"), 
                 labels=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden"), 
                 ordered=FALSE)



ds$agecat <- cut(ds$B003_01,
                 breaks=c(-Inf, 34, 54, Inf),
                 labels=c("18-34","35-54","55+"))
ds$gender <- NA
ds$gender[ds$B002==1] <- "Men"
ds$gender[ds$B002==2] <- "Women"


ds$cheerful<- NA
ds$cheerful[ds$C005_01==6] <- 5
ds$cheerful[ds$C005_01==5] <- 4
ds$cheerful[ds$C005_01==4] <- 3
ds$cheerful[ds$C005_01==3] <- 2
ds$cheerful[ds$C005_01==2] <- 1
ds$cheerful[ds$C005_01==1] <- 0

ds$calm<- NA
ds$calm[ds$C005_02==6] <- 5
ds$calm[ds$C005_02==5] <- 4
ds$calm[ds$C005_02==4] <- 3
ds$calm[ds$C005_02==3] <- 2
ds$calm[ds$C005_02==2] <- 1
ds$calm[ds$C005_02==1] <- 0

ds$active<- NA
ds$active[ds$C005_03==6] <- 5
ds$active[ds$C005_03==5] <- 4
ds$active[ds$C005_03==4] <- 3
ds$active[ds$C005_03==3] <- 2
ds$active[ds$C005_03==2] <- 1
ds$active[ds$C005_03==1] <- 0

ds$rested<- NA
ds$rested[ds$C005_04==6] <- 5
ds$rested[ds$C005_04==5] <- 4
ds$rested[ds$C005_04==4] <- 3
ds$rested[ds$C005_04==3] <- 2
ds$rested[ds$C005_04==2] <- 1
ds$rested[ds$C005_04==1] <- 0

ds$interested<- NA
ds$interested[ds$C005_05==6] <- 5
ds$interested[ds$C005_05==5] <- 4
ds$interested[ds$C005_05==4] <- 3
ds$interested[ds$C005_05==3] <- 2
ds$interested[ds$C005_05==2] <- 1
ds$interested[ds$C005_05==1] <- 0

ds$who5 = apply(ds[562:566],1,sum)*4


setwd("C:/Users/esa/OneDrive - Eurofound/LWEU survey/Round 6 2023/R scripts/")




save(ds, file="data/6waves_full.Rda")



