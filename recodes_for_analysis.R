setwd("C:/Users/stsac/OneDrive - Eurofound/LWEU survey/Round 6 2023/R scripts")

library(dplyr)
library(weights)
library(questionr)
library(tidyr)
library(naniar)
library(rlist)
library(foreign)
library(haven)
#library(rgdal)
library(foreign)
library(haven)



load ("data/ds_labelled.Rda")
ds <- ds_labelled
rm(ds_labelled)


#who5 - first to do as it uses the col position

#who-5
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

ds$who5 = apply(ds[240:244],1,sum)*4



#country
ds <- ds %>% filter (B001<28) %>% droplevels()

ds$B001 = factor(ds$B001, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27"), labels=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden"), ordered=FALSE)

#gender

ds$male <- NA
ds$male[ds$B002==1] <- 1
ds$male[ds$B002==2] <- 0

#age

ds$agecat <- cut(ds$B003_01,
                 breaks=c(-Inf, 34, 49, 64, Inf),
                 labels=c("16-34","35-49","50-64","65+"))

ds$agecat2 <- cut(ds$B003_01,
                 breaks=c(-Inf, 29, 39, 49, 64, Inf),
                 labels=c("16-29","30-39","40-49","50-64", "65+"))

ds$youth <- cut(ds$B003_01,
                breaks=c(-Inf, 29, Inf),
                labels=c("16-29","30+"))

ds$youngcat <- cut(ds$B003_01,
                breaks=c(-Inf, 24, 34, Inf),
                labels=c("16-24","25-34", "35+" ))

ds$age1634 <- 0
ds$age1634[ds$agecat=="16-34"] <- 1

ds$age3549 <- 0
ds$age3549[ds$agecat=="35-49"] <- 1

ds$age5064 <- 0
ds$age5064[ds$agecat=="50-64"] <- 1

ds$age65 <- 0
ds$age65[ds$agecat=="65+"] <- 1

ds$age1624 <- 0
ds$age1624[ds$B003_01<25] <- 1


#education

ds$tertiary <- NA
ds$tertiary[ds$F004==1] <- 0
ds$tertiary[ds$F004==2] <- 0
ds$tertiary[ds$F004==3] <- 1

#optimism etc

ds$optimistic <- case_when(
  ds$C003_01==1 ~1,
  ds$C003_01==2 ~1,
  ds$C003_01==3 ~0,
  ds$C003_01==4 ~0,
  ds$C003_01==5 ~0)


#mwb
ds$lonely <- case_when(
  ds$C006_02==1 ~1,
  ds$C006_02==2 ~1,
  ds$C006_02==3 ~1,
  ds$C006_02==4 ~0,
  ds$C006_02==5 ~0,
  ds$C006_02==6 ~0)

ds$tense <- case_when(
  ds$C006_01==1 ~1,
  ds$C006_01==2 ~1,
  ds$C006_01==3 ~1,
  ds$C006_01==4 ~0,
  ds$C006_01==5 ~0,
  ds$C006_01==6 ~0)

ds$depressed <- case_when(
  ds$C006_03==1 ~1,
  ds$C006_03==2 ~1,
  ds$C006_03==3 ~1,
  ds$C006_03==4 ~0,
  ds$C006_03==5 ~0,
  ds$C006_03==6 ~0)

ds$riskdepr <- NA
ds$riskdepr <- case_when(
  ds$who5<=50 ~ 1,
  ds$who5>50 ~ 0)

#disability and health
ds$disab <- NA
ds$disab[ds$C310==0] <-0
ds$disab[ds$C311==1] <-1
ds$disab[ds$C311==2] <-1
ds$disab[ds$C311==3] <-0


ds$badhealth <- case_when(
  ds$C004_01==1 ~0,
  ds$C004_01==2 ~0,
  ds$C004_01==3 ~0,
  ds$C004_01==4 ~1,
  ds$C004_01==5 ~1)



#urbanisation
ds$urbanisation <- case_when(
  ds$C008== 1 ~ 1,
  ds$C008== 2 ~ 1,
  ds$C008== 3 ~ 2,
  ds$C008== 4 ~ 2)

#urbanisation
ds$urban <- case_when(
  ds$C008== 1 ~ 0,
  ds$C008== 2 ~ 0,
  ds$C008== 3 ~ 1,
  ds$C008== 4 ~ 1)


# Social exclusion
ds$socex <- case_when(
  ds$C203_05== 1 ~ 1,
  ds$C203_05== 2 ~ 1,
  ds$C203_05== 3 ~ 0,
  ds$C203_05== 4 ~ 0,
  ds$C203_05== 5 ~ 0)

table(ds$C203_05)

#employment, contracts

#Employment
ds$selfemp[ds$D001==1]<- 0
ds$selfemp[ds$D001==2]<- 1
ds$selfemp[ds$D001==3]<- 1
ds$selfemp[ds$D001>3]<- 0

ds$employed <- NA
ds$employed[ds$D001<4] <- 1
ds$employed[ds$D001>=4] <- 0

ds$unemployed <- NA
ds$unemployed[ds$D001==4] <- 1
ds$unemployed[(ds$D001<4)|(ds$D001>4)] <- 0

ds$inactive <- NA
ds$inactive[ds$D001<=4] <- 0
ds$inactive[ds$D001>4] <- 1

ds$student <- NA
ds$student[ds$D001==8] <- 1
ds$student[(ds$D001<8)] <- 0

ds$ecostat <- NA
ds$ecostat[ds$D001<4] <- "employed"
ds$ecostat[ds$D001==4] <- "unemployed"
ds$ecostat[ds$D001>4] <- "other inactive"
ds$ecostat[ds$D001==8] <- "student"
ds$ecostat <- as.factor(ds$ecostat)


ds$permcont <- NA
ds$permcont[ds$D209==1] <- 1
ds$permcont[ds$D209>1] <- 0

ds$tempcont <- NA
ds$tempcont[ds$D209==1] <- 0
ds$tempcont[ds$D209==2] <- 1
ds$tempcont[ds$D209>2] <- 0

ds$agencycont <- NA
ds$agencycont[ds$D209<3] <- 0
ds$agencycont[ds$D209==3] <- 1
ds$agencycont[ds$D209>3] <- 0

ds$trainingcont <- NA
ds$trainingcont[ds$D209<4] <- 0
ds$trainingcont[ds$D209==4] <- 1
ds$trainingcont[ds$D209==5] <- 0

ds$nocont <- NA
ds$nocont[ds$D209==5] <- 1
ds$nocont[ds$D209<5] <- 0

#Lost job in past year
ds$lostjob <- NA
ds$lostjob <-case_when(
  ((ds$D235 == 1|ds$D235 ==2|ds$D235 ==3) &
     (ds$D001 == 1|ds$D001 ==2|ds$D001 ==3
      |ds$D001 ==5|ds$D001 ==6|ds$D001 ==7|ds$D001 ==8
      |ds$D001 ==9)) ~ 0,
  ((ds$D235 == 1|ds$D235 ==2|ds$D235 ==3) &
     (ds$D001 == 4))  ~ 1
)

ds$lostjob[ds$D002==1]<- 1
ds$lostjob[ds$D002==2]<- 0
ds$lostjob[ds$D002==3]<- 0

#job insecurity

ds$job_insecure <- NA
ds$job_insecure <-case_when(
  ds$D008 == 5 ~ 0,
  ds$D008 == 4 ~ 0,
  ds$D008 == 3 ~ 0,
  ds$D008 == 2 ~ 1,
  ds$D008 == 1 ~ 1)


ds$workinghours <- ds$D211_01
ds$workinghours[ds$D211_01==0] <- NA


#table(ds$wave, ds$D210_01)

ds$telework <- case_when(
  (ds$D210_01==1 & ds$D210_02==0 & ds$D210_03==0 & ds$D210_04==0) ~"telework",
  (ds$D210_01==0 & (ds$D210_02==1 | ds$D210_03==1 | ds$D210_04==1)) ~"workplace",
  (ds$D210_01==1 & (ds$D210_02==1 | ds$D210_03==1 | ds$D210_04==1)) ~"combined",
  (ds$D210_01==0 & ds$D210_02==0 & ds$D210_03==0 & ds$D210_04==0) ~ "nowhere"
)
ds$telework <- as.factor(ds$telework)


ds$wfh <- NA
ds$wfh[ds$D210_01==1] <- 1
ds$wfh[ds$D210_01==0] <- 0


ds$teleworkable[ds$D542<3] <- 1
ds$teleworkable[ds$D542==3] <- 0

#traineeships

ds$tr_app <- NA
ds$tr_app[ds$D662_01==1] <-1
ds$tr_app[ds$D662_02==1] <-1
ds$tr_app[(ds$D662_01==0)&(ds$D662_02==0)] <-0

#voted

ds$voted <- case_when(
  ds$C530==1 ~1,
  ds$C530==0 ~0)

#make ends meet

ds$mem_diff2 <- case_when(
  ds$E001_01==1 ~1,
  ds$E001_01==2 ~1,
  ds$E001_01==3 ~0,
  ds$E001_01==4 ~0,
  ds$E001_01==5 ~0,
  ds$E001_01==6 ~0)

ds$mem_diff3 <- case_when(
  ds$E001_01==1 ~1,
  ds$E001_01==2 ~1,
  ds$E001_01==3 ~1,
  ds$E001_01==4 ~0,
  ds$E001_01==5 ~0,
  ds$E001_01==6 ~0)

#home ownership

ds$homeowner <- case_when(
  ds$E208==1 ~1,
  ds$E208==2 ~1,
  ds$E208==3 ~0,
  ds$E208==4 ~0,
  ds$E208==5 ~0)

#renter

ds$renter <- case_when(
  ds$E208==1 ~0,
  ds$E208==2 ~0,
  ds$E208==3 ~1,
  ds$E208==4 ~1,
  ds$E208==5 ~0)

#housing insecurity

ds$hou_insec <- NA
ds$hou_insec <-case_when(
  ds$E007_01 == 5 ~ 0,
  ds$E007_01 == 4 ~ 0,
  ds$E007_01 == 3 ~ 0,
  ds$E007_01 == 2 ~ 1,
  ds$E007_01 == 1 ~ 1)

#deprivation

ds$depr_goingout <- case_when(
  ds$E627_05==1 ~0,
  ds$E627_05==0 ~1)

#energy poverty

ds$utility_enpov <- case_when(
  ds$E525_01==1 ~1,
  ds$E525_01==2 ~1,
  ds$E525_01==3 ~0,
  ds$E525_01==4 ~0,
  ds$E525_01==5 ~0)

ds$car_enpov <- case_when(
  ds$E526_01==1 ~1,
  ds$E526_01==2 ~1,
  ds$E526_01==3 ~0,
  ds$E526_01==4 ~0,
  ds$E526_01==5 ~0)

#households-families

#lone parents

ds$lonepar <- 0
ds$lonepar[(ds$H308==1)&((ds$H002==0)|is.na(ds$H002))]<- 1

#has partner in hh

ds$haspartner <- NA
ds$haspartner[(ds$H002==0)|is.na(ds$H002)] <- 0
ds$haspartner[ds$H002==1] <- 1

#has own child in hh

ds$haschild <- NA
ds$haschild[(ds$H308==0)|is.na(ds$H308)] <- 0
ds$haschild[ds$H308==1] <- 1

#lives with parent

ds$liveswithparent <- NA
ds$liveswithparent[(ds$H203==0)|is.na(ds$H203)] <- 0
ds$liveswithparent[ds$H203==1] <- 1

#homeowner not parent

ds$ownhomenopar <- NA
ds$ownhomenopar[ds$liveswithparent==1] <- 0
ds$ownhomenopar[ds$homeowner==0] <- 0
ds$ownhomenopar[(ds$liveswithparent==0)&(ds$homeowner==1)] <- 1

#renter no parent

ds$rentnopar <- NA
ds$rentnopar[ds$liveswithparent==1] <- 0
ds$rentnopar[ds$renter==0] <- 0
ds$rentnopar[(ds$liveswithparent==0)&(ds$renter==1)] <- 1

#housing categories for youth 

ds$youthhousing <- NA
ds$youthhousing [ds$liveswithparent==1] <- "Lives with parent"
ds$youthhousing[(ds$liveswithparent==0)&(ds$rentnopar==1)] <- "Renting"
ds$youthhousing[(ds$liveswithparent==0)&(ds$ownhomenopar==1)] <- "Homeowner"
ds$youthhousing[(ds$liveswithparent==0)&(ds$rentnopar==0)&(ds$ownhomenopar==0)] <- "Other"
ds$youthhousing <- as.factor(ds$youthhousing)


#------------ Income ------------------- #

table(ds$E410)
ds$income <- NA
ds$income[ds$E410==1] <-1
ds$income[ds$E410==2] <-2
ds$income[ds$E410==3] <-3
ds$income[ds$E410==4] <-4
ds$income[ds$E410==5] <-5
ds$income[ds$E410==6] <-6
ds$income[ds$E410==7] <-7
ds$income[ds$E410==8] <-8
ds$income[ds$E410==9] <-9
ds$income[ds$E410==10] <-10
ds$income[ds$E410==11] <-1
ds$income[ds$E410==12] <-2
ds$income[ds$E410==13] <-3
ds$income[ds$E410==14] <-4
ds$income[ds$E410==15] <-5
ds$income[ds$E410==16] <-6
ds$income[ds$E410==17] <-7
ds$income[ds$E410==18] <-8
ds$income[ds$E410==19] <-9
ds$income[ds$E410==20] <-10
ds$income[ds$E410==21] <-1
ds$income[ds$E410==22] <-2
ds$income[ds$E410==23] <-3
ds$income[ds$E410==24] <-4
ds$income[ds$E410==25] <-5
ds$income[ds$E410==26] <-6
ds$income[ds$E410==27] <-7
ds$income[ds$E410==28] <-8
ds$income[ds$E410==29] <-9
ds$income[ds$E410==30] <-10
ds$income[ds$E410==31] <-1
ds$income[ds$E410==32] <-2
ds$income[ds$E410==33] <-3
ds$income[ds$E410==34] <-4
ds$income[ds$E410==35] <-5
ds$income[ds$E410==36] <-6
ds$income[ds$E410==37] <-7
ds$income[ds$E410==38] <-8
ds$income[ds$E410==39] <-9
ds$income[ds$E410==40] <-10
ds$income[ds$E410==41] <-1
ds$income[ds$E410==42] <-2
ds$income[ds$E410==43] <-3
ds$income[ds$E410==44] <-4
ds$income[ds$E410==45] <-5
ds$income[ds$E410==46] <-6
ds$income[ds$E410==47] <-7
ds$income[ds$E410==48] <-8
ds$income[ds$E410==49] <-9
ds$income[ds$E410==50] <-10
ds$income[ds$E410==51] <-1
ds$income[ds$E410==52] <-2
ds$income[ds$E410==53] <-3
ds$income[ds$E410==54] <-4
ds$income[ds$E410==55] <-5
ds$income[ds$E410==56] <-6
ds$income[ds$E410==57] <-7
ds$income[ds$E410==58] <-8
ds$income[ds$E410==59] <-9
ds$income[ds$E410==60] <-10
ds$income[ds$E410==61] <-1
ds$income[ds$E410==62] <-2
ds$income[ds$E410==63] <-3
ds$income[ds$E410==64] <-4
ds$income[ds$E410==65] <-5
ds$income[ds$E410==66] <-6
ds$income[ds$E410==67] <-7
ds$income[ds$E410==68] <-8
ds$income[ds$E410==69] <-9
ds$income[ds$E410==70] <-10
ds$income[ds$E410==71] <-1
ds$income[ds$E410==72] <-2
ds$income[ds$E410==73] <-3
ds$income[ds$E410==74] <-4
ds$income[ds$E410==75] <-5
ds$income[ds$E410==76] <-6
ds$income[ds$E410==77] <-7
ds$income[ds$E410==78] <-8
ds$income[ds$E410==79] <-9
ds$income[ds$E410==80] <-10
ds$income[ds$E410==81] <-1
ds$income[ds$E410==82] <-2
ds$income[ds$E410==83] <-3
ds$income[ds$E410==84] <-4
ds$income[ds$E410==85] <-5
ds$income[ds$E410==86] <-6
ds$income[ds$E410==87] <-7
ds$income[ds$E410==88] <-8
ds$income[ds$E410==89] <-9
ds$income[ds$E410==90] <-10
ds$income[ds$E410==91] <-1
ds$income[ds$E410==92] <-2
ds$income[ds$E410==93] <-3
ds$income[ds$E410==94] <-4
ds$income[ds$E410==95] <-5
ds$income[ds$E410==96] <-6
ds$income[ds$E410==97] <-7
ds$income[ds$E410==98] <-8
ds$income[ds$E410==99] <-9
ds$income[ds$E410==100] <-10
ds$income[ds$E410==101] <-1
ds$income[ds$E410==102] <-2
ds$income[ds$E410==103] <-3
ds$income[ds$E410==104] <-4
ds$income[ds$E410==105] <-5
ds$income[ds$E410==106] <-6
ds$income[ds$E410==107] <-7
ds$income[ds$E410==108] <-8
ds$income[ds$E410==109] <-9
ds$income[ds$E410==110] <-10
ds$income[ds$E410==111] <-1
ds$income[ds$E410==112] <-2
ds$income[ds$E410==113] <-3
ds$income[ds$E410==114] <-4
ds$income[ds$E410==115] <-5
ds$income[ds$E410==116] <-6
ds$income[ds$E410==117] <-7
ds$income[ds$E410==118] <-8
ds$income[ds$E410==119] <-9
ds$income[ds$E410==120] <-10
ds$income[ds$E410==121] <-1
ds$income[ds$E410==122] <-2
ds$income[ds$E410==123] <-3
ds$income[ds$E410==124] <-4
ds$income[ds$E410==125] <-5
ds$income[ds$E410==126] <-6
ds$income[ds$E410==127] <-7
ds$income[ds$E410==128] <-8
ds$income[ds$E410==129] <-9
ds$income[ds$E410==130] <-10
ds$income[ds$E410==131] <-1
ds$income[ds$E410==132] <-2
ds$income[ds$E410==133] <-3
ds$income[ds$E410==134] <-4
ds$income[ds$E410==135] <-5
ds$income[ds$E410==136] <-6
ds$income[ds$E410==137] <-7
ds$income[ds$E410==138] <-8
ds$income[ds$E410==139] <-9
ds$income[ds$E410==140] <-10
ds$income[ds$E410==141] <-1
ds$income[ds$E410==142] <-2
ds$income[ds$E410==143] <-3
ds$income[ds$E410==144] <-4
ds$income[ds$E410==145] <-5
ds$income[ds$E410==146] <-6
ds$income[ds$E410==147] <-7
ds$income[ds$E410==148] <-8
ds$income[ds$E410==149] <-9
ds$income[ds$E410==150] <-10
ds$income[ds$E410==151] <-1
ds$income[ds$E410==152] <-2
ds$income[ds$E410==153] <-3
ds$income[ds$E410==154] <-4
ds$income[ds$E410==155] <-5
ds$income[ds$E410==156] <-6
ds$income[ds$E410==157] <-7
ds$income[ds$E410==158] <-8
ds$income[ds$E410==159] <-9
ds$income[ds$E410==160] <-10
ds$income[ds$E410==161] <-1
ds$income[ds$E410==162] <-2
ds$income[ds$E410==163] <-3
ds$income[ds$E410==164] <-4
ds$income[ds$E410==165] <-5
ds$income[ds$E410==166] <-6
ds$income[ds$E410==167] <-7
ds$income[ds$E410==168] <-8
ds$income[ds$E410==169] <-9
ds$income[ds$E410==170] <-10
ds$income[ds$E410==171] <-1
ds$income[ds$E410==172] <-2
ds$income[ds$E410==173] <-3
ds$income[ds$E410==174] <-4
ds$income[ds$E410==175] <-5
ds$income[ds$E410==176] <-6
ds$income[ds$E410==177] <-7
ds$income[ds$E410==178] <-8
ds$income[ds$E410==179] <-9
ds$income[ds$E410==180] <-10
ds$income[ds$E410==181] <-1
ds$income[ds$E410==182] <-2
ds$income[ds$E410==183] <-3
ds$income[ds$E410==184] <-4
ds$income[ds$E410==185] <-5
ds$income[ds$E410==186] <-6
ds$income[ds$E410==187] <-7
ds$income[ds$E410==188] <-8
ds$income[ds$E410==189] <-9
ds$income[ds$E410==190] <-10
ds$income[ds$E410==191] <-1
ds$income[ds$E410==192] <-2
ds$income[ds$E410==193] <-3
ds$income[ds$E410==194] <-4
ds$income[ds$E410==195] <-5
ds$income[ds$E410==196] <-6
ds$income[ds$E410==197] <-7
ds$income[ds$E410==198] <-8
ds$income[ds$E410==199] <-9
ds$income[ds$E410==200] <-10
ds$income[ds$E410==201] <-1
ds$income[ds$E410==202] <-2
ds$income[ds$E410==203] <-3
ds$income[ds$E410==204] <-4
ds$income[ds$E410==205] <-5
ds$income[ds$E410==206] <-6
ds$income[ds$E410==207] <-7
ds$income[ds$E410==208] <-8
ds$income[ds$E410==209] <-9
ds$income[ds$E410==210] <-10
ds$income[ds$E410==211] <-1
ds$income[ds$E410==212] <-2
ds$income[ds$E410==213] <-3
ds$income[ds$E410==214] <-4
ds$income[ds$E410==215] <-5
ds$income[ds$E410==216] <-6
ds$income[ds$E410==217] <-7
ds$income[ds$E410==218] <-8
ds$income[ds$E410==219] <-9
ds$income[ds$E410==220] <-10
ds$income[ds$E410==221] <-1
ds$income[ds$E410==222] <-2
ds$income[ds$E410==223] <-3
ds$income[ds$E410==224] <-4
ds$income[ds$E410==225] <-5
ds$income[ds$E410==226] <-6
ds$income[ds$E410==227] <-7
ds$income[ds$E410==228] <-8
ds$income[ds$E410==229] <-9
ds$income[ds$E410==230] <-10
ds$income[ds$E410==231] <-1
ds$income[ds$E410==232] <-2
ds$income[ds$E410==233] <-3
ds$income[ds$E410==234] <-4
ds$income[ds$E410==235] <-5
ds$income[ds$E410==236] <-6
ds$income[ds$E410==237] <-7
ds$income[ds$E410==238] <-8
ds$income[ds$E410==239] <-9
ds$income[ds$E410==240] <-10
ds$income[ds$E410==241] <-1
ds$income[ds$E410==242] <-2
ds$income[ds$E410==243] <-3
ds$income[ds$E410==244] <-4
ds$income[ds$E410==245] <-5
ds$income[ds$E410==246] <-6
ds$income[ds$E410==247] <-7
ds$income[ds$E410==248] <-8
ds$income[ds$E410==249] <-9
ds$income[ds$E410==250] <-10
ds$income[ds$E410==251] <-1
ds$income[ds$E410==252] <-2
ds$income[ds$E410==253] <-3
ds$income[ds$E410==254] <-4
ds$income[ds$E410==255] <-5
ds$income[ds$E410==256] <-6
ds$income[ds$E410==257] <-7
ds$income[ds$E410==258] <-8
ds$income[ds$E410==259] <-9
ds$income[ds$E410==260] <-10
ds$income[ds$E410==261] <-1
ds$income[ds$E410==262] <-2
ds$income[ds$E410==263] <-3
ds$income[ds$E410==264] <-4
ds$income[ds$E410==265] <-5
ds$income[ds$E410==266] <-6
ds$income[ds$E410==267] <-7
ds$income[ds$E410==268] <-8
ds$income[ds$E410==269] <-9
ds$income[ds$E410==270] <-10

ds$incomecat <- NA
ds$incomecat [ds$income<4]<- "low"
ds$incomecat [ds$income>3]<- "medium"
ds$incomecat [ds$income>7]<- "high"
ds$incomecat <- as.factor(ds$incomecat)

ds$quintile <- NA
ds$quintile [ds$income<3]<- "1st quintile"
ds$quintile [(ds$income==3)|(ds$income==4)]<- "2nd quintile"
ds$quintile [(ds$income==5)|(ds$income==6)]<- "3rd quintile"
ds$quintile [(ds$income==7)|(ds$income==8)]<- "4th quintile"
ds$quintile [ds$income>8]<- "5th quintile"
ds$quintile <- as.factor(ds$quintile)

#---- Youth wishes and plans ----

ds$gap_emp_1y <- NA
ds$gap_emp_1y <- case_when(
  (ds$H613_01==1)&(ds$H614_01==0) ~1,
  (ds$H613_01==1)&(is.na(ds$H614_01)) ~1,
  (ds$H613_01==1)&(ds$H614_01==1) ~0
)

ds$gap_change_1y <- NA
ds$gap_change_1y <- case_when(
  (ds$H613_02==1)&(ds$H614_02==0) ~1,
  (ds$H613_02==1)&(is.na(ds$H614_02)) ~1,
  (ds$H613_02==1)&(ds$H614_02==1) ~0
)

ds$gap_business_1y <- NA
ds$gap_business_1y <- case_when(
  (ds$H613_03==1)&(ds$H614_03==0) ~1,
  (ds$H613_03==1)&(is.na(ds$H614_03)) ~1,
  (ds$H613_03==1)&(ds$H614_03==1) ~0
)

ds$gap_uni_1y <- NA
ds$gap_uni_1y <- case_when(
  (ds$H613_04==1)&(ds$H614_04==0) ~1,
  (ds$H613_04==1)&(is.na(ds$H614_04)) ~1,
  (ds$H613_04==1)&(ds$H614_04==1) ~0
)

ds$gap_trai_1y <- NA
ds$gap_trai_1y <- case_when(
  (ds$H613_05==1)&(ds$H614_05==0) ~1,
  (ds$H613_05==1)&(is.na(ds$H614_05)) ~1,
  (ds$H613_05==1)&(ds$H614_05==1) ~0
)

ds$gap_move_1y <- NA
ds$gap_move_1y <- case_when(
  (ds$H613_06==1)&(ds$H614_06==0) ~1,
  (ds$H613_06==1)&(is.na(ds$H614_06)) ~1,
  (ds$H613_06==1)&(ds$H614_06==1) ~0
)

ds$gap_moveout_1y <- NA
ds$gap_moveout_1y <- case_when(
  (ds$H613_07==1)&(ds$H614_07==0) ~1,
  (ds$H613_07==1)&(is.na(ds$H614_07)) ~1,
  (ds$H613_07==1)&(ds$H614_07==1) ~0
)

ds$gap_country_1y <- NA
ds$gap_country_1y <- case_when(
  (ds$H613_08==1)&(ds$H614_08==0) ~1,
  (ds$H613_08==1)&(is.na(ds$H614_08)) ~1,
  (ds$H613_08==1)&(ds$H614_08==1) ~0
)

ds$gap_buy_1y <- NA
ds$gap_buy_1y <- case_when(
  (ds$H613_09==1)&(ds$H614_09==0) ~1,
  (ds$H613_09==1)&(is.na(ds$H614_09)) ~1,
  (ds$H613_09==1)&(ds$H614_09==1) ~0
)

ds$gap_marry_1y <- NA
ds$gap_marry_1y <- case_when(
  (ds$H613_10==1)&(ds$H614_10==0) ~1,
  (ds$H613_10==1)&(is.na(ds$H614_10)) ~1,
  (ds$H613_10==1)&(ds$H614_10==1) ~0
)

ds$gap_partner_1y <- NA
ds$gap_partner_1y <- case_when(
  (ds$H613_11==1)&(ds$H614_11==0) ~1,
  (ds$H613_11==1)&(is.na(ds$H614_11)) ~1,
  (ds$H613_11==1)&(ds$H614_11==1) ~0
)

ds$gap_child_1y <- NA
ds$gap_child_1y <- case_when(
  (ds$H613_12==1)&(ds$H614_12==0) ~1,
  (ds$H613_12==1)&(is.na(ds$H614_12)) ~1,
  (ds$H613_12==1)&(ds$H614_12==1) ~0
)

ds$gap_child_3y <- NA
ds$gap_child_3y <- case_when(
  (ds$H615_12==1)&(ds$H616_12==0) ~1,
  (ds$H615_12==1)&(is.na(ds$H616_12)) ~1,
  (ds$H615_12==1)&(ds$H616_12==1) ~0
)
save (ds, file = "data/ds_analysis_esa.Rda")

write.dta (ds, "data/ds_analysis_esa.dta",version = 7L,
           convert.dates = TRUE, tz = "CET",
           convert.factors = c("labels", "string", "numeric", "codes"))


