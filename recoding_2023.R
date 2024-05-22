recode_2023 <- function(ds) {
  
  
# Removing unnecessary variables
  
  remove <- c("MODE", "A603_01", "B604", "C623_05", "C623_04", "C623_06", "C623_07",
                    "D611_01a", "D672_01a", "D657_01", "D657_02",
                   "D657_03", "D612_02a", "D612_03a", "D612_04a", "D659", "D659_01",
                   "D659_02", "D659_03", "D659_04", "D659_05", "D659_06", "D659_07", 
                   "D659_08", "D659_09", "D659_10", "D659_11", "D648", "D648_01",
                  "D648_02", "D648_03", "D648_04", "D648_05", "D660", "D661", "D661_01",
                  "D661_02", "D661_03", "D661_04", "D661_05", "D661_06", "D661_07", 
                 "D661_08", "E612_01", "E612_02", "E612_03", "H612", "H612_01", "H612_02",
                "H612_03", "H612_04", "H612_05", "TIME001", "TIME002", "TIME003", "TIME004", 
              "TIME005", "TIME006", "TIME007", "TIME008", "TIME009", "TIME010", "TIME011", 
              "TIME012", "TIME013", "TIME014", "TIME015", "TIME016", "TIME017", "TIME018", 
              "TIME019", "TIME020", "TIME021", "TIME022", "TIME023", "TIME024", "TIME025", 
              "TIME026", "TIME027", "TIME028", "TIME029", "TIME030", "TIME031", "TIME032",
              "TIME033", "TIME034", "TIME035", "TIME036", "TIME037", "TIME038", "TIME039", 
              "TIME040", "TIME041", "TIME042", "TIME043", "TIME044", "TIME045", "TIME046", 
              "TIME047", "TIME048", "TIME049", "TIME050", "TIME051", "TIME052", "TIME053", 
              "TIME054", "TIME055", "TIME056", "TIME057", "TIME058", "TIME059", "TIME060", 
              "TIME061", "TIME062", "TIME063", "TIME064", "TIME065", "TIME066", "TIME067", 
              "TIME068", "TIME069", "TIME070", "TIME071", "TIME072", "TIME073", "TIME074", 
              "TIME075", "TIME076", "TIME077", "TIME078", "TIME079", "MAILSENT", "Q_VIEWER"
              )
  
  ds_recoded <- ds %>%select(!remove)
  
  
  # Recode NAs (except -2, where the meaning matters)
  
 ds_recoded<- ds_recoded %>% replace_with_na_all(condition = ~.x ==-1)
 ds_recoded<- ds_recoded %>% replace_with_na_all(condition = ~.x ==-9)
  
  # Recode Yes/No questions to 0 and 1
  
  yes_no <- c("C610","C53","C630","D654_01","D654_02","D654_03","D654_04","D654_05",
              "D654_06","D644","D645_01","D645_02","D645_03","D658","D646","D643",
              "D647", "D662_01", "D662_02", "D665", "D666", "D668_01","D668_02",
              "D668_03","D668_04","D669", "D671","E603_01","E603_02","E603_03",
              "E603_04","E603_05","E603_06","E603_07","E626_01","E626_02","E626_03",
              "E626_04","E626_05","E626_06","E626_07","E627_01","E627_02","E627_03",
              "E627_04","E627_05","E627_06","F662_01","F662_02","F662_03","F662_04",
              "F662_05","F662_06","F637_02", "H602", "H603", "H604", "H608", "H613_01",
              "H613_02","H613_03","H613_04","H613_05","H613_06","H613_07","H613_08",
              "H613_09","H613_10","H613_11","H613_12","H614_01", "H614_02","H614_03",
              "H614_04","H614_05","H614_06","H614_07","H614_08","H614_09","H614_10",
              "H614_11","H614_12","H615_01", "H615_02","H615_03","H615_04","H615_05",
              "H615_06","H615_07","H615_08","H615_09","H615_10","H615_11","H615_12",
              "H616_01", "H616_02","H616_03","H616_04","H616_05","H616_06","H616_07",
              "H616_08","H616_09","H616_10","H616_11","H616_12")
  
  norecode <- function(var){
    ds_recoded[,var][ds_recoded[,var]==2] <- 0
    return(ds_recoded)
  }
  
  for(variables in yes_no){
    ds_recoded <- norecode(variables)
  }

  #  Rename vars to trend
  ds_recoded <- ds_recoded %>% 
    rename(B001 = B601,
           B002 = B602,
           B003_01 = B603_01,
           C001_01 = C601_01,
           C003_01 = C603_01,
           C203_07 = C603_07,
           C003_03 = C603_03,
           C003_04 = C603_04,
           C303_08 = C603_08,
           C004_01 = C604_01,
           C005_01 = C638_01,
           C005_02 = C638_02,
           C005_03 = C638_03,
           C005_04 = C638_04,
           C005_05 = C638_05,
           C006_01 = C606_01,
           C006_02 = C606_02,
           C006_03 = C606_03,
           C310 = C610,
           C311 = C611,
           F339 = C53,
           F346 = C640,
           F346_01 = C640_01,
           F346_02 = C640_02,
           F346_03 = C640_03,
           F346_04 = C640_04,
           F346_05 = C640_05,
           F346_06 = C640_06,
           F346_07 = C640_07,
           F346_08 = C640_08,
           C312_01 = C612_01,
           C007_02 = C607_02,
           C007_05 = C607_05,
           C007_03 = C607_03,
           C007_04 = C607_04,
           C007_01 = C607_01,
           C307_06 = C607_06,
           C319 = C619,
           C209 = C609_01,
           C203_05 = C629_01,
           C543_05 = C629_05,
           C530 = C630,
           D235 =  D635,
           D001 = D601,
           D336 = D636,
           D209 = D609,
           D008 = D608_01,
           D210 = D610,
           D210_01 = D610_01,
           D210_02 = D610_02,
           D210_03 = D610_03,
           D210_04 = D610_04,
           D542 = D642,
           C008 = F661,
           D211_01 = D611_01,
           D211_02 = D672_01,
           D216_01 = D616_01,
           D004_01 = D604_01,
           D004_02 = D604_02,
           D004_03 = D604_03,
           D004_04 = D604_04,
           D004_05 = D604_05,
           D005_01 = D639_01,
           D212_02 = D612_02,
           D212_03 = D612_03,
           D212_04 = D612_04,
           D544 = D644,
           D545_01 = D645_01,
           D545_02 = D645_02,
           D545_03 = D645_03,
           D546 = D646, 
           D543 = D643,
           E511 =  E611, 
           E516_01 = E616_01,
           E516_02 = E616_02,
           E001_01 = E601_01,
           E003_01 = E603_01,
           E003_02 = E603_02,
           E003_03 = E603_03,
           E003_04 = E603_04,
           E003_05 = E603_05,
           E003_06 = E603_06,
           E203_07 = E603_07,
           E525_01 = E625_01,
           E526_01 = E625_02,
           E006 = E606,
           E208 = E608,
           E007_01 = E607_01,
           E410 = E610,
           F344 = F644,
           F225 = F625,
           F556 = F656,
           F557 = F657,
           F558 = F658,
           F021 = F655,
           F022_01 = F637_02,
           G601_01 = G601_01,
           G502_01 = G602_01,
           G502_02 = G602_02,
           G502_03 = G602_03,
           G503_01 = G603_01,
           G503_02 = G603_02,
           G503_03 = G603_03,
           G503_04 = G603_04,
           H001 = H601,
           H002 = H602,
           H203 = H603,
           H204 = H604,
           H308 = H608,
           H409 = H609,
           H410 = H610,
           H411 = H611,
           H411_01 = H611_01,
           H411_02 = H611_02,
           H411_03 = H611_03,
           H411_04 = H611_04
           )
  
  #Number of children (first answer option is 0)
  
  ds_recoded$H005 <- NA
  ds_recoded$H005 <- case_when(
    ds_recoded$H605==1 ~0,
    ds_recoded$H605==2 ~1,
    ds_recoded$H605==3 ~2,
    ds_recoded$H605==4 ~3,
    ds_recoded$H605==5 ~4,
    ds_recoded$H605==6 ~5,
    ds_recoded$H605==7 ~6,
    ds_recoded$H605==8 ~7,
    ds_recoded$H605==9 ~8,
    ds_recoded$H605==10 ~9,
    ds_recoded$H605==11 ~10
  )
  
  ds_recoded$H006 <- NA
  ds_recoded$H006 <- case_when(
    ds_recoded$H606==1 ~0,
    ds_recoded$H606==2 ~1,
    ds_recoded$H606==3 ~2,
    ds_recoded$H606==4 ~3,
    ds_recoded$H606==5 ~4,
    ds_recoded$H606==6 ~5,
    ds_recoded$H606==7 ~6,
    ds_recoded$H606==8 ~7,
    ds_recoded$H606==9 ~8,
    ds_recoded$H606==10 ~9,
    ds_recoded$H606==11 ~10
  )
  
  ds_recoded$H207 <- NA
  ds_recoded$H207 <- case_when(
    ds_recoded$H607==1 ~0,
    ds_recoded$H607==2 ~1,
    ds_recoded$H607==3 ~2,
    ds_recoded$H607==4 ~3,
    ds_recoded$H607==5 ~4,
    ds_recoded$H607==6 ~5,
    ds_recoded$H607==7 ~6,
    ds_recoded$H607==8 ~7,
    ds_recoded$H607==9 ~8,
    ds_recoded$H607==10 ~9,
    ds_recoded$H607==11 ~10
  )
  
  remove <- c("H605", "H606", "H607")
  ds_recoded <- ds_recoded %>%select(!remove)
  
  
  # Education
  ds_recoded$F004 <- NA
  ds_recoded$F004[ds_recoded$F344==1] <- 1
  ds_recoded$F004[ds_recoded$F344==2] <- 2
  ds_recoded$F004[ds_recoded$F344==3] <- 2
  ds_recoded$F004[ds_recoded$F344==4] <- 2
  ds_recoded$F004[ds_recoded$F344==5] <- 3
  ds_recoded$F004[ds_recoded$F344==6] <- 3
  ds_recoded$F004[ds_recoded$F344==7] <- 3
  ds_recoded$F004[ds_recoded$F344==8] <- 3
  
  
  
  return(ds_recoded)
  
}