cleaning_2023 <- function(ds) {
  
  ###### Correcting for unauthorised routing #######
  
  #Non-workers answering employment questions via back button
  
  routing.list <- c("D652",	"D609",	"D608_01",	"D610",	"D610_01",	"D610_02",	"D610_03",	"D610_04",	
                    "D642",	"D611_01",	"D611_01a",	"D672_01",	"D672_01a", "D616_01", "D604_01",
                    "D604_02", "D604_03", "D604_04", "D604_05", "D639_01", "D646", "H613_02", "H614_02", "H615_02", "H616_02")
  
  for(var in routing.list){
  for(i in nrow(ds)){
  if(ds[i,"D601"]!=1 & ds[i,"D601"]!=2 & ds[i,"D601"]!=3 | is.na(ds[i,"D601"])){
    ds[i, var] <- NA
  }else{ds[i,var] <- ds[i, var]}
  }
  }
    
  #Traineeship questions
  
  routing.list2 <- c("D663_01", "D664_01", "D665", "D666", "D667", "D667_01", "D667_02",
                     "D667_03", "D667_04", "D667_05", "D668_01", "D668_02", "D668_03", 
                     "D668_04", "D669", "D670", "D671")
  
  for(var in routing.list){
    for(i in nrow(ds)){
      if((ds[i,"D662_01"]!=1 & ds[i,"D662_02"]!=1) | (is.na(ds[i,"D662_01"]) & is.na(ds[i,"D662_02"]))) {
        ds[i, var] <- NA
      }else{ds[i,var] <- ds[i, var]}
    }
  } 

  
  ######## Dropouts ###########
  
  ##Rule: Valid interview if the respondent did not drop out before page 60 - household questions
  
  ds_clean <- ds[!(ds$LASTPAGE<60),]

  ######## Item nonresponse #######
  
  #Rule: Valid interview if there are less than 50% item nonresponse for core questions
  # Demographics (B, F, H section), paradata, routed questions (most of D section) are excluded
  # Training questions excluded
  # Sensitive questions (income, disability, G section) excluded
  # A core question was selected for each page
  
  itemlist <- c("C601_01", "C603_01", "C604_01", "C638_01", "C606_01", "C612_01",
                "C607_03", "C619", "C609_01", "C629_01", "D612_04", "E616_01",
                "E601_01", "E603_01", "E625_01", "E606", "E608", "E607_01",
                "E626_02", "E627_01")
  
  ds_clean$item_na_count <- apply(is.na(ds_clean[colnames(ds_clean) %in% itemlist]), 1, sum)
  #ds_clean$long <- NA
  ds_clean$items <- length(itemlist)
  ds_clean$perc_na <- ds_clean$item_na_count / ds_clean$items
  ds_clean <- ds_clean[ds_clean$perc_na<0.5,]
  
  
  ##Time spent
  
  #Rule: if spent less than 6 minutes, interview is excluded
  
  ds_clean <- ds_clean[!(ds_clean$TIME_SUM<360),]
  
  ## Age
  
  #Rule: over 98 is excluded (99 is a common value)
  
  ds_clean <- ds_clean[!(ds_clean$B603_01>98),]
  
  #email duplicates - CURRENTLY THIS SECTION IS SWITCHED OFF
  
  #email_d <- duplicated(ds_clean["SERIAL"])
  #email_none <- is.na(ds_clean$SERIAL)
  
  #ds_clean$email_d <- (email_none=="FALSE"& email_d=="TRUE")
  
  #ds_clean$clean <- TRUE

return(ds_clean)



}



