library(survey)
library(weights)
library(anesrake)

weigh_data <- function(ds, minimum_weight, trim_lower, trim_upper) {

    ##Randomise third gender category and don't knows
    
    #Replace with NA
    ds$gender_w <-ds$B002
    ds$gender_w[ds$B002==3]<- NA
    
    #Randomise
    ds$gender_w[is.na(ds$gender_w)] <- sample(ds$gender_w[!is.na(ds$gender_w)], sum(is.na(ds$gender_w)), replace=F)
    
    
    #Country
    ds$country <- ds$B001
    
    # EU only
    ds <- ds[ds$country<28,] %>% droplevels()

    ds$country = factor(ds$country, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27"), labels=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden"), ordered=FALSE)
    
    
    

    ##Recode age gender categories
    
    ds$agecat_w <- cut(ds$B003_01,
                       breaks=c(-Inf, 24, 34, 44, 54, 64, Inf),
                       labels=c("18-24","25-34","35-44", "45-54", "55-64", "65+"))
    
    
    ds$age_gender_w <-NA
    
    male <- (ds$gender_w==1)
    female <- (ds$gender_w==2)
    age_18_24 <- (ds$agecat_w=="18-24")
    age_25_34 <- (ds$agecat_w=="25-34")
    age_35_44 <- (ds$agecat_w=="35-44")
    age_45_54 <- (ds$agecat_w=="45-54")
    age_55_64 <- (ds$agecat_w=="55-64")
    age_65_over <- (ds$agecat_w=="65+")
    
    ds$age_gender_w <- case_when(
      male & age_18_24 ~ 1,
      male & age_25_34 ~ 2,
      male & age_35_44 ~ 3,
      male & age_45_54 ~ 4,
      male & age_55_64 ~ 5,
      male & age_65_over ~ 6,
      female & age_18_24 ~ 7,
      female & age_25_34 ~ 8,
      female & age_35_44 ~ 9,
      female & age_45_54 ~ 10,
      female & age_55_64 ~ 11,
      female & age_65_over ~ 12)
    
    ##Recode employment to 1=employed, 2=other
    
        ds$emp_w <- case_when(
      ds$D001==1 ~ 1,
      ds$D001==2 ~ 1,
      ds$D001==3 ~ 1,
      ds$D001>4 ~ 2)

    ##Randomise NAs for employment
    
    ds$emp_w[is.na(ds$emp_w)] <- sample(ds$emp_w[!is.na(ds$emp_w)], sum(is.na(ds$emp_w)), replace=F)
    
    ##recode education to 1=tertiary, 2=non-tertiary
    
    ds$edu_w <- case_when(
      ds$F004==1 ~ 2,
      ds$F004==2 ~ 2,
      ds$F004==3 ~ 1
      )
    
    ##randomise NAs for education
    
    ds$edu_w[is.na(ds$edu_w)] <- sample(ds$edu_w[!is.na(ds$edu_w)], sum(is.na(ds$edu_w)), replace=F)
    
    #type of variable
    
    ds$CASE <- as.numeric(ds$CASE)
    ds$country <- as.character(ds$country)
    ds$age_gender_w <- as.integer(ds$age_gender_w)
    ds$emp_w<- as.integer(ds$emp_w)
    ds$edu_w <- as.integer(ds$edu_w)
    
    ##necessary variables only
    ds$CASE <- as.character(ds$CASE)
    vars <- c("CASE", "country","age_gender_w","emp_w","edu_w")
    ds_w <- ds[vars]
    
    ##imputing missing strata (not run)
  #  imput <- read.csv("imput.csv")
  #  ds_w <- full_join(ds_w, imput)


    #Creating a list of country files
    country_names <- as.vector(unique(ds_w$country))
    
    country_data <- lapply(country_names, function(country) {
      
      ds_w[ds_w$country == country,]
      
    })
    names(country_data) <- country_names
    
    ##reading target data

    targets <-read.csv("weight_targets_2023.csv")

    country_targets <- lapply(country_names, function(country) {
      
      targets[targets$country == country,]
      
    })
    names(country_targets) <- country_names
    
    set.seed(1)
    
    
    
    #### Applying raking function to each country ###
    weights <- lapply(country_names, function(country) {
      
      w_target <- with(country_targets[[country]], list(
        age_gender_w = wpct(age_gender, population),
        emp_w = wpct(employed, population),
        edu_w = wpct(tertiary, population)
      ))
      
      print(paste("<<--------------",country,"-------------->>"))
      
      cap <- 4
      convergence <- FALSE
      while (convergence==FALSE) {
        
        raking <-     anesrake(w_target,
                               country_data[[country]],
                               country_data[[country]]$CASE,
                               cap=cap,
                               choosemethod = "total",
                               type = "pctlim",
                               pctlim = 0.05,
                               maxit = 5000,
                               convcrit = 0.001,
         )
      
        if (raking$converge=="Complete convergence was achieved" &
            min(raking$weightvec)>minimum_weight) {
          
          convergence <- TRUE
          print(paste("<<-------------- Convergence at cap: ", cap," -------------->>"))
          
        } else if (raking$converge=="Complete convergence was achieved") {
          
          print("<<-------------- Convergence, but too small weights -------------->>")
          cap <- cap + 1
          print(paste("<<-------------- Cap increased to: ", cap," -------------->>"))
          
        } else {
          
          cap <- cap + 1
          print(paste("<<-------------- Cap increased to: ", cap," -------------->>"))
          
        }
      
      }
     
      country_data[[country]]$w <- raking$weightvec
      
      country_data[[country]]$cap <- cap
      
      country_data[[country]]$trimmed <- "OK"
      country_data[[country]]$trimmed[country_data[[country]]$w<0.16] <- "Too low"
      country_data[[country]]$trimmed[country_data[[country]]$w>6] <- "Too high"
      
      #Adding the targets
      country_data[[country]] <- country_data[[country]] %>%
        rename(employed = emp_w) %>%
        rename(tertiary = edu_w) %>%
        rename(age_gender = age_gender_w) %>%
                left_join(targets,by=c("country","age_gender","employed","tertiary"))
      
      #Trimming the weights
      ds_svy <- svydesign(id=~CASE, 
                          strata=~strata, 
                          weights=~w, 
                          data=country_data[[country]], 
                          fpc=~population)
      
      #trim weights
      ds_trim <-trimWeights(ds_svy, lower=trim_lower, upper=trim_upper, strict=TRUE)
      
      #store trimmed weights
      country_data[[country]]$w_trimmed <- weights(ds_trim)
      
      #rescaling weights to population 18+ total=424,755,108
      country_data[[country]] <- country_data[[country]] %>%
        mutate(w_country = sum(w)) %>%
        mutate(w_gross = pop_country / w_country * w) %>%
        mutate(w_country_trim = sum(w_trimmed)) %>%
        mutate(w_gross_trim = pop_country / w_country_trim * w_trimmed)
        
      #returns the dataset with extra weight variable
      return(country_data[[country]])
      
    }) 
    names(weights) <- country_names
    
    
    #print the final cap used
    lapply(country_names, function(country) {
      
      print(paste(country,": ",max(weights[[country]]$cap)))
      print(table(weights[[country]]$trimmed))
      
    })
    
    
    #Merging into one dataframe
    weights <- do.call("rbind",weights)
    
    #Population: 
    sum(weights$w_gross)
    sum(weights$w_gross_trim)
    
    #Rescaling the weights to a mean of 1
    weights$w_gross <- weights$w_gross / mean(weights$w_gross)
    weights$w_gross_trim <- weights$w_gross_trim / mean(weights$w_gross_trim)
    
    #Showing the distributions of the weights by country
    for (country in country_names) {
      
      print(country)
      
      weights %>%
        filter(country == !!country) %>%
        select(w, w_trimmed, w_gross, w_gross_trim) %>%
        summary() %>%
        print()
      
    }
    
    return(weights)

}