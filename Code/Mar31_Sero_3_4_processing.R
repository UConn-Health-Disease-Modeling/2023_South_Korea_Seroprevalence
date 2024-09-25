# set root directory
setwd("/Users/frankyzhang/Dropbox/Jo_Franky/Disease-Modeling/2023_South\ Korea_Seroprevalence")

# load packages
library(dplyr)
library(lubridate)
library(tidyr)
library(lubridate)
library(openxlsx)
library(survey)
library(srvyr)
library(ggplot2)
library(magrittr)
library(readxl)
library(devtools)
library(DescTools)
library(tibble)
# library(deeplr)
library(plotROC)


# ######################
# ## 3rd Surveillance ##
# ######################

# load data
url_sero3 <- "Data/datashare_240326/df_S3_240321.xlsx"
sero3 <- read.xlsx(url_sero3)

### Age 
# unique(sero3$agebase_cat1)
# [1] "Aged 40-49"   "Aged 60-69"   "Aged 30-39"   "Aged 50-59"   "Aged 70-79"   "Aged 20-29"   "Aged 10-19"  
# [8] "Aged over 80" "Aged 05-09"  
# unique(sero3$agebase_cat2)
# "Aged 35-49"   "Aged over 65" "Aged 50-64"   "Aged 20-34"   "Aged 05-19" 

### vac date 
sero3$vax.date1 <- openxlsx::convertToDate(sero3$vax.date1)
sero3$vax.date2 <- openxlsx::convertToDate(sero3$vax.date2)
sero3$vax.date3 <- openxlsx::convertToDate(sero3$vax.date3)
sero3$vax.date4 <- openxlsx::convertToDate(sero3$vax.date4)

# sero3$vax.date.xbb <- openxlsx::convertToDate(sero3$vax.date.xbb)
# sero3$vax.date.bi <-  openxlsx::convertToDate(sero3$vax.date.bi)
# sero3$vax.date.flu22 <- openxlsx::convertToDate(sero3$vax.date.flu22)
# sero3$vax.date.flu23 <- openxlsx::convertToDate(sero3$vax.date.flu23)

# unique(sero3$vax.name.xbb)
# # [1] NA                "화이자XBB.1.5"   "모더나XBB.1.5"   "노바백스XBB.1.5"
# unique(sero3$vax.name.bi)
# # [1] NA                         "모더나BA.1"               "화이자BA.4/5"             "모더나BA.4/5"            
# # [5] "화이자BA.1"               "뉴백소비드프리필드시린지" "스카이코비원"             "노바백스" 
# unique(sero3$vax.name.flu22)
# # [1] NA                                     "박씨그리프테트라주"                   "지씨플루쿼드리밸런트프리필드시린지주"
# # [4] "코박스인플루4가PF주"                  "보령플루V테트라백신주"                "테라텍트프리필드시린지주"            
# # [7] "보령플루Ⅷ테트라백신주"                "코박스플루4가PF주"                    "비알플루텍Ⅰ테트라백신주"
# unique(sero3$vax.name.flu23)
# #  [1] NA                                     "테라텍트프리필드시린지주"            
# #  [3] "보령플루Ⅷ테트라백신주"                "스카이셀플루4가프리필드시린지"       
# #  [5] "지씨플루쿼드리밸런트프리필드시린지주" "보령플루V테트라백신주"               
# #  [7] "코박스플루4가PF주"                    "코박스인플루4가PF주"                 
# #  [9] "박씨그리프테트라주"                   "비알플루텍Ⅰ테트라백신주"    

### collect date (S3)
sero3$collect_date <- substr(sero3$collect_date, start = 1, stop = 10)
sero3$collect_date  <- lubridate::ymd(sero3$collect_date)

### confirmed date (already good to use)
# sero3$confirm1_date
# sero3$confirm2_date
# sero3$confirm3_date

### Selecting the needed features
sero3<-sero3[c("GNO", "sex", "strata", 
               "age_base", "agebase_cat1", "agebase_cat2", 
               "edu", 
               "income", "income1", "income2", "income5", 
               "collect_date", 
               "confirm1_date", "confirm2_date", "confirm3_date", 
               
               "vax.date1", "vax.date2", "vax.date3", "vax.date4", 
               "vax.name1", "vax.name2", "vax.name3", "vax.name4", 
               "S_num_S3", "S_cha_S3", "N_num_S3", "N_cha_S3"
)] 

### Define the variables by myself 
nearest_date_before <- function(ref_date, ...){
  dates <- na.omit(c(...))
  dates <- dates[dates < ref_date]
  if(length(dates) == 0) return(NA)
  return(max(dates))
}

nearest_date_after <- function(ref_date, ...){
  dates <- na.omit(c(...))
  dates <- dates[dates > ref_date]
  if(length(dates) == 0) return(NA)
  return(min(dates))
}

sero3 <- sero3 %>%
  rowwise() %>%
  mutate(
    nearest_confirmed_before_S3 = nearest_date_before(collect_date, confirm1_date, confirm2_date, confirm3_date),
    nearest_confirmed_after_S3  =  nearest_date_after(collect_date, confirm1_date, confirm2_date, confirm3_date)
  ) %>%
  ungroup()

sero3 %>% filter(!is.na(nearest_confirmed_after_S3)) # empty
# prove that only record the infection before the collect date

sero3 <- sero3 %>%
  rowwise() %>%
  mutate(
    nearest_vac_before_S3 = nearest_date_before(collect_date, vax.date1, vax.date2, vax.date3, vax.date4),
    nearest_vac_after_S3 =  nearest_date_after (collect_date, vax.date1, vax.date2, vax.date3, vax.date4)
  ) %>%
  ungroup()

sero3 %>% filter(!is.na(nearest_vac_after_S3)) %>% select(collect_date, vax.date1,vax.date2, vax.date3, vax.date4,
                                                          nearest_vac_after_S3)
# # I don't think these record should be counted! 
sero3 <- sero3 %>%
  rowwise() %>%
  mutate(vac_freq_S3 = sum(c(vax.date1, vax.date2, vax.date3, vax.date4) < collect_date, na.rm = TRUE)) %>%
  ungroup()

sero3 <- sero3 %>% 
  mutate(S_num_S3 = gsub("<", "", S_num_S3), 
         S_num_S3 = gsub(">", "", S_num_S3), 
         N_num_S3 = gsub("<", "", N_num_S3), 
         N_num_S3 = gsub("<", "", N_num_S3)) %>% 
  mutate(S_num_S3 =    as.numeric(S_num_S3), 
         N_num_S3 =    as.numeric(N_num_S3), 
         S_num_S3 =    as.numeric(S_num_S3), 
         N_num_S3 =    as.numeric(N_num_S3)) 

# ######################
# ###### 3rd end #######
# ######################



# ########################################################################################
# ########################################################################################



# ######################
# ## 4th Surveillance ##
# ######################

url_sero4 <- "Data/datashare_240326/df_S4_240327.xlsx"
sero4 <- read.xlsx(url_sero4)

### Age 
# unique(sero3$agebase_cat1)
# [1] "Aged 40-49"   "Aged 60-69"   "Aged 30-39"   "Aged 50-59"   "Aged 70-79"   "Aged 20-29"   "Aged 10-19"  
# [8] "Aged over 80" "Aged 05-09"  
# unique(sero3$agebase_cat2)
# "Aged 35-49"   "Aged over 65" "Aged 50-64"   "Aged 20-34"   "Aged 05-19"  


### vac date 
sero4$vax.date1 <- openxlsx::convertToDate(sero4$vax.date1)
sero4$vax.date2 <- openxlsx::convertToDate(sero4$vax.date2)
sero4$vax.date3 <- openxlsx::convertToDate(sero4$vax.date3)
sero4$vax.date4 <- openxlsx::convertToDate(sero4$vax.date4)

### collect date (S4)
sero4$collect_date  <- openxlsx::convertToDate(sero4$collect_date_S4)
sero4$confirm1_date <- openxlsx::convertToDate(sero4$confirm1_date)
sero4$confirm2_date <- openxlsx::convertToDate(sero4$confirm2_date)


### Selecting the needed features
sero4<-sero4[c("GNO", "sex", "strata", 
               "age_base", "agebase_cat1", "agebase_cat2", 
               "edu" , 
               "collect_date", 
               "confirm1_date", "confirm2_date", 
               
               "vax.date1", "vax.date2", "vax.date3", "vax.date4", 
               "vax.name1", "vax.name2", "vax.name3", "vax.name4", 
               "S_num_S4", "S_cha_S4", "N_num_S4", "N_cha_S4"
)] 

### Define the variables by myself 
sero4 <- sero4 %>%
  rowwise() %>%
  mutate(
    nearest_confirmed_before_S4 = nearest_date_before(collect_date, confirm1_date, confirm2_date),
    nearest_confirmed_after_S4  =  nearest_date_after(collect_date, confirm1_date, confirm2_date)
  ) %>%
  ungroup()

# sero4 %>% filter(!is.na(nearest_confirmed_after_S4)) # empty
# prove that only record the infection before the collect date 

sero4 <- sero4 %>%
  rowwise() %>%
  mutate(
    nearest_vac_before_S4 = nearest_date_before(collect_date, vax.date1, vax.date2, vax.date3, vax.date4),
    nearest_vac_after_S4 =  nearest_date_after (collect_date, vax.date1, vax.date2, vax.date3, vax.date4)
  ) %>%
  ungroup()

# sero4 %>% filter(!is.na(nearest_vac_after_S4)) # empty as expected
sero4 <- sero4 %>%
  rowwise() %>%
  mutate(vac_freq_S4 = sum(c(vax.date1, vax.date2, vax.date3, vax.date4) < collect_date, na.rm = TRUE)) %>%
  ungroup()

sero4 <- sero4 %>% 
  mutate(S_num_S4 = gsub("<", "", S_num_S4), 
         S_num_S4 = gsub(">", "", S_num_S4), 
         N_num_S4 = gsub("<", "", N_num_S4), 
         N_num_S4 = gsub("<", "", N_num_S4)) %>% 
  mutate(S_num_S4 =    as.numeric(S_num_S4), 
         N_num_S4 =    as.numeric(N_num_S4), 
         S_num_S4 =    as.numeric(S_num_S4), 
         N_num_S4 =    as.numeric(N_num_S4)) 

# ######################
# ###### 4th end #######
# ######################

# Combine two surveillance data 
current_colnames <- colnames(sero4)

modified_S4 <- sapply(current_colnames, function(name) {
  if(!grepl("_S4$", name)) { # If the name does NOT end with "_S4"
    return(paste0(name, "_S4")) # Append "_S4"
  } else {
    return(name) # Return the name as is
  }
})

colnames(sero4) <- modified_S4
index <- which(colnames(sero4) == "GNO_S4")
colnames(sero4)[index] <- "GNO"

sero <- sero3 %>%
  left_join(sero4, by = "GNO")

sero$nearest_confirmed_before_S3 = ymd(sero$nearest_confirmed_before_S3)
sero$nearest_confirmed_before_S4 = ymd(sero$nearest_confirmed_before_S4)
sero <- sero %>%
  mutate(gap_between_surveys = as.integer(collect_date_S4 - collect_date), 
         infec_gap_before_S3 = as.integer(collect_date - nearest_confirmed_before_S3),
         infec_gap_before_S4 = as.integer(collect_date_S4 - nearest_confirmed_before_S4),
         between_S1_S2_infec_cat = ifelse(is.na(collect_date_S4), NA, 
                                          ifelse(is.na(infec_gap_before_S4), "no", 
                                                 ifelse(!is.na(infec_gap_before_S4) & 
                                                          infec_gap_before_S4<=gap_between_surveys,
                                                        "yes", "no"))))
# sero %>% select(collect_date_S4, collect_date, nearest_confirmed_before_S3, infec_gap_before_S3,
#                 nearest_confirmed_before_S4, infec_gap_before_S4, gap_between_surveys, between_S1_S2_infec_cat)

check = sero %>% select(collect_date_S4, confirm1_date_S4, confirm2_date_S4, nearest_confirmed_before_S4, collect_date, 
                        gap_between_surveys, infec_gap_before_S4)

# sero[order(sero$infec_gap_before_S4, decreasing = FALSE, na.last = TRUE), ] %>% 
#   select(collect_date_S4, collect_date, nearest_confirmed_before_S3, infec_gap_before_S3,
#          nearest_confirmed_before_S4, infec_gap_before_S4, gap_between_surveys, between_S1_S2_infec_cat)






