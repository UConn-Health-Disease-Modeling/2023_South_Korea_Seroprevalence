# set root directory
setwd("/Users/frankyzhang/Dropbox/Jo_Franky/Disease-Modeling/2023_South\ Korea_Seroprevalence")

# load packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(openxlsx)
# install.packages("MatchIt")
library(MatchIt)

################################
####### Data Process ###########
################################

# sero <- read.xlsx("Data/df_S1S2_0329_forshare.xlsx")
# # sero$collect_time_S2
# # round the ages
# sero$age <- round(sero$age)
# # convertToDate on vac date 
# sero$vac1_date <- openxlsx::convertToDate(sero$vac1_date)
# sero$vac2_date <- openxlsx::convertToDate(sero$vac2_date)
# sero$vac3_date <- openxlsx::convertToDate(sero$vac3_date)
# sero$vac4_date <- openxlsx::convertToDate(sero$vac4_date)
# sero$vac5_date <- openxlsx::convertToDate(sero$vac5_date)
# # convertToDate on confirm date
# sero$confirm1_date <- openxlsx::convertToDate(sero$confirm1_date)
# sero$confirm2_date <- openxlsx::convertToDate(sero$confirm2_date)
# sero$confirm3_date <- openxlsx::convertToDate(sero$confirm3_date)
# # lubridate::ymd on collect_date_S1
# sero$collect_date_S1 <- lubridate::ymd(sero$collect_date_S1)
# sero$collect_time_S2 <- substr(sero$collect_time_S2, start = 1, stop = 10)
# sero$collect_date_S2 <- lubridate::ymd(sero$collect_time_S2)
# 
# # sero %>% filter(q9_3_S2 == 1) # 112 "yes" with hospitalization
# 
# # colnames(sero)
# sero2<-sero[c("GNO_S1", "GNO_S2", "sex", "strata", 
#               "age", "age10", 
#               "edu", 
#               "income", "income5", 
#               
#               "collect_date_S1", "collect_date_S2", 
#               "confirm1_date", "confirm2_date", "confirm3_date", 
#               "p21_q6_S1", "q9_S2",                                             # confirm_S1, confirm_S2
#               
#               "vac1_type", "vac2_type", "vac3_type", "vac4_type", "vac5_type", 
#               "vac1_date", "vac2_date", "vac3_date", "vac4_date", "vac5_date", 
#               "S_num_S1", "N_num_S1", "S_num_S2", "N_num_S2", 
#               "S_cha_S1", "N_cha_S1", "S_cha_S2", "N_cha_S2", 
#               
#               "wt_S1", "wt_S2", "wt_age_S1", "wt_age_S2", 
#               
#               "q9_4_1_S2", "q9_4_2_S2", "q9_4_3_S2", "q9_4_4_S2", "q9_4_5_S2", 
#               "q9_4_6_S2", "q9_4_7_S2", "q9_4_8_S2", "q9_4_9_S2", "q9_4_10_S2", 
#               "q9_4_11_S2", "q9_4_12_S2", "q9_4_13_S2", "q9_4_14_S2", "q9_4_15_S2", 
#               "q9_4_16_S2", "q9_4_17_S2", "q9_4_18_S2", "q9_4_19_S2", "q9_4_20_S2", 
#               "q9_4_21_S2", "q9_4_22_S2", "q9_4_23_S2",                         # S2 symptoms
#               
#               "p21_q2_1_S1", "p21_q2_2_S1", "p21_q2_3_S1", 
#               "p21_q2_4_S1", "p21_q2_5_S1", "p21_q2_6_S1",
#               "p21_q2_7_S1", "p21_q2_8_S1", "p21_q2_9_S1", 
#               "p21_q2_10_S1", "p21_q2_91_S1",                                   # S1 diseases
#               "q2_S2",                                                          # S2 diseases
#               "p21_q1_S1", "q1_S2",                                             # S1, S2 health status
#               "q9_3_S2", "q9_3_day_S2",                                         # Hospitalization treatment and duration (S2)
#               "p21_q4_S1", "q6_S2"                                              # S1_symptom and S2_symptom
# )] %>% dplyr::rename("hosp_S2"        = "q9_3_S2",                # 1:yes 2:no
#                      "hosp_dur_S2"    = "q9_3_day_S2", 
#                      
#                      "sym_S1_raw"     = "p21_q4_S1",              # 1:no 2:yes
#                      "sym_S2_raw"     = "q6_S2",                  # 1:yes 2:no
#                      
#                      "confirm_S1"     = "p21_q6_S1",              # 1:no 2:yes
#                      "confirm_S2"     = "q9_S2",                  # 1:yes 2:no
#                      
#                      "hypertension1"  = "p21_q2_1_S1",            # 1:no; 2:yes
#                      "diabetes1"      = "p21_q2_2_S1", 
#                      "highcol1"       = "p21_q2_3_S1", 
#                      "cancer1"        = "p21_q2_4_S1", 
#                      "brainblood1"    = "p21_q2_5_S1", 
#                      "chronickidney1" = "p21_q2_6_S1",
#                      "chroniclung1"   = "p21_q2_7_S1", 
#                      "liver1"         = "p21_q2_8_S1", 
#                      "immunocomp1"    = "p21_q2_9_S1", 
#                      "rumatis1"       = "p21_q2_10_S1",
#                      "other1"         = "p21_q2_91_S1", 
#                      
#                      "S2_sym1"        = "q9_4_1_S2" , 
#                      "S2_sym2"        = "q9_4_2_S2" , 
#                      "S2_sym3"        = "q9_4_3_S2" , 
#                      "S2_sym4"        = "q9_4_4_S2" , 
#                      "S2_sym5"        = "q9_4_5_S2" , 
#                      "S2_sym6"        = "q9_4_6_S2" , 
#                      "S2_sym7"        = "q9_4_7_S2" , 
#                      "S2_sym8"        = "q9_4_8_S2" , 
#                      "S2_sym9"        = "q9_4_9_S2" , 
#                      "S2_sym10"       = "q9_4_10_S2", 
#                      "S2_sym11"       = "q9_4_11_S2", 
#                      "S2_sym12"       = "q9_4_12_S2", 
#                      "S2_sym13"       = "q9_4_13_S2", 
#                      "S2_sym14"       = "q9_4_14_S2", 
#                      "S2_sym15"       = "q9_4_15_S2", 
#                      "S2_sym16"       = "q9_4_16_S2", 
#                      "S2_sym17"       = "q9_4_17_S2", 
#                      "S2_sym18"       = "q9_4_18_S2", 
#                      "S2_sym19"       = "q9_4_19_S2", 
#                      "S2_sym20"       = "q9_4_20_S2", 
#                      "S2_sym21"       = "q9_4_21_S2", 
#                      "S2_sym22"       = "q9_4_22_S2", 
#                      "S2_sym23"       = "q9_4_23_S2", 
#                      
#                      "otherdisease_S2_raw"  = "q2_S2",            # whether you have diseases or not; 1:yes, 2:no
#                      
#                      "health_Status_S1" = "p21_q1_S1",            # 1: Very good 2: Good 3: Average 4: Bad 5: Very bad
#                      "health_Status_S2" = "q1_S2")                # 1: Very good 2: Good 3: Average 4: Bad 5: Very bad
# sero2 <- sero2 %>% mutate(otherdisease_S1 = ifelse(hypertension1  == 1& 
#                                                      diabetes1      == 1&
#                                                      highcol1       == 1&
#                                                      cancer1        == 1&
#                                                      brainblood1    == 1&
#                                                      chronickidney1 == 1&
#                                                      chroniclung1   == 1&
#                                                      liver1         == 1&
#                                                      immunocomp1    == 1&
#                                                      rumatis1       == 1&
#                                                      other1         == 1, "no", "yes"), 
#                           otherdisease_S2 = ifelse(otherdisease_S2_raw == 2, "no", "yes"),
#                           
#                           sym_S1          = ifelse(sym_S1_raw == 1, "no", 
#                                                    ifelse(sym_S1_raw == 2, "yes", NA)), 
#                           sym_S2          = ifelse(sym_S2_raw == 1, "yes", 
#                                                    ifelse(sym_S2_raw == 2, "no", NA)), 
#                           
#                           hosp_S2         = ifelse(hosp_S2 == 1, "yes", "no"), 
#                           confirm_S1      = ifelse(confirm_S1 == 2, "yes", "no"), 
#                           confirm_S2      = ifelse(confirm_S2 == 1, "yes", "no")) %>% 
#   mutate(group1 = case_when((grepl("Positive",S_cha_S1) & grepl("Reactive",N_cha_S1)) ~ 1, # S+N+ Inf(vac)
#                             (grepl("Negative",S_cha_S1) & grepl("Reactive",N_cha_S1)) ~ 2,           # S-N+ Inf
#                             (grepl("Positive",S_cha_S1) & grepl("Nonreactive",N_cha_S1)) ~ 3,        # S+N- Vac
#                             (grepl("Negative",S_cha_S1) & grepl("Nonreactive",N_cha_S1)) ~ 4,        # S-N-  Naive               
#                             TRUE ~ 0))%>%                                                          
#   mutate(group2 = case_when((grepl("Positive",S_cha_S2) & grepl("Reactive",N_cha_S2)) ~ 1, # S+N+ Inf(vac) 
#                             (grepl("Negative",S_cha_S2) & grepl("Reactive",N_cha_S2)) ~ 2,           # S-N+ Inf
#                             (grepl("Positive",S_cha_S2) & grepl("Nonreactive",N_cha_S2)) ~ 3,        # S+N- Vac
#                             (grepl("Negative",S_cha_S2) & grepl("Nonreactive",N_cha_S2)) ~ 4,        # S-N-  Naive   
#                             TRUE ~ 0))
# sero2$group11 =  ifelse( grepl("1", sero2$group1),"inf",
#                          ifelse(grepl("2", sero2$group1),"inf",
#                                 ifelse(grepl("3", sero2$group1), "vac", 
#                                        ifelse(grepl("4", sero2$group1), "naive", 
#                                               NA))))
# sero2$group22 =  ifelse( grepl("1", sero2$group2),"inf",
#                          ifelse(grepl("2", sero2$group2),"inf",
#                                 ifelse(grepl("3", sero2$group2), "vac", 
#                                        ifelse(grepl("4", sero2$group2), "naive", 
#                                               NA))))  
# drop_list <- c(
#   # "hypertension1", "diabetes1", "highcol1", "cancer1", "brainblood1", 
#   #              "chronickidney1", "chroniclung1", "liver1", "immunocomp1", "rumatis1", 
#   #              "other1", 
#   "otherdisease_S2_raw", "sym_S1_raw", "sym_S2_raw")
# sero2 <- sero2[, !(names(sero2) %in% drop_list)]
# 
# ################################################################################
# # infections and gaps
# 
# sero2 <- sero2 %>%
#   mutate(across(ends_with("date"), ymd, .names = "date_{.col}"))
# 
# # Function to find nearest date before a reference date
# nearest_date_before <- function(ref_date, ...){
#   dates <- na.omit(c(...))
#   dates <- dates[dates < ref_date]
#   if(length(dates) == 0) return(NA)
#   return(max(dates))
# }
# 
# # Function to find nearest date after a reference date
# nearest_date_after <- function(ref_date, ...){
#   dates <- na.omit(c(...))
#   dates <- dates[dates > ref_date]
#   if(length(dates) == 0) return(NA)
#   return(min(dates))
# }
# 
# # Apply functions row-wise to create new columns
# sero2 <- sero2 %>%
#   rowwise() %>%
#   mutate(
#     nearest_confirmed_before_S1 = nearest_date_before(collect_date_S1, confirm1_date, confirm2_date, confirm3_date),
#     nearest_confirmed_before_S2 = nearest_date_before(collect_date_S2, confirm1_date, confirm2_date, confirm3_date),
#     nearest_confirmed_after_S1 = nearest_date_after(collect_date_S1, confirm1_date, confirm2_date, confirm3_date)
#   ) %>%
#   ungroup()
# # sero2 %>% select(collect_date_S1, collect_date_S2, confirm1_date, confirm2_date, confirm3_date, nearest_confirmed_before_S1,
# #                 nearest_confirmed_before_S2, nearest_confirmed_after_S1)
# 
# sero2 <- sero2 %>%
#   mutate(collect_date_S1 = ymd(collect_date_S1),
#          collect_date_S2 = ymd(collect_date_S2),
#          gap_between_surveys = as.integer(collect_date_S2 - collect_date_S1), 
#          infec_gap_after_S1 = as.integer(nearest_confirmed_after_S1 - collect_date_S1), 
#          infec_gap_before_S1 = as.integer(collect_date_S1 - nearest_confirmed_before_S1), 
#          infec_gap_before_S2 = as.integer(collect_date_S2 - nearest_confirmed_before_S2), 
#          between_S1_S2_infec_cat = ifelse(is.na(collect_date_S2), NA, 
#                                           ifelse(is.na(infec_gap_after_S1), "no", 
#                                                  ifelse(!is.na(infec_gap_after_S1) & infec_gap_after_S1<=gap_between_surveys, 
#                                                         "yes", "no"))))
# # sero2 %>% select(collect_date_S1, collect_date_S2,gap_between_surveys,
# #                 infec_gap_after_S1, infec_gap_before_S1, infec_gap_before_S2, between_S1_S2_infec_cat)
# # table(sero2$between_S1_S2_infec_cat)
# # sero2 %>% filter(between_S1_S2_infec_cat == "yes" & N_cha_S2 == "Reactive")
# ################################################################################
# # vaccinations and gapes
# sero2 <- sero2 %>%
#   rowwise() %>%
#   mutate(
#     nearest_vac_before_S1 = nearest_date_before(collect_date_S1, vac1_date,vac2_date, vac3_date, vac4_date, vac5_date),
#     nearest_vac_before_S2 = nearest_date_before(collect_date_S2, vac1_date,vac2_date, vac3_date, vac4_date, vac5_date),
#     nearest_vac_after_S1 =  nearest_date_after (collect_date_S1, vac1_date,vac2_date, vac3_date, vac4_date, vac5_date)
#   ) %>%
#   ungroup()
# 
# sero2 <- sero2 %>%
#   mutate(collect_date_S1 = ymd(collect_date_S1),
#          collect_date_S2 = ymd(collect_date_S2),
#          vac_gap_after_S1 =  as.integer(nearest_vac_after_S1 - collect_date_S1), 
#          vac_gap_before_S1 = as.integer(collect_date_S1 - nearest_vac_before_S1), 
#          vac_gap_before_S2 = as.integer(collect_date_S2 - nearest_vac_before_S2)) %>% 
#   mutate(between_S1_S2_vac_cat = ifelse(is.na(collect_date_S2), NA, 
#                                         ifelse(is.na(vac_gap_before_S2), "no", 
#                                                ifelse(vac_gap_before_S1 > 0 & is.na(vac_gap_after_S1), "no", 
#                                                       ifelse(vac_gap_after_S1 <= gap_between_surveys, "yes", "no")))))
# # sero2[sample(1:dim(sero2)[1], 20), ] %>% select(collect_date_S1, collect_date_S2,
# #                                                 vac_gap_after_S1, vac_gap_before_S1, vac_gap_before_S2,
# #                                                 between_S1_S2_vac_cat)
# sero2 <- sero2 %>%
#   rowwise() %>%
#   mutate(vac_before_S1_freq = sum(c(vac1_date, vac2_date, vac3_date, vac4_date, vac5_date) < collect_date_S1, na.rm = TRUE)) %>%
#   ungroup()
# 
# # cope with vaccine type
# # sero2 %>% filter(vac1_type == "화이자(소아용)")
# convert_vaccine_type <- function(vac_type) {
#   ifelse(vac_type == "아스트라제네카", "AstraZeneca",
#          ifelse(vac_type %in% c("화이자", "화이자(소아용)", "화이자BA.4/5", "화이자BA.1"), "Pfizer",
#                 ifelse(vac_type %in% c("모더나", "모더나BA.4/5", "모더나BA.1"), "Moderna",
#                        ifelse(vac_type == "얀센", "Jansen",
#                               ifelse(vac_type %in% c("국외접종", "노바백스", "시험용 백신", "스카이코비원"), "Others", NA)))))
# }
# 
# # Apply the function to each vaccine type column
# sero2$vac1_type <- convert_vaccine_type(sero2$vac1_type)
# sero2$vac2_type <- convert_vaccine_type(sero2$vac2_type)
# sero2$vac3_type <- convert_vaccine_type(sero2$vac3_type)
# sero2$vac4_type <- convert_vaccine_type(sero2$vac4_type)
# sero2$vac5_type <- convert_vaccine_type(sero2$vac5_type)
# # sero2[c("S_num_S1", "wt_S1")]
# # sero2[c("age", "wt_age_S1")]
# # table(sero2$collect_date_S2)
# sero2 <- sero2 %>% 
#   mutate(S_num_S1 = gsub("<", "", S_num_S1), 
#          S_num_S1 = gsub(">", "", S_num_S1), 
#          N_num_S1 = gsub("<", "", N_num_S1), 
#          N_num_S1 = gsub("<", "", N_num_S1)) %>% 
#   mutate(S_num_S2 = gsub("<", "", S_num_S2), 
#          S_num_S2 = gsub(">", "", S_num_S2), 
#          N_num_S2 = gsub("<", "", N_num_S2), 
#          N_num_S2 = gsub("<", "", N_num_S2)) %>% 
#   mutate(S_num_S1 = as.numeric(S_num_S1), 
#          N_num_S1 = as.numeric(N_num_S1), 
#          S_num_S2 = as.numeric(S_num_S2), 
#          N_num_S2 = as.numeric(N_num_S2)) 
# 
# infec_index <- (sero2$between_S1_S2_infec_cat == "yes" 
#                 # & sero2$N_cha_S2 == "Reactive"  
#                 & !is.na(sero2$between_S1_S2_infec_cat) 
#                 # & !is.na(sero2$N_cha_S2)
# )  # population who reported infection during 1st and 2nd surveillance                                                                      # and show reactive N antibody level
# # sum(infec_index)
# 
# 
# 
# sero2$vac_S1 <- apply(sero2, 1, function(row) {
#   sum(!is.na(row[c('vac1_date', 
#                    'vac2_date', 
#                    'vac3_date', 
#                    'vac4_date')]) & 
#         row[c('vac1_date', 
#               'vac2_date', 
#               'vac3_date', 
#               'vac4_date')] < row['collect_date_S1'])
# })

# # sero2 %>% select('vac1_date', 'vac2_date', 'vac3_date', 'vac4_date', 'collect_date_S1', 'vac_S1')

# # if took vaccine after the first surveillance
# sero3 <- sero2 %>%
#   mutate(index = ifelse(is.na(vac_gap_after_S1), 
#                         FALSE, TRUE))
# 
# # group1 := took 3 doses before S1 and get infection 
# group1 <- sero3 %>% 
#   dplyr::filter(vac_S1 == 3) %>% 
#   dplyr::filter(!index) %>% 
#   dplyr::filter(age10 %in% c("60-69", "70-79", "80over")) %>% 
#   dplyr::mutate(group = "group1") %>% 
#   dplyr::select(GNO_S1, age, age10, sex, between_S1_S2_infec_cat, hosp_S2, group)
# 
# # group2 := took 3 doses before S1 and took the 4th dose and get infection afterwards
# group2 <- sero3 %>% 
#   dplyr::filter(vac_S1 == 3) %>% 
#   dplyr::filter(index) %>% 
#   dplyr::filter(age10 %in% c("60-69", "70-79", "80over")) %>% 
#   dplyr::mutate(group = "group2") %>% 
#   dplyr::select(GNO_S1, age, age10, sex, between_S1_S2_infec_cat, hosp_S2, group)
# 
# # rbind the group1 and group2 
# BMRP_Data <- rbind(group1, group2)
# 
# # further process the data
# BMRP_Data$sex <- factor(BMRP_Data$sex, 
#                         levels = c("Male", "Female"))
# 
# # unfortunately, there are lots of missing data here, we could only assume NA with no infection/hospitalization
# BMRP_Data$between_S1_S2_infec_cat <- ifelse(is.na(BMRP_Data$between_S1_S2_infec_cat), 
#                                             "no", 
#                                             BMRP_Data$between_S1_S2_infec_cat)
# 
# BMRP_Data$hosp_S2 <- ifelse(is.na(BMRP_Data$hosp_S2), 
#                             "no", 
#                             BMRP_Data$hosp_S2)
# 
# # if there is no infection, we assume that the hospitalizaiton is not ture if it exists 
# BMRP_Data$hosp_S2 <- ifelse(BMRP_Data$between_S1_S2_infec_cat == "no", 
#                             "no", 
#                             BMRP_Data$hosp_S2)
# 
# # save the data to .rds
# saveRDS(BMRP_Data, file = "Code/TempData/BMRP_Data.rds")

################################
####### End Process ############
################################

# load the processed data
url <- "Code/TempData/BMRP_Data.rds"
BMRP_Data <- readRDS(url)

# filter age >= 65
BMRP_Data <- BMRP_Data %>% filter(age >= 65)

# alter the group name
BMRP_Data$age10 <- ifelse(BMRP_Data$age10 < 70, "65-69", BMRP_Data$age10)

# Estimate propensity scores and perform matching
BMRP_Data$treatment <- ifelse(BMRP_Data$group == "group2", 1, 0) # treatment == 1 -> take the 4th booster

m.out <- matchit(treatment ~ age + sex, data = BMRP_Data, method = "nearest")
summary(m.out)
matched_data <- match.data(m.out)

# check the data 
# table(BMRP_Data$group)
# table(matched_data$group)

group1 <- matched_data %>% filter(group == "group1")
group2 <- matched_data %>% filter(group == "group2")

table(group1$between_S1_S2_infec_cat, group1$age10)











