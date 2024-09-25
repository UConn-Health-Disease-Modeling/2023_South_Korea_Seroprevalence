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
library(plotROC)
library(purrr)
library(DMwR2)
library(smotefamily)
library(ROSE)
library(gridExtra)

# ################################
# ####### Data Process ###########
# ################################

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
# 
# infec     <- sero2[infec_index, ]
# non_infec <- sero2[!infec_index, ]
# 
# ### infec_rule1: exclude those who had N_cha_S2 Nonreactive or missing 
# infec_rule1 <- (infec$N_cha_S2 == "Nonreactive" | is.na(infec$N_cha_S2))
# infec <- infec[!infec_rule1, ]
# 
# ### non_infec_rule1: exclude those who didn't attend the 2nd surveillance
# non_infec_rule1 <- is.na(non_infec$collect_date_S2)
# non_infec <- non_infec[!non_infec_rule1, ]
# 
# ## combine them together 
# infec$infec_cat <- "infec"
# non_infec$infec_cat <- "non_infec"
# 
# df <- rbind(infec, non_infec)
# 
# df <- df %>% mutate(induced_index = ifelse(group1 == 4, "naive", 
#                                            ifelse(group1 == 3 & vac_before_S1_freq > 0 & 
#                                                     is.na(infec_gap_before_S1), "vac-induced",
#                                                   ifelse(group1 == 1 & vac_before_S1_freq > 0, "hybrid-induced", 
#                                                          ifelse(group1 == 2 , "infec-induced", NA)))))
# 
# df$induced_index <- factor(df$induced_index, levels = c("hybrid-induced", "vac-induced", "infec-induced", "naive"))
# table(df$induced_index)
# # df$age_cat <- cut(df$age,
# #                      breaks = c(-Inf, 40, Inf),
# #                      labels = c("<40", ">=40"),
# #                      right = FALSE)
# # df$age_cat <- factor(df$age_cat, levels = c("<40", ">=40"))
# # table(df$induced_index)
# 
# df$age_cat <- cut(df$age,
#                   breaks = c(-Inf, 19, 39, 59, 79, Inf),
#                   labels = c("<20", "20-39", "40-59", "60-79", "80+"),
#                   right = FALSE)
# df$age_cat <- factor(df$age_cat, levels = c("<20", "20-39", "40-59", "60-79", "80+"))
# df$sex <- factor(df$sex, levels = c("Male", "Female"))
# df$edu <- factor(df$edu, levels = c("Primary School", "Middle/High School", "Postsecondary"))
# df$income_cat <- cut(df$income,
#                      breaks = c(-Inf, 3000, 6000, 9000, Inf),
#                      labels = c("<3000", "3000-6000", "6000-9000", ">9000"),
#                      right = FALSE,
#                      include.lowest = TRUE,
#                      addNA = TRUE)
# df$income_cat <- factor(df$income_cat, levels = c("<3000", "3000-6000", "6000-9000", ">9000"))
# df$otherdisease_S1 <- factor(df$otherdisease_S1, levels = c("yes", "no"))
# 
# df <- df %>% mutate(latest_immunology = ifelse(is.na(vac_gap_before_S1), infec_gap_before_S1,
#                                                ifelse(is.na(infec_gap_before_S1), vac_gap_before_S1, 
#                                                       ifelse(vac_gap_before_S1 < infec_gap_before_S1, 
#                                                              vac_gap_before_S1, infec_gap_before_S1))))
# df <- df %>% mutate(latest_immunology_cat = ifelse(latest_immunology >=0 & latest_immunology <=30, "<1 month", 
#                                                    ifelse(latest_immunology > 30 & 
#                                                             latest_immunology <= 180, "1-6 months", 
#                                                           ifelse(latest_immunology > 180 & 
#                                                                    latest_immunology <= 365, "6-12 months", ">1 year"))))
# df[is.na(df$latest_immunology_cat), ]$latest_immunology_cat <- "no_event"
# df$latest_immunology_cat <- factor(df$latest_immunology_cat, levels = c("<1 month", "1-6 months", "6-12 months", 
#                                                                         ">1 year", "no_event"))
# 
# df <- df %>% 
#   mutate(event_after_S1 = case_when(
#     infec_cat == "infec" & between_S1_S2_vac_cat == "yes" ~ "infec_vac", 
#     infec_cat == "infec" & between_S1_S2_vac_cat == "no"  ~ "infec", 
#     infec_cat == "non_infec" & between_S1_S2_vac_cat == "yes" ~ "vac", 
#     infec_cat == "non_infec" & between_S1_S2_vac_cat == "no" ~ "no_event"))
# 
# df$pre_infec_S1 <- ifelse(is.na(df$infec_gap_before_S1), "no", "yes")
# df$pre_infec_S1 <- factor(df$pre_infec_S1, levels = c("yes","no"))
# 
# df$event_after_S1 <- factor(df$event_after_S1, levels = c("infec", "vac", "infec_vac", "no_event"))
# 
# df1 <- df %>% filter(!is.na(event_after_S1) & !is.na(infec_cat) & !is.na(induced_index)) %>% 
#   filter(induced_index != "naive") %>%
#   filter(induced_index != "infec-induced")
# 
# df1$induced_index <- factor(df1$induced_index, levels = c("vac-induced", "hybrid-induced"))
# 
# 
# df1 <- df1 %>%
#   mutate(age_category = case_when(
#     age < 20 ~ "<20",
#     age >= 20 & age <= 40 ~ "20-40",
#     age > 40 & age <= 60 ~ "40-60",
#     age > 60 ~ ">60"
#   ))
# df1$age_category <- factor(df1$age_category, levels = c("<20", "20-40", "40-60", ">60"))
# 
# case_index <- read.csv("Code/TempData/Mar17_Case_Study_Data.csv") %>% select(GNO_S1, case_study)
# df2 <- df1 %>% left_join(case_index, by = "GNO_S1") %>% filter(!is.na(case_study)) %>% 
#   filter(case_study == "case")
# 
# columns_of_interest <- paste0("S2_sym", 1:23)
# df2$S2_sym_num <- apply(df2[, columns_of_interest], 1, function(x) sum(!is.na(x)))
# df2$hosp_S2[is.na(df2$hosp_S2)] = "no"
# 
# df2$severe <- ifelse(df2$hosp_S2 == 1 | df2$S2_sym_num >= 1, 1, 0)

# save the data to .rds
# saveRDS(df2, file = "Code/TempData/SevereSymptomAnalysis.rds")

# ################################
# ####### End Process ############
# ################################

url <- "Code/TempData/SevereSymptomAnalysis.rds"
df <- readRDS(url)

df3 <- df %>% filter(induced_index == "hybrid-induced")
df4 <- df %>% filter(induced_index == "vac-induced")

df3 <- df2 %>% select(age_category, sex, hosp_S2, S_num_S1, severe, induced_index)

inf <- df3 %>% filter(induced_index == "hybrid-induced")
vac <- df3 %>% filter(induced_index == "vac-induced")
cat("hybrid \n")
table(inf$hosp_S2, inf$age_category)
cat("vaccine \n")
table(vac$hosp_S2, vac$age_category)


table(df3$hosp_S2, df3$induced_index)


model <- glm(severe ~ age_category + sex + induced_index, family = "binomial", data = df3)
summary(model)

