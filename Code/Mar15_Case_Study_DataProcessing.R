# load packages
library(dplyr)
library(lubridate)
library(tidyr)
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

## LOAD and PREPROCESSING THE .xlsx FILE 
## output: sero2 (including all 9,945 cases in the first surveillance)

# features added: 
# (nearest_confirmed_before_S1, nearest_confirmed_before_S2, nearest_confirmed_after_S1, gap_between_surveys, 
# infec_gap_after_S1, infec_gap_before_S1, infec_gap_before_S2, between_S1_S2_infec_cat; vac_gap_after_S1, 
# vac_gap_before_S1, vac_gap_before_S2, between_S1_S2_vac_cat)

# ######################
# ## Process the data ##
# ######################

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
# infections and gaps
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
# 
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
# ## Case study cohort creating:
# # 03/25 adding the variable "immnology_type";
# # Output: "Code/TempData/Mar17Case_Study_Data.csv"
# 
# case_index <- (sero2$between_S1_S2_infec_cat == "yes"
#                # & sero2$N_cha_S2 == "Reactive"
#                & !is.na(sero2$between_S1_S2_infec_cat)
#                # & !is.na(sero2$N_cha_S2)
# )  # population who reported infection during 1st and 2nd surveillance                                                                      # and show reactive N antibody level
# 
# case    <- sero2[case_index, ]
# control <- sero2[!case_index, ]
#
#
# ### case_rule1: exclude those who had N_cha_S2 Nonreactive or missing
# case_rule1 <- (case$N_cha_S2 == "Nonreactive" | is.na(case$N_cha_S2))
# case <- case[!case_rule1, ]
#
# ### case_rule2: exclude those who had vaccination before the infeciton during S1 and S2 collect time
# case_rule2 <- (case$infec_gap_after_S1 > case$vac_gap_after_S1 & !is.na(case$infec_gap_after_S1) & !is.na(case$vac_gap_after_S1))
# case <- case[!case_rule2, ]
#
#
#
#
# ### control_rule1: exclude those who didn't attend the 2nd surveillance
# control_rule1 <- is.na(control$collect_date_S2)
# control <- control[!control_rule1, ]
#
# ### control_rule2: exclude those who had missing S_num_S2 or S_num_S1
# control_rule2 <- (is.na(control$S_num_S1) | is.na(control$S_num_S2))
# control <- control[!control_rule2, ]
#
# ## control_rule3: exclude those who had vaccination between S1 and S2
# control_rule3 <- control$between_S1_S2_vac_cat == "yes"
# control <- control[!control_rule3, ]
#
# ## control_rule4: exclude those who had higher S antibody level at S2 than S1
# control_rule4 <- (control$S_num_S2 >= control$S_num_S1)
# control <- control[!control_rule4, ]
#
#
#
#
#
# ## combine them together
# case$case_study <- "case"
# control$case_study <- "control"
#
# df <- rbind(case, control)
#
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
# summary(df$N_num_S1)
#
# # apply the rules used in "Mar08_Cohort_Study_Design.rmd"
# df <- df %>% mutate(induced_index = ifelse(group1 == 4, "naive",
#                                            ifelse(group1 == 3 & vac_before_S1_freq > 0 & is.na(infec_gap_before_S1), "vac-induced",
#                                                   ifelse(group1 == 1 & vac_before_S1_freq > 0, "hybrid-induced", NA))))
# df$induced_index <- factor(df$induced_index, levels = c("hybrid-induced", "vac-induced", "naive"))

# # save the data.frame (to csv)
# write.csv(df, "Code/TempData/Mar17Case_Study_Data.csv")

# ######################
# ######## end #########
# ######################


## Further on the Data (filling the "_zfranky" form and perform chi-squared test to check distribution)
# read the .csv file
url <- "Code/TempData/Mar17_Case_Study_Data.csv"
df <- read.csv(url)

df$pre_infec_S1 <- ifelse(is.na(df$infec_gap_before_S1), "no", "yes")
df$pre_infec_S1 <- factor(df$pre_infec_S1, levels = c("yes","no"))

df$S_num_S1_cat <- cut(df$S_num_S1,
                       breaks = c(-Inf, 6000, 15000, 24000, Inf),
                       labels = c("<6000", "6000-15000", "15000-24000", ">=24000"),
                       right = FALSE,
                       include.lowest = TRUE,
                       addNA = TRUE)

df$N_num_S1_cat <- cut(df$N_num_S1, 
                       breaks = c(-Inf, 1, 10, 20, Inf),
                       labels = c("<1", "1-10", "10-20", ">=20"),
                       right = FALSE,
                       include.lowest = TRUE,
                       addNA = TRUE)

df$case_study <- factor(df$case_study, levels = c("control", "case"))
table(df$N_num_S1_cat, df$case_study)

