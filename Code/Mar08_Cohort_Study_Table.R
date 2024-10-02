# # check the route directory
# list.files()

# sero <- openxlsx::read.xlsx("Data/df_S1S2_0329_forshare.xlsx")
# 
# sero$age <- round(sero$age)
# sero$vac1_date <- openxlsx::convertToDate(sero$vac1_date)
# sero$vac2_date <- openxlsx::convertToDate(sero$vac2_date)
# sero$vac3_date <- openxlsx::convertToDate(sero$vac3_date)
# sero$vac4_date <- openxlsx::convertToDate(sero$vac4_date)
# sero$vac5_date <- openxlsx::convertToDate(sero$vac5_date)
# sero$confirm1_date <- openxlsx::convertToDate(sero$confirm1_date)
# sero$confirm2_date <- openxlsx::convertToDate(sero$confirm2_date)
# sero$confirm3_date <- openxlsx::convertToDate(sero$confirm3_date)
# sero$collect_date_S1 <- lubridate::ymd(sero$collect_date_S1)
# sero$collect_time_S2 <- substr(sero$collect_time_S2, start = 1, stop = 10)
# sero$collect_date_S2 <- lubridate::ymd(sero$collect_time_S2)
# 
# sero2 <- sero[c("GNO_S1", "GNO_S2", "sex", "strata", 
#                 "age", "age10", 
#                 "edu", 
#                 "income", "income5", 
#                 "collect_date_S1", "collect_date_S2", 
#                 "confirm1_date", "confirm2_date", "confirm3_date", 
#                 "p21_q6_S1", "q9_S2", 
#                 "vac1_type", "vac2_type", "vac3_type", "vac4_type", "vac5_type", 
#                 "vac1_date", "vac2_date", "vac3_date", "vac4_date", "vac5_date", 
#                 "S_num_S1", "N_num_S1", "S_num_S2", "N_num_S2", 
#                 "S_cha_S1", "N_cha_S1", "S_cha_S2", "N_cha_S2", 
#                 "wt_S1", "wt_S2", "wt_age_S1", "wt_age_S2", 
#                 "p21_q2_1_S1", "p21_q2_2_S1", "p21_q2_3_S1", 
#                 "p21_q2_4_S1", "p21_q2_5_S1", "p21_q2_6_S1", 
#                 "p21_q2_7_S1", "p21_q2_8_S1", "p21_q2_9_S1", 
#                 "p21_q2_10_S1", "p21_q2_91_S1", 
#                 "q2_S2", 
#                 "p21_q1_S1", "q1_S2", 
#                 "q9_3_S2", "q9_3_day_S2", 
#                 "p21_q4_S1", "q6_S2")]
# 
# sero2 <- sero2 |> 
#   dplyr::rename(
#     hosp_S2 = "q9_3_S2",
#     hosp_dur_S2 = "q9_3_day_S2",
#     sym_S1_raw = "p21_q4_S1",
#     sym_S2_raw = "q6_S2",
#     confirm_S1 = "p21_q6_S1",
#     confirm_S2 = "q9_S2",
#     hypertension1 = "p21_q2_1_S1",
#     diabetes1 = "p21_q2_2_S1",
#     highcol1 = "p21_q2_3_S1",
#     cancer1 = "p21_q2_4_S1",
#     brainblood1 = "p21_q2_5_S1",
#     chronickidney1 = "p21_q2_6_S1",
#     chroniclung1 = "p21_q2_7_S1",
#     liver1 = "p21_q2_8_S1",
#     immunocomp1 = "p21_q2_9_S1",
#     rumatis1 = "p21_q2_10_S1",
#     other1 = "p21_q2_91_S1",
#     otherdisease_S2_raw = "q2_S2",
#     health_Status_S1 = "p21_q1_S1",
#     health_Status_S2 = "q1_S2"
#   ) |> 
#   dplyr::mutate(
#     otherdisease_S1 = ifelse(hypertension1 == 1 & 
#                                diabetes1 == 1 & 
#                                highcol1 == 1 & 
#                                cancer1 == 1 & 
#                                brainblood1 == 1 & 
#                                chronickidney1 == 1 & 
#                                chroniclung1 == 1 & 
#                                liver1 == 1 & 
#                                immunocomp1 == 1 & 
#                                rumatis1 == 1 & 
#                                other1 == 1, "no", "yes"), 
#     otherdisease_S2 = ifelse(otherdisease_S2_raw == 2, "no", "yes"),
#     sym_S1 = ifelse(sym_S1_raw == 1, "no", 
#                     ifelse(sym_S1_raw == 2, "yes", NA)), 
#     sym_S2 = ifelse(sym_S2_raw == 1, "yes", 
#                     ifelse(sym_S2_raw == 2, "no", NA)), 
#     hosp_S2 = ifelse(hosp_S2 == 1, "yes", "no"), 
#     confirm_S1 = ifelse(confirm_S1 == 2, "yes", "no"), 
#     confirm_S2 = ifelse(confirm_S2 == 1, "yes", "no")
#   ) |> 
#   dplyr::mutate(
#     group1 = dplyr::case_when(
#       (grepl("Positive", S_cha_S1) & grepl("Reactive", N_cha_S1)) ~ 1,
#       (grepl("Negative", S_cha_S1) & grepl("Reactive", N_cha_S1)) ~ 2,
#       (grepl("Positive", S_cha_S1) & grepl("Nonreactive", N_cha_S1)) ~ 3,
#       (grepl("Negative", S_cha_S1) & grepl("Nonreactive", N_cha_S1)) ~ 4,
#       TRUE ~ 0
#     ),
#     group2 = dplyr::case_when(
#       (grepl("Positive", S_cha_S2) & grepl("Reactive", N_cha_S2)) ~ 1,
#       (grepl("Negative", S_cha_S2) & grepl("Reactive", N_cha_S2)) ~ 2,
#       (grepl("Positive", S_cha_S2) & grepl("Nonreactive", N_cha_S2)) ~ 3,
#       (grepl("Negative", S_cha_S2) & grepl("Nonreactive", N_cha_S2)) ~ 4,
#       TRUE ~ 0
#     )
#   ) |> 
#   dplyr::mutate(
#     group11 = ifelse(grepl("1", group1), "inf",
#                      ifelse(grepl("2", group1), "inf",
#                             ifelse(grepl("3", group1), "vac", 
#                                    ifelse(grepl("4", group1), "naive", 
#                                           NA)))),
#     group22 = ifelse(grepl("1", group2), "inf",
#                      ifelse(grepl("2", group2), "inf",
#                             ifelse(grepl("3", group2), "vac", 
#                                    ifelse(grepl("4", group2), "naive", 
#                                           NA))))
#   )
# 
# drop_list <- c("otherdisease_S2_raw", "sym_S1_raw", "sym_S2_raw")
# sero2 <- sero2[, !(names(sero2) %in% drop_list)]
# 
# # Infections and gaps
# sero2 <- sero2 |>
#   dplyr::mutate(
#     across(ends_with("date"), lubridate::ymd, .names = "date_{.col}")
#   )
# 
# nearest_date_before <- function(ref_date, ...){
#   dates <- na.omit(c(...))
#   dates <- dates[dates < ref_date]
#   if(length(dates) == 0) return(NA)
#   return(max(dates))
# }
# 
# nearest_date_after <- function(ref_date, ...){
#   dates <- na.omit(c(...))
#   dates <- dates[dates > ref_date]
#   if(length(dates) == 0) return(NA)
#   return(min(dates))
# }
# 
# sero2 <- sero2 |>
#   dplyr::rowwise() |>
#   dplyr::mutate(
#     nearest_confirmed_before_S1 = nearest_date_before(collect_date_S1, confirm1_date, confirm2_date, confirm3_date),
#     nearest_confirmed_before_S2 = nearest_date_before(collect_date_S2, confirm1_date, confirm2_date, confirm3_date),
#     nearest_confirmed_after_S1 = nearest_date_after(collect_date_S1, confirm1_date, confirm2_date, confirm3_date)
#   ) |>
#   dplyr::ungroup()
# 
# sero2 <- sero2 |>
#   dplyr::mutate(collect_date_S1 = lubridate::ymd(collect_date_S1),
#                 collect_date_S2 = lubridate::ymd(collect_date_S2),
#                 gap_between_surveys = as.integer(collect_date_S2 - collect_date_S1), 
#                 infec_gap_after_S1 = as.integer(nearest_confirmed_after_S1 - collect_date_S1), 
#                 infec_gap_before_S1 = as.integer(collect_date_S1 - nearest_confirmed_before_S1), 
#                 infec_gap_before_S2 = as.integer(collect_date_S2 - nearest_confirmed_before_S2), 
#                 between_S1_S2_infec_cat = ifelse(is.na(collect_date_S2), NA, 
#                                                  ifelse(is.na(infec_gap_after_S1), "no", 
#                                                         ifelse(!is.na(infec_gap_after_S1) & infec_gap_after_S1 <= gap_between_surveys, 
#                                                                "yes", "no"))))
# 
# sero2 <- sero2 |>
#   dplyr::rowwise() |>
#   dplyr::mutate(
#     nearest_vac_before_S1 = nearest_date_before(collect_date_S1, vac1_date, vac2_date, vac3_date, vac4_date, vac5_date),
#     nearest_vac_before_S2 = nearest_date_before(collect_date_S2, vac1_date, vac2_date, vac3_date, vac4_date, vac5_date),
#     nearest_vac_after_S1 = nearest_date_after(collect_date_S1, vac1_date, vac2_date, vac3_date, vac4_date, vac5_date)
#   ) |>
#   dplyr::ungroup()
# 
# sero2 <- sero2 |>
#   dplyr::mutate(collect_date_S1 = lubridate::ymd(collect_date_S1),
#                 collect_date_S2 = lubridate::ymd(collect_date_S2),
#                 vac_gap_after_S1 = as.integer(nearest_vac_after_S1 - collect_date_S1), 
#                 vac_gap_before_S1 = as.integer(collect_date_S1 - nearest_vac_before_S1), 
#                 vac_gap_before_S2 = as.integer(collect_date_S2 - nearest_vac_before_S2)) |> 
#   dplyr::mutate(between_S1_S2_vac_cat = ifelse(is.na(collect_date_S2), NA, 
#                                                ifelse(is.na(vac_gap_before_S2), "no", 
#                                                       ifelse(vac_gap_before_S1 > 0 & is.na(vac_gap_after_S1), "no", 
#                                                              ifelse(vac_gap_after_S1 <= gap_between_surveys, "yes", "no")))))
# 
# sero2 <- sero2 |>
#   dplyr::rowwise() |>
#   dplyr::mutate(vac_before_S1_freq = sum(c(vac1_date, vac2_date, vac3_date, vac4_date, vac5_date) < collect_date_S1, na.rm = TRUE)) |>
#   dplyr::ungroup()
# 
# convert_vaccine_type <- function(vac_type) {
#   ifelse(vac_type == "아스트라제네카", "AstraZeneca",
#          ifelse(vac_type %in% c("화이자", "화이자(소아용)", "화이자BA.4/5", "화이자BA.1"), "Pfizer",
#                 ifelse(vac_type %in% c("모더나", "모더나BA.4/5", "모더나BA.1"), "Moderna",
#                        ifelse(vac_type == "얀센", "Jansen",
#                               ifelse(vac_type %in% c("국외접종", "노바백스", "시험용 백신", "스카이코비원"), "Others", NA)))))
# }
# 
# sero2$vac1_type <- convert_vaccine_type(sero2$vac1_type)
# sero2$vac2_type <- convert_vaccine_type(sero2$vac2_type)
# sero2$vac3_type <- convert_vaccine_type(sero2$vac3_type)
# sero2$vac4_type <- convert_vaccine_type(sero2$vac4_type)
# sero2$vac5_type <- convert_vaccine_type(sero2$vac5_type)
# 
# sero2 <- sero2 |>
#   dplyr::mutate(S_num_S1 = gsub("<", "", S_num_S1), 
#                 S_num_S1 = gsub(">", "", S_num_S1), 
#                 N_num_S1 = gsub("<", "", N_num_S1), 
#                 N_num_S1 = gsub(">", "", N_num_S1)) |>
#   dplyr::mutate(S_num_S2 = gsub("<", "", S_num_S2), 
#                 S_num_S2 = gsub(">", "", S_num_S2), 
#                 N_num_S2 = gsub("<", "", N_num_S2), 
#                 N_num_S2 = gsub(">", "", N_num_S2)) |>
#   dplyr::mutate(S_num_S1 = as.numeric(S_num_S1), 
#                 N_num_S1 = as.numeric(N_num_S1), 
#                 S_num_S2 = as.numeric(S_num_S2), 
#                 N_num_S2 = as.numeric(N_num_S2))

# # save the dataset to 'processed_df_S1S2.rds'
# saveRDS(sero2, file = "Code/TempData/processed_df_S1S2.rds")




##################################################################
# # Define the infection and non-infection group
# # notice that the criteria might differ from the cased study
url <- "Code/TempData/processed_df_S1S2.rds"
sero2 <- readRDS(url)


# Define the infection index
infec_index <- sero2$between_S1_S2_infec_cat == "yes" & !is.na(sero2$between_S1_S2_infec_cat)

# Split the data into infected and non-infected groups
infec <- sero2[infec_index, ]
non_infec <- sero2[!infec_index, ]

# Apply infection rule 1: Exclude those with Nonreactive or missing N_cha_S2
# infec <- infec[!(infec$N_cha_S2 == "Nonreactive" | is.na(infec$N_cha_S2)), ]

# Apply non-infection rule 1: Exclude those who didn't attend the 2nd surveillance
non_infec <- non_infec[!is.na(non_infec$collect_date_S2), ]

# Combine infected and non-infected groups and add infection category
infec$infec_cat <- "infec"
non_infec$infec_cat <- "non_infec"
df <- rbind(infec, non_infec)









# Define induced_index
df <- df |>
  dplyr::mutate(induced_index = dplyr::case_when(
    group1 == 4 ~ "naive",
    group1 == 3 & vac_before_S1_freq > 0 & is.na(infec_gap_before_S1) ~ "vac-induced",
    group1 == 1 & vac_before_S1_freq > 0 ~ "hybrid-induced",
    group1 == 2 ~ "infec-induced",
    TRUE ~ NA_character_
  ))

df$induced_index <- factor(df$induced_index, levels = c("hybrid-induced", "vac-induced", "infec-induced", "naive"))

# Define age categories
df$age_cat <- cut(df$age,
                  breaks = c(-Inf, 19, 39, 59, 79, Inf),
                  labels = c("<20", "20-39", "40-59", "60-79", "80+"),
                  right = FALSE)
df$age_cat <- factor(df$age_cat, levels = c("<20", "20-39", "40-59", "60-79", "80+"))

# Define sex, education, and income categories
df$sex <- factor(df$sex, levels = c("Male", "Female"))
df$edu <- factor(df$edu, levels = c("Primary School", "Middle/High School", "Postsecondary"))
df$income_cat <- cut(df$income,
                     breaks = c(-Inf, 3000, 6000, 9000, Inf),
                     labels = c("<3000", "3000-6000", "6000-9000", ">9000"),
                     right = FALSE,
                     include.lowest = TRUE,
                     addNA = TRUE)
df$income_cat <- factor(df$income_cat, levels = c("<3000", "3000-6000", "6000-9000", ">9000"))

# Define other disease status
df$otherdisease_S1 <- factor(df$otherdisease_S1, levels = c("yes", "no"))

# Calculate latest immunology and categorize
df <- df |>
  dplyr::mutate(latest_immunology = pmin(
    dplyr::coalesce(vac_gap_before_S1, Inf),
    dplyr::coalesce(infec_gap_before_S1, Inf),
    na.rm = TRUE
  ))

df <- df |>
  dplyr::mutate(latest_immunology_cat = dplyr::case_when(
    latest_immunology >= 0 & latest_immunology <= 30 ~ "<1 month",
    latest_immunology > 30 & latest_immunology <= 180 ~ "1-6 months",
    latest_immunology > 180 & latest_immunology <= 365 ~ "6-12 months",
    latest_immunology > 365 ~ ">1 year",
    TRUE ~ "no_event"
  ))

df$latest_immunology_cat <- factor(df$latest_immunology_cat, levels = c("<1 month", "1-6 months", "6-12 months", ">1 year", "no_event"))

# Define event after S1
df <- df |>
  dplyr:: mutate(
    event_after_S1 = dplyr::case_when(
      infec_cat == "infec" & hosp_S2 == "yes" ~ "infec_hosp", 
      infec_cat == "infec" & hosp_S2 == "no" ~ "infec", 
      infec_cat == "infec" & is.na(hosp_S2) ~ "infec", 
      infec_cat == "non_infec" ~ "no_events"
    )
  )

df$event_after_S1 <- factor(df$event_after_S1, 
                            levels = c("infec_hosp", "infec", "no_events"))

table(df$induced_index)
##################################################################
# filling the tables 
table(df$event_after_S1, df$induced_index)

vac_group <- df |> dplyr::filter(induced_index == "vac-induced")
hyb_group <- df |> dplyr::filter(induced_index == "hybrid-induced")

table(vac_group$event_after_S1)






