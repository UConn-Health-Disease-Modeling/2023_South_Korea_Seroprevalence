library(dplyr)
library(tidyr)
library(lubridate)



# ######################
# ## Process the data ##
# ######################

# Load the dataset
url_S_12 <- "Data/datashare_240902/df_S1S2_240827.xlsx"

## Processing S_12
S_12 <- readxl::read_excel(url_S_12) |> 
  dplyr::select(starts_with("GNO"), sex, 
                starts_with("age"),
                starts_with("N_"), starts_with("S_"), 
                starts_with("vax"), starts_with("conf"), 
                starts_with("collect"), 
                starts_with("p21_q2"), "q2_S2", 
                "p21_q4_S1", "q6_S2", "q9_3_S2") |> 
  dplyr::rename(
    hypertension1    = "p21_q2_1_S1",     
    diabetes1        = "p21_q2_2_S1",
    highcol1         = "p21_q2_3_S1",
    cancer1          = "p21_q2_4_S1",
    brainblood1      = "p21_q2_5_S1",
    chronickidney1   = "p21_q2_6_S1",
    chroniclung1     = "p21_q2_7_S1",
    liver1           = "p21_q2_8_S1",
    immunocomp1      = "p21_q2_9_S1",
    rumatis1         = "p21_q2_10_S1",
    other1           = "p21_q2_91_S1", 
    otherdisease_S2_raw = "q2_S2", 
    sym_S1_raw       = "p21_q4_S1",
    sym_S2_raw       = "q6_S2", 
    vax.date5        = "vax.date.bi",
    hosp.S2          = "q9_3_S2"
  ) |> 
  # Create new variables for disease status and symptoms
  dplyr::mutate(
    otherdisease_S1 = if_else(
      hypertension1 == 1 & diabetes1 == 1 & highcol1 == 1 & cancer1 == 1 &
        brainblood1 == 1 & chronickidney1 == 1 & chroniclung1 == 1 &
        liver1 == 1 & immunocomp1 == 1 & rumatis1 == 1 & other1 == 1, 
      "no", "yes"
    ),
    otherdisease_S2 = if_else(otherdisease_S2_raw == 2, "no", "yes"),
    sym_S1 = case_when(
      sym_S1_raw == 1 ~ "no",
      sym_S1_raw == 2 ~ "yes",
      TRUE ~ NA_character_
    ),
    sym_S2 = case_when(
      sym_S2_raw == 1 ~ "yes",
      sym_S2_raw == 2 ~ "no",
      TRUE ~ NA_character_
    )
  ) |> 
  # Categorize groups based on S and N test results
  dplyr::mutate(
    group1 = case_when(
      grepl("Positive", S_cha_S1) & grepl("Reactive", N_cha_S1) ~ 1,  # S+N+ Inf(vac)
      grepl("Negative", S_cha_S1) & grepl("Reactive", N_cha_S1) ~ 2,  # S-N+ Inf
      grepl("Positive", S_cha_S1) & grepl("Nonreactive", N_cha_S1) ~ 3,  # S+N- Vac
      grepl("Negative", S_cha_S1) & grepl("Nonreactive", N_cha_S1) ~ 4,  # S-N- Naive
      TRUE ~ 0
    ),
    group2 = case_when(
      grepl("Positive", S_cha_S2) & grepl("Reactive", N_cha_S2) ~ 1,  # S+N+ Inf(vac)
      grepl("Negative", S_cha_S2) & grepl("Reactive", N_cha_S2) ~ 2,  # S-N+ Inf
      grepl("Positive", S_cha_S2) & grepl("Nonreactive", N_cha_S2) ~ 3,  # S+N- Vac
      grepl("Negative", S_cha_S2) & grepl("Nonreactive", N_cha_S2) ~ 4,  # S-N- Naive
      TRUE ~ 0
    )
  ) |> 
  # Group the results as "inf", "vac", or "naive" based on the values of group1 and group2
  dplyr::mutate(
    group11 = case_when(
      group1 %in% c(1, 2) ~ "inf",
      group1 == 3 ~ "vac",
      group1 == 4 ~ "naive",
      TRUE ~ NA_character_
    ),
    group22 = case_when(
      group2 %in% c(1, 2) ~ "inf",
      group2 == 3 ~ "vac",
      group2 == 4 ~ "naive",
      TRUE ~ NA_character_
    )
  )

colnames(S_12)
# convertToDate on vac date
S_12$vax.date1 <- substr(S_12$vax.date1, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$vax.date2 <- substr(S_12$vax.date2, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$vax.date3 <- substr(S_12$vax.date3, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$vax.date4 <- substr(S_12$vax.date4, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$vax.date5 <- substr(S_12$vax.date5, 1, 10) |> as.Date(format = "%Y-%m-%d")

# convertToDate on confirm date
S_12$conf_date1 <- substr(S_12$conf_date1, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$conf_date2 <- substr(S_12$conf_date2, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$conf_date3 <- substr(S_12$conf_date3, 1, 10) |> as.Date(format = "%Y-%m-%d")

S_12$collect_date_S2 <- substr(S_12$collect_time_S2, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$collect_date_S1 <- as.Date(S_12$collect_date_S1, format = "%Y-%m-%d")

# Processing the missing missing collect_date_S2 
mean_date <- mean(S_12$collect_date_S2, na.rm = TRUE)
S_12$collect_date_S2[is.na(S_12$collect_date_S2)] <- mean_date

# Function to find nearest date before a reference date
nearest_date_before <- function(ref_date, ...){
  dates <- na.omit(c(...))
  dates <- dates[dates < ref_date]
  if(length(dates) == 0) return(NA)
  return(max(dates))
}

# Function to find nearest date after a reference date
nearest_date_after <- function(ref_date, ...){
  dates <- na.omit(c(...))
  dates <- dates[dates > ref_date]
  if(length(dates) == 0) return(NA)
  return(min(dates))
}

# Apply functions row-wise to create new columns
S_12 <- S_12 |>
  rowwise() |>
  mutate(
    nearest_confirmed_before_S1 = nearest_date_before(collect_date_S1, conf_date1, conf_date2, conf_date3),
    nearest_confirmed_before_S2 = nearest_date_before(collect_date_S2, conf_date1, conf_date2, conf_date3),
    nearest_confirmed_after_S1 = nearest_date_after(collect_date_S1, conf_date1, conf_date2, conf_date3)
  ) |>
  ungroup()

# check if nearest date is good. 
# S_12 |> select(collect_date_S1, collect_date_S2, conf_date1, conf_date2, conf_date3, 
#                nearest_confirmed_before_S1, nearest_confirmed_before_S2, nearest_confirmed_after_S1) |> head(20)

S_12 <- S_12 |>
  mutate(gap_between_surveys = as.integer(collect_date_S2 - collect_date_S1),
         infec_gap_after_S1 = as.integer(nearest_confirmed_after_S1 - collect_date_S1),
         infec_gap_before_S1 = as.integer(collect_date_S1 - nearest_confirmed_before_S1),
         infec_gap_before_S2 = as.integer(collect_date_S2 - nearest_confirmed_before_S2),
         between_S1_S2_infec_cat = ifelse(is.na(collect_date_S2), NA,
                                          ifelse(is.na(infec_gap_after_S1), "no",
                                                 ifelse(!is.na(infec_gap_after_S1) & infec_gap_after_S1<=gap_between_surveys,
                                                        "yes", "no"))))

S_12 <- S_12 |>
  rowwise() |>
  mutate(
    nearest_vac_before_S1 = nearest_date_before(collect_date_S1, vax.date1, vax.date2,  vax.date3,  vax.date4,  vax.date5),
    nearest_vac_before_S2 = nearest_date_before(collect_date_S2, vax.date1, vax.date2,  vax.date3,  vax.date4,  vax.date5),
    nearest_vac_after_S1 =  nearest_date_after (collect_date_S1, vax.date1, vax.date2,  vax.date3,  vax.date4,  vax.date5)
  ) |>
  ungroup()

S_12 <- S_12 |>
  mutate(vac_gap_after_S1 =  as.integer(nearest_vac_after_S1 - collect_date_S1),
         vac_gap_before_S1 = as.integer(collect_date_S1 - nearest_vac_before_S1),
         vac_gap_before_S2 = as.integer(collect_date_S2 - nearest_vac_before_S2)) |>
  mutate(between_S1_S2_vac_cat = ifelse(is.na(collect_date_S2), NA,
                                        ifelse(is.na(vac_gap_before_S2), "no",
                                               ifelse(vac_gap_before_S1 > 0 & is.na(vac_gap_after_S1), "no",
                                                      ifelse(vac_gap_after_S1 <= gap_between_surveys, "yes", "no")))))

S_12 |> select(collect_date_S1, collect_date_S2, vax.date1, vax.date2,  vax.date3,  vax.date4,  vax.date5, 
               between_S1_S2_vac_cat) -> test

S_12 <- S_12 |>
  rowwise() |>
  mutate(vac_before_S1_freq = sum(c(vax.date1, vax.date2,  vax.date3,  vax.date4,  vax.date5) < collect_date_S1, na.rm = TRUE)) |>
  ungroup()

S_12 <- S_12 |>
  mutate(S_num_S1 = gsub("<", "", S_num_S1),
         S_num_S1 = gsub(">", "", S_num_S1),
         N_num_S1 = gsub("<", "", N_num_S1),
         N_num_S1 = gsub("<", "", N_num_S1)) |>
  mutate(S_num_S2 = gsub("<", "", S_num_S2),
         S_num_S2 = gsub(">", "", S_num_S2),
         N_num_S2 = gsub("<", "", N_num_S2),
         N_num_S2 = gsub("<", "", N_num_S2)) |>
  mutate(S_num_S1 = as.numeric(S_num_S1),
         N_num_S1 = as.numeric(N_num_S1),
         S_num_S2 = as.numeric(S_num_S2),
         N_num_S2 = as.numeric(N_num_S2))

# ######################
# ######## end #########
# ######################

# save_url <- "Code/TempData/Oct09_processed_S12.rds"
# saveRDS(S_12, save_url)


# flow chart excluding criteria 1: excluding those with vaccination during S1 and S2 
S_12.v2 <- S_12 |> filter(between_S1_S2_vac_cat == "no")

# dim(S_12)[1] - dim(S_12.v2)[1] # number of exclusion 1 

S_12.v2 <- S_12.v2 |> mutate(immune_type = ifelse(group1 == 4, "naive",
                                                  ifelse(group1 == 3, "vac-induced",
                                                         ifelse(group1 == 2, "inf-induced",
                                                                ifelse(group1 == 1, "hybrid-induced", NA)))))
S_12.v2$immune_type <- factor(S_12.v2$immune_type, levels = c("hybrid-induced", "vac-induced", "inf-induced", "naive"))

S_12.vac    <- S_12.v2 |> filter(immune_type == "vac-induced") |> filter(vac_before_S1_freq > 0)
S_12.hybrid <- S_12.v2 |> filter(immune_type == "hybrid-induced") |> filter(vac_before_S1_freq > 0)

# (S_12.vac$between_S1_S2_infec_cat |> table())/dim(S_12.vac)[1]
# (S_12.hybrid$between_S1_S2_infec_cat |> table())/dim(S_12.hybrid)[1]

cox_hazard_data <- rbind(S_12.vac, S_12.hybrid)









# fill the tables (using the cox-harzard data)
cox_hazard_data$age_cat <- cut(cox_hazard_data$age_base,
                               breaks = c(-Inf, 19, 39, 59, 79, Inf),
                               labels = c("<20", "20-39", "40-59", "60-79", "80+"),
                               right = FALSE)
cox_hazard_data$age_cat <- factor(cox_hazard_data$age_cat, levels = c("<20", "20-39", "40-59", "60-79", "80+"))
cox_hazard_data$sex <- factor(cox_hazard_data$sex, levels = c("Male", "Female"))
cox_hazard_data$otherdisease_S1 <- factor(cox_hazard_data$otherdisease_S1, levels = c("yes", "no"))
cox_hazard_data <- cox_hazard_data %>% mutate(latest_immunology = ifelse(is.na(vac_gap_before_S1), infec_gap_before_S1,
                                                                         ifelse(is.na(infec_gap_before_S1), vac_gap_before_S1,
                                                                                ifelse(vac_gap_before_S1 < infec_gap_before_S1,
                                                                                       vac_gap_before_S1, infec_gap_before_S1))))
cox_hazard_data <- cox_hazard_data %>% mutate(latest_immunology_cat = ifelse(latest_immunology >=0 & latest_immunology <=30, "<1 month",
                                                                             ifelse(latest_immunology > 30 &
                                                                                      latest_immunology <= 180, "1-6 months",
                                                                                    ifelse(latest_immunology > 180 &
                                                                                             latest_immunology <= 365, "6-12 months", ">1 year"))))

cox_hazard_data[is.na(cox_hazard_data$latest_immunology_cat), ]$latest_immunology_cat <- "no_event"
cox_hazard_data$S_num_S1_cat <- cut(cox_hazard_data$S_num_S1,
                                    breaks = c(-Inf, 6000, 15000, 24000, Inf),
                                    labels = c("<6000", "6000-15000", "15000-24000", ">=24000"),
                                    right = FALSE,
                                    include.lowest = TRUE,
                                    addNA = TRUE)
cox_hazard_data$N_num_S1_cat <- cut(cox_hazard_data$N_num_S1, 
                                    breaks = c(-Inf, 1, 10, 20, Inf),
                                    labels = c("<1", "1-10", "10-20", ">=20"),
                                    right = FALSE,
                                    include.lowest = TRUE,
                                    addNA = TRUE)

cox_hazard_data <- cox_hazard_data |>
  dplyr:: mutate(
    event_after_S1 = dplyr::case_when(
      between_S1_S2_infec_cat == "yes" & hosp.S2 == "yes" ~ "infec_hosp", 
      between_S1_S2_infec_cat == "yes" & hosp.S2 == "no" ~ "infec", 
      between_S1_S2_infec_cat == "yes" & is.na(hosp.S2) ~ "infec", 
      between_S1_S2_infec_cat == "no" ~ "no_events"
    )
  )
cox_hazard_data$latest_immunology_cat <- factor(cox_hazard_data$latest_immunology_cat, levels = c("<1 month", "1-6 months", "6-12 months", ">1 year"))

cox_hazard_data <- cox_hazard_data |> mutate(age_cat = case_when(
  age_base < 20 ~ "<20",
  age_base >= 20 & age_base <= 40 ~ "20-40",
  age_base > 40 & age_base <= 60 ~ "40-60",
  age_base > 60 ~ ">60"
))

cox_hazard_data.vac    <- cox_hazard_data |> filter(immune_type == "vac-induced")
cox_hazard_data.hybrid <- cox_hazard_data |> filter(immune_type == "hybrid-induced")

# fill the table1 
cox_hazard_data.hybrid$age_cat |> table()

# # save cox hazad data
# Cox_Hazard.url <- "Code/TempData/cox_hazard_data.rds"
# saveRDS(cox_hazard_data, Cox_Hazard.url)
# 
# cox_hazard_data <- readRDS(Cox_Hazard.url)

wilcox.test(cox_hazard_data.vac$vac_before_S1_freq, cox_hazard_data.hybrid$vac_before_S1_freq)

cox_hazard_data.vac.infec <- cox_hazard_data.vac %>% filter(between_S1_S2_infec_cat == "yes")
table(cox_hazard_data.vac.infec$age_cat, cox_hazard_data.vac.infec$hosp.S2)




regression_data <- cox_hazard_data

# regression_data |> filter(N_cha_S1 == "Nonreactive" & N_cha_S2 == "Reactive") |> dim()
# regression_data |> filter(N_cha_S1 == "Nonreactive" & N_cha_S2 == "Reactive" & between_S1_S2_infec_cat == "yes") |> dim()

# get the undeteced cases 
# undetected <- regression_data |> 
#   filter(N_cha_S1 == "Nonreactive" & N_cha_S2 == "Reactive" & between_S1_S2_infec_cat == "no")
# undetected.url <- "Code/TempData/undetected_cases.rds"
# saveRDS(undetected, undetected.url)

regression_data$between_S1_S2_infec_cat[regression_data$N_cha_S1 == "Nonreactive" & regression_data$N_cha_S2 == "Reactive"] <- "yes"


regression.vac    <- regression_data |> filter(immune_type == "vac-induced")
regression.hybrid <- regression_data |> filter(immune_type == "hybrid-induced")

# table(regression.vac$between_S1_S2_infec_cat)
# table(regression.hybrid$between_S1_S2_infec_cat)
# table(regression.vac$between_S1_S2_infec_cat)/dim(regression.vac)[1]


# # save regression data
# Regression.url <- "Code/TempData/regression_data.rds"
# saveRDS(regression_data, Regression.url)
# 
# regression_data <- readRDS(Regression.url)