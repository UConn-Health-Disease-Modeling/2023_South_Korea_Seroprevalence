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
                "p21_q4_S1", "q6_S2") |> 
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
    vax.date5        = "vax.date.bi"
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

# flow chart excluding criteria 1: excluding those with vaccination during S1 and S2 
S_12.v2 <- S_12 |> filter(between_S1_S2_vac_cat == "no")


















