# Load required libraries
library(dplyr)
library(lubridate)
library(openxlsx)

################################
# ####### Data Process ###########
################################

# Load the dataset
# sero <- read.xlsx("Data/df_S1S2_0329_forshare.xlsx")
# Clean and transform columns related to dates and rounding ages

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

# Selecting and renaming variables
# sero2 <- sero %>% select() %>%
#   dplyr::rename(hosp_S2 = "q9_3_S2", 
#                 hosp_dur_S2 = "q9_3_day_S2", 
#                 sym_S1_raw = "p21_q4_S1", 
#                 sym_S2_raw = "q6_S2",
#                 confirm_S1 = "p21_q6_S1", 
#                 confirm_S2 = "q9_S2",
#                 hypertension1 = "p21_q2_1_S1", 
#                 diabetes1 = "p21_q2_2_S1")

# Define groups based on S_cha and N_cha categories

# Adding new variables related to nearest dates
# sero2 <- sero2 %>%
#   rowwise() %>%
#   mutate(
#     nearest_confirmed_before_S1 = nearest_date_before(collect_date_S1, confirm1_date, confirm2_date, confirm3_date),
#     nearest_confirmed_before_S2 = nearest_date_before(collect_date_S2, confirm1_date, confirm2_date, confirm3_date),
#     nearest_confirmed_after_S1 = nearest_date_after(collect_date_S1, confirm1_date, confirm2_date, confirm3_date)
#   ) %>%
#   ungroup()

# Adding time gap variables between vaccinations, infections, and surveys
# sero2 <- sero2 %>%
#   mutate(gap_between_surveys = as.integer(collect_date_S2 - collect_date_S1))

################################
# ####### Final Processing ######
################################

# Read the final cleaned dataset
df <- readRDS("Code/TempData/SevereSymptomAnalysis.rds")

# Filter data based on induced immunity categories
df_hybrid <- df %>% filter(induced_index == "hybrid-induced")
df_vac <- df %>% filter(induced_index == "vac-induced")

# Select relevant columns for analysis
df_filtered <- df %>% select(age_category, sex, hosp_S2, S_num_S1, severe, induced_index)

# Create subsets for hybrid-induced and vac-induced categories
hybrid_group <- df_filtered %>% filter(induced_index == "hybrid-induced")
vac_group <- df_filtered %>% filter(induced_index == "vac-induced")

# Output contingency tables for hospitalizations by age category
cat("Hybrid Group\n")
print(table(hybrid_group$hosp_S2, hybrid_group$age_category))

cat("Vaccine Group\n")
print(table(vac_group$hosp_S2, vac_group$age_category))

# Logistic regression model to predict severe symptoms
model <- glm(severe ~ age_category + sex + induced_index, family = "binomial", data = df_filtered)
summary(model)

# Save processed data if needed
# saveRDS(df_filtered, file = "Code/TempData/ProcessedData.rds")


