library(dplyr)
library(lubridate)
library(tidyr)

# ######################
# ## Process the data ##
# ######################

url_S_12 <- "Data/datashare_240902/df_S1S2_240827.xlsx"
url_S_4 <- "Data/datashare_240902/df_S4_240902.xlsx"
url_S_5 <- "Data/datashare_240902/df_S5_240829.xlsx"



## Processing S_12
S_12 <- readxl::read_excel(url_S_12) |> 
  dplyr::select(starts_with("GNO"), sex, 
                starts_with("age"),
                starts_with("N_"), starts_with("S_"), 
                starts_with("vax"), starts_with("conf"), 
                starts_with("collect"))

colnames(S_12)
# convertToDate on vac date
S_12$vax.date1 <- substr(S_12$vax.date1, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$vax.date2 <- substr(S_12$vax.date2, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$vax.date3 <- substr(S_12$vax.date3, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$vax.date4 <- substr(S_12$vax.date4, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$vax.date.bi <- substr(S_12$vax.date.bi, 1, 10) |> as.Date(format = "%Y-%m-%d")

# # convertToDate on confirm date
S_12$conf_date1 <- substr(S_12$conf_date1, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$conf_date2 <- substr(S_12$conf_date2, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$conf_date3 <- substr(S_12$conf_date3, 1, 10) |> as.Date(format = "%Y-%m-%d")

colnames(S_12) <- ifelse(colnames(S_12) == "GNO", "GNO", paste0("S12.", colnames(S_12)))
S_12$S12.collect_date_S2 <- substr(S_12$S12.collect_time_S2, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_12$S12.collect_date_S1 <- as.Date(S_12$S12.collect_date_S1, format = "%Y-%m-%d")



## Processing S_4
S_4 <- readxl::read_excel(url_S_4) |> 
  dplyr::select(starts_with("GNO"), sex, 
                starts_with("age"),
                starts_with("N_"), starts_with("S_"), 
                starts_with("vax"), starts_with("conf"), 
                starts_with("collect"))

# convertToDate on vac date
S_4$vax.date1 <- substr(S_4$vax.date1, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_4$vax.date2 <- substr(S_4$vax.date2, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_4$vax.date3 <- substr(S_4$vax.date3, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_4$vax.date4 <- substr(S_4$vax.date4, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_4$vax.date.bi <- substr(S_4$vax.date.bi, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_4$vax.date.xbb <- substr(S_4$vax.date.xbb, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_4$vax.date.flu22 <- substr(S_4$vax.date.flu22, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_4$vax.date.flu23 <- substr(S_4$vax.date.flu23, 1, 10) |> as.Date(format = "%Y-%m-%d")

# # convertToDate on confirm date
S_4$conf_date1 <- substr(S_4$conf_date1, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_4$conf_date2 <- substr(S_4$conf_date2, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_4$conf_date3 <- substr(S_4$conf_date3, 1, 10) |> as.Date(format = "%Y-%m-%d")

colnames(S_4) <- ifelse(colnames(S_4) == "GNO", "GNO", paste0("S4.", colnames(S_4)))
S_4$S4.collect_date_S2 <- as.Date(S_4$S4.collect_date_S2, format = "%Y-%m-%d")
S_4$S4.collect_date_S3 <- as.Date(S_4$S4.collect_date_S3, format = "%Y-%m-%d")
S_4$S4.collect_date_S4 <- as.Date(S_4$S4.collect_date_S4, format = "%Y-%m-%d")




## Processing S_5
S_5 <- readxl::read_excel(url_S_5) |> 
  dplyr::select(starts_with("GNO"), sex, 
                starts_with("age"),
                starts_with("N_"), starts_with("S_"), 
                starts_with("vax"), starts_with("conf"), 
                starts_with("collect"))

# convertToDate on vac date
S_5$vax.date1 <- substr(S_5$vax.date1, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_5$vax.date2 <- substr(S_5$vax.date2, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_5$vax.date3 <- substr(S_5$vax.date3, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_5$vax.date4 <- substr(S_5$vax.date4, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_5$vax.date.bi <- substr(S_5$vax.date.bi, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_5$vax.date.xbb <- substr(S_5$vax.date.xbb, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_5$vax.date.flu22 <- substr(S_5$vax.date.flu22, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_5$vax.date.flu23 <- substr(S_5$vax.date.flu23, 1, 10) |> as.Date(format = "%Y-%m-%d")

# # convertToDate on confirm date
S_5$conf_date1 <- substr(S_5$conf_date1, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_5$conf_date2 <- substr(S_5$conf_date2, 1, 10) |> as.Date(format = "%Y-%m-%d")
S_5$conf_date3 <- substr(S_5$conf_date3, 1, 10) |> as.Date(format = "%Y-%m-%d")

colnames(S_5) <- ifelse(colnames(S_5) == "GNO", "GNO", paste0("S5.", colnames(S_5)))

S_5$S5.collect_date_S5 <- substr(S_5$S5.collect_date_S5, 1, 10) |> as.Date(format = "%Y-%m-%d")

# ######################
# ######## end #########
# ######################





sero.full <- S_12 |> 
  left_join(S_4, by = "GNO") |> 
  left_join(S_5, by = "GNO")

# check the number of attendance for each surveillance 
sero.full$S12.collect_time_S2 |> is.na() |> sum()
sero.full$S4.collect_date_S4 |> is.na() |> sum()
sero.full$S5.collect_date_S5 |> is.na() |> sum()


library(purrr)
# Assuming conf_cols is the list of columns you want to check
conf_cols <- grep("conf", colnames(sero.full), value = TRUE)

# Calculate the number of rows where any "conf" date is between S4 and S5 dates
num_rows_in_range <- pmap_lgl(sero.full, function(...) {
  row <- list(...)
  
  # Ensure S4 and S5 dates are not NA before comparison
  if (!is.na(row[["S4.collect_date_S4"]]) & !is.na(row[["S5.collect_date_S5"]])) {
    # Check if any date in conf_cols is non-NA and between S4.collect_date_S4 and S5.collect_date_S5
    any(!is.na(as.Date(unlist(row[conf_cols]))) & 
          as.Date(unlist(row[conf_cols])) >= as.Date(row[["S4.collect_date_S4"]]) & 
          as.Date(unlist(row[conf_cols])) <= as.Date(row[["S5.collect_date_S5"]]))
  } else {
    # If S4 or S5 is NA, skip the row
    FALSE
  }
}) |> sum()



# Assuming conf_cols is the list of columns you want to check
conf_cols <- grep("conf", colnames(sero.full), value = TRUE)

# Calculate the number of rows where any "conf" date is between S4 and S5 dates
num_rows_in_range <- pmap_lgl(sero.full, function(...) {
  row <- list(...)
  
  # Ensure S4 and S5 dates are not NA before comparison
  if (!is.na(row[["S12.collect_date_S2"]]) & !is.na(row[["S4.collect_date_S4"]])) {
    # Check if any date in conf_cols is non-NA and between S4.collect_date_S4 and S5.collect_date_S5
    any(!is.na(as.Date(unlist(row[conf_cols]))) & 
          as.Date(unlist(row[conf_cols])) >= as.Date(row[["S12.collect_date_S2"]]) & 
          as.Date(unlist(row[conf_cols])) <= as.Date(row[["S4.collect_date_S4"]]))
  } else {
    # If S4 or S5 is NA, skip the row
    FALSE
  }
}) |> sum()


# Extract the rows where any "conf" date is between S12.collect_date_S2 and S4.collect_date_S4
rows_in_range <- pmap_lgl(sero.full, function(...) {
  row <- list(...)
  
  # Ensure S12.collect_date_S2 and S4.collect_date_S4 are not NA before comparison
  if (!is.na(row[["S12.collect_date_S2"]]) & !is.na(row[["S4.collect_date_S4"]])) {
    # Check if any date in conf_cols is non-NA and between S12.collect_date_S2 and S4.collect_date_S4
    any(!is.na(as.Date(unlist(row[conf_cols]))) & 
          as.Date(unlist(row[conf_cols])) >= as.Date(row[["S12.collect_date_S2"]]) & 
          as.Date(unlist(row[conf_cols])) <= as.Date(row[["S4.collect_date_S4"]]))
  } else {
    # If S12 or S4 is NA, return FALSE
    FALSE
  }
})

sero.full.S12_S4.infec <- sero.full[rows_in_range, ] |> dplyr::select(c(conf_cols, S12.collect_date_S2, S4.collect_date_S4))



