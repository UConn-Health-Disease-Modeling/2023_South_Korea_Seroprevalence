# read datesets
base_url <- "Data/datashare_240326/"
sero12 <- openxlsx::read.xlsx(paste0(base_url, "df_S1S2_240321.xlsx"))
sero4 <- openxlsx::read.xlsx(paste0(base_url, "df_S4_240327.xlsx"))

### Processing S12
## convertToDate on vac date
date_columns <- c("vax.date1", "vax.date2", "vax.date3", "vax.date4", "vax.date.bi", "vax.date.xbb", 
                  "confirm1_date", "confirm2_date", "confirm3_date")
sero12[date_columns] <- lapply(sero12[date_columns], openxlsx::convertToDate)
# sapply(sero12[date_columns], function(x) inherits(x, "Date")) # check that all these columns are 'Date'.


## ymd on collect_date_S1 and collect_date_S2
sero12$collect_date_S1 <- lubridate::ymd(sero12$collect_date_S1)
sero12$collect_time_S2 <- substr(sero12$collect_time_S2, start = 1, stop = 10) 
sero12$collect_date_S2 <- lubridate::ymd(sero12$collect_time_S2)
# sapply(sero12[c("collect_date_S1", "collect_date_S2")], function(x) inherits(x, "Date")) # check again

## This step is intended to convert values like ">25,000" to "25,000" in order to enable the columns to be treated as numeric.
sero12 <- sero12 |>
  dplyr::mutate(across(c(S_num_S1, N_num_S1, S_num_S2, N_num_S2), 
                       ~ as.numeric(gsub("&gt;|&lt;", "", .))))
# sapply(sero12[c("S_num_S1", "N_num_S1", "S_num_S2", "N_num_S2")], function(x) inherits(x, "numeric")) 
sero12 <- sero12 |> dplyr::select(-collect_time_S2)



### Processing S4
date_columns <- c("vax.date1", "vax.date2", "vax.date3", "vax.date4", "vax.date.bi", "vax.date.xbb", "vax.date.flu22", "vax.date.flu23", 
                  "confirm1_date", "confirm2_date", "collect_date_S2", "collect_date_S3", "collect_date_S4")

sero4[date_columns] <- lapply(sero4[date_columns], openxlsx::convertToDate)
# sapply(sero4[date_columns], function(x) inherits(x, "Date")) # check that all these columns are 'Date'.

sero4 <- sero4 |>
  dplyr::mutate(across(c(S_num_S2, S_num_S3, S_num_S4), 
                       ~ as.numeric(gsub("> ", "", .))))



### Merge S12 and S4
sero12 <- sero12 |>
  dplyr::rename_with(~ paste0("S12.", .), 
                     .cols = -GNO)

sero4 <- sero4 |>
  dplyr::rename_with(~ paste0("S4.", .), 
                     .cols = -GNO)

sero_merge <- sero12 |> 
  dplyr::left_join(
    sero4, by = "GNO"
  )



## Save the processed data
save.path <- paste0(base_url, "df_merge_S12_4_240902.xlsx")
openxlsx::write.xlsx(sero_merge, file = save.path)