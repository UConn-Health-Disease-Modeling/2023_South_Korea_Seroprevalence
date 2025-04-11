library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(viridis)

url_S_12 <- "data/datashare_240902/df_S1S2_240827.xlsx"
S_12 <- readxl::read_excel(url_S_12) %>%
  # Select relevant columns
  dplyr::select(
    starts_with("GNO"), sex, starts_with("age"),
    starts_with("wgt"), 
    starts_with("N_"), starts_with("S_"),
    starts_with("vax"), starts_with("conf"),
    starts_with("collect"), starts_with("p21_q2"),
    "q2_S2", "p21_q4_S1", "q6_S2", "q9_3_S2", 
    "p11_q1_S1", "p11_q3_2_S1", "p21_q1_S1", "p21_q3_1_S1", "p21_q3_2_S1"
  ) 
colnames(S_12)

cov_12 <- 
  S_12 %>% filter(!is.na(N_num_S2) ) %>%
  mutate(
    collect_date_S1 = as.Date(collect_date_S1),
    collect_date_S2 = as.Date(collect_time_S2),
    cov1_1 = case_when(is.na(conf_date1) | conf_date1>collect_date_S2 ~ 0, # Not confirmed before the 2nd wave
                       conf_date1==collect_date_S1 | conf_date1==collect_date_S2 ~ 9999, # Confirmed on the day of the survey 
                       conf_date1<collect_date_S1 ~ 1, # Confirmed before the 1st wave
                       conf_date1<collect_date_S2 ~ 2), # Confirmed between the 1st & 2nd wave
    cov1_2 = case_when(is.na(conf_date2) |  conf_date2>collect_date_S2 ~ 0,
                       conf_date2==collect_date_S1 | conf_date2==collect_date_S2 ~ 9999,
                       conf_date2<collect_date_S1 ~ 1,
                       conf_date2<collect_date_S2 ~ 2),
    cov_cat = fct_relevel(case_when(cov1_1==2 ~ "New inf",
                                    cov1_1==0 ~ "Never inf",
                                    cov1_1==1 & cov1_2==2 ~ "Re inf",
                                    cov1_1==1 ~ "Not inf",
                                    TRUE ~ "others"),
                          "New inf","Re inf","Not inf","Never inf","others"),
    N_diff =  N_num_S2 - N_num_S1,
    N_log = log2(N_num_S2) - log2(N_num_S1),
    N_cat = paste0(substr(N_cha_S1,1,1),substr(N_cha_S2, 1, 1)),
    wash = case_when((conf_date1>(collect_date_S1-60) & conf_date1<=collect_date_S1) |
                       (conf_date2>(collect_date_S1-60) & conf_date2<=collect_date_S1) ~ 1,
                     (vax.date1>=(collect_date_S1-14) & vax.date1<=collect_date_S1) |
                       (vax.date2>=(collect_date_S1-14) & vax.date2<=collect_date_S1) |
                       (vax.date3>=(collect_date_S1-14) & vax.date3<=collect_date_S1) |
                       (vax.date4>=(collect_date_S1-14) & vax.date4<=collect_date_S1) |
                       (vax.date.bi>=(collect_date_S1-14) & vax.date.bi<=collect_date_S1) ~ 2, # for anti-S
                     TRUE ~ 0) ) %>%
  filter(wash==0 ) # id_check==1 means that the individuals were matched with the KDCA confirmation data (others may have incorrect personal IDs)

# cov_12 %>% filter(!cov_cat%in%c("others","Never inf")) %>%
#   ggplot(aes(x=N_log, fill = cov_cat))+
#   geom_density(adjust=0.3)+
#   scale_fill_viridis(discrete = TRUE, alpha=0.8) +
#   theme_classic(base_family = "AppleGothic") +
#   theme(legend.title = element_blank())+
#   labs(title="", x = "log2(Anti-N Ratio)") +
#   scale_x_continuous(breaks = seq(-10, 10, by = 1))



cov_12_check <- cov_12 %>% 
  filter(!cov_cat %in% c("others", "Not inf", "Re inf")) %>%
  select(GNO, GNO_S2, N_log, cov_cat)

thresholds <- seq(from = min(cov_12_check$N_log), 
                  to = max(cov_12_check$N_log),
                  by = 0.01)  

total_inf <- sum(cov_12_check$cov_cat == "New inf")
total_not <- sum(cov_12_check$cov_cat == "Never inf")

results <- data.frame(threshold = thresholds) %>%
  rowwise() %>%
  mutate(
    type_I_error_rate  = sum(cov_12_check$cov_cat == "New inf"   & cov_12_check$N_log <= threshold) / total_inf,
    type_II_error_rate = sum(cov_12_check$cov_cat == "Never inf" & cov_12_check$N_log >= threshold) / total_not,
  ) %>%
  ungroup()




# ------------------------------------------------------------------------------
plot_bias_proportion <- function(result) {
  
  # result = results
  
  # Reshape data to long format
  results_long <- tidyr::pivot_longer(result, 
                                      cols = -threshold, 
                                      names_to = "category", 
                                      values_to = "proportion")
  
  # Set category factor levels
  results_long$category <- factor(results_long$category, 
                                  levels = c("type_I_error_rate", "type_II_error_rate"))
  
  # Define colors using viridis
  categories <- unique(results_long$category)
  colors <- c(
    adjustcolor("red", alpha.f = 0.8),
    adjustcolor("blue", alpha.f = 0.8)
  )
  
  # Filter for highlighted thresholds
  highlight_data <- result[
    result$type_I_error_rate < 0.05 & result$type_II_error_rate < 0.2,
  ]
  
  # Base plot
  plot(NULL,
       xlim = c(min(results_long$threshold), max(results_long$threshold) + 0.5),
       ylim = range(results_long$proportion, na.rm = TRUE),
       xlab = expression(log[2](N[2]/N[1])),
       ylab = "Error Rate",
       family = "AppleGothic")
  
  # Plot lines by category
  for (i in seq_along(categories)) {
    cat_data <- results_long[results_long$category == categories[i], ]
    lines(cat_data$threshold, cat_data$proportion,
          col = colors[i],
          lwd = 2)
  }
  
  # Add black rectangles on the x-axis
  if (nrow(highlight_data) > 0) {
    rect(xleft = highlight_data$threshold - 0.02,
         xright = highlight_data$threshold + 0.02,
         ybottom = par("usr")[3],
         ytop = par("usr")[3] + 0.01 * diff(par("usr")[3:4]),
         col = "black",
         border = NA)
  }

  abline(v = highlight_data$threshold[1], col = "black", lty = 5, lwd = 1)

  points(x = highlight_data$threshold[1], y = highlight_data$type_I_error_rate[1],
        pch = 16, col = "black", cex = .7)
  text(x = highlight_data$threshold[1], y = highlight_data$type_I_error_rate[1],
       labels = paste0(round(highlight_data$type_I_error_rate[1] * 100, 1), "%"),
       pos = 2, cex = .6, col = "black")

  points(x = highlight_data$threshold[1], y = highlight_data$type_II_error_rate[1],
         pch = 16, col = "black", cex = .7)
  text(x = highlight_data$threshold[1], y = highlight_data$type_II_error_rate[1],
       labels = paste0(round(highlight_data$type_II_error_rate[1] * 100, 1), "%"),
       pos = 2, cex = .6, col = "black")

  abline(v = tail(highlight_data$threshold, 1), col = "black", lty = 5, lwd = 1)

  points(x = highlight_data$threshold[dim(highlight_data)[1]],
         y = highlight_data$type_I_error_rate[dim(highlight_data)[1]],
         pch = 16, col = "black", cex = .7)
  text(x = highlight_data$threshold[dim(highlight_data)[1]],
       y = highlight_data$type_I_error_rate[dim(highlight_data)[1]] - 0.02,
       labels = paste0(round(highlight_data$type_I_error_rate[dim(highlight_data)[1]] * 100, 1), "%"),
       pos = 4, cex = .6, col = "black")

  points(x = highlight_data$threshold[dim(highlight_data)[1]],
         y = highlight_data$type_II_error_rate[dim(highlight_data)[1]],
         pch = 16, col = "black", cex = .7)
  text(x = highlight_data$threshold[dim(highlight_data)[1]],
       y = highlight_data$type_II_error_rate[dim(highlight_data)[1]] + 0.02,
       labels = paste0(round(highlight_data$type_II_error_rate[dim(highlight_data)[1]] * 100, 1), "%"),
       pos = 4, cex = .6, col = "black")
  

  
  # Add legend
  legend("topright",
         legend = c(expression(alpha), expression(1 - beta)),
         col = colors,
         lwd = 2,
         bty = "o",
         bg = "white",
         box.col = "black",
         inset = c(0.01, 0.01),
         x.intersp = 0.7,
         y.intersp = 0.9)
}


png("result/plots/figS1.png", width = 1800, height = 1200, res = 300)
par(mar = c(4, 4, 1, 1))
plot_bias_proportion(results)
dev.off()







# ------------------------------------------------------------------------------
# Additional check for different age group 
url_S_12 <- "data/datashare_240902/df_S1S2_240827.xlsx"
cov_12 <- readxl::read_excel(url_S_12)
cov_12 <- cov_12 %>%
  # Select relevant columns
  dplyr::select(
    starts_with("GNO"), sex, starts_with("age"),
    starts_with("wgt"), 
    starts_with("N_"), starts_with("S_"),
    starts_with("vax"), starts_with("conf"),
    starts_with("collect"), starts_with("p21_q2"),
    "q2_S2", "p21_q4_S1", "q6_S2", "q9_3_S2", 
    "p11_q1_S1", "p11_q3_2_S1", "p21_q1_S1", "p21_q3_1_S1", "p21_q3_2_S1"
  ) %>%
  # Rename columns for clarity
  dplyr::rename(
    hypertension1        = "p21_q2_1_S1",
    diabetes1            = "p21_q2_2_S1",
    highcol1             = "p21_q2_3_S1",
    cancer1              = "p21_q2_4_S1",
    brainblood1          = "p21_q2_5_S1",
    chronickidney1       = "p21_q2_6_S1",
    chroniclung1         = "p21_q2_7_S1",
    liver1               = "p21_q2_8_S1",
    immunocomp1          = "p21_q2_9_S1",
    rumatis1             = "p21_q2_10_S1",
    other1               = "p21_q2_91_S1",
    otherdisease_S2_raw  = "q2_S2",
    sym_S1_raw           = "p21_q4_S1",
    sym_S2_raw           = "q6_S2",
    vax.date5            = "vax.date.bi",
    hosp.S2              = "q9_3_S2", 
    household_type       = "p11_q1_S1", 
    income               = "p11_q3_2_S1", 
    health_status        = "p21_q1_S1", 
    height               = "p21_q3_1_S1", 
    weight               = "p21_q3_2_S1"
  ) %>%
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
    ), 
    BMI = weight/((height/100)^2)
  ) %>%
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
  ) %>%
  # Categorize groups as "inf", "vac", or "naive"
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

# create new column: number of diseases by 1st Seroprevalence
cov_12 <- cov_12 %>%
  mutate(
    num_diseases_S1 = rowSums(
      cbind(hypertension1, diabetes1, highcol1, cancer1, brainblood1, chronickidney1, chroniclung1, 
            liver1, immunocomp1, rumatis1, other1 == 1) == 2,  
      na.rm = TRUE
    )
  )

# Convert date columns
convert_to_date <- function(date_col) {
  substr(date_col, 1, 10) %>%
    as.Date(format = "%Y-%m-%d")
}

# Apply date conversion for vaccination and confirmation dates
cov_12 <- cov_12 %>%
  dplyr::mutate(
    vax.date1 = convert_to_date(vax.date1),
    vax.date2 = convert_to_date(vax.date2),
    vax.date3 = convert_to_date(vax.date3),
    vax.date4 = convert_to_date(vax.date4),
    vax.date5 = convert_to_date(vax.date5),
    conf_date1 = convert_to_date(conf_date1),
    conf_date2 = convert_to_date(conf_date2),
    conf_date3 = convert_to_date(conf_date3),
    collect_date_S2 = convert_to_date(collect_time_S2),
    collect_date_S1 = as.Date(collect_date_S1, format = "%Y-%m-%d")
  )

# Fill missing collection dates with the mode
most_common_date <- names(which.max(table(cov_12$collect_date_S2))) %>% as.Date()
cov_12$collect_date_S2[is.na(cov_12$collect_date_S2)] <- most_common_date

# Function to find nearest date before a reference date
nearest_date_before <- function(ref_date, ...){
  dates <- na.omit(c(...))
  dates <- dates[dates < ref_date]
  if(length(dates) == 0) return(NA)
  return(max(dates))
}

# Function to find the nearest date after a reference date
nearest_date_after <- function(ref_date, ...) {
  dates <- na.omit(c(...))
  dates <- dates[dates > ref_date]
  if (length(dates) == 0) return(NA)
  return(min(dates))
}

# Calculate nearest dates
cov_12 <- cov_12 %>%
  rowwise() %>%
  mutate(
    nearest_confirmed_before_S1 = nearest_date_before(collect_date_S1, conf_date1, conf_date2, conf_date3),
    nearest_confirmed_before_S2 = nearest_date_before(collect_date_S2, conf_date1, conf_date2, conf_date3),
    nearest_confirmed_after_S1 = nearest_date_after(collect_date_S1, conf_date1, conf_date2, conf_date3)
  ) %>%
  ungroup()

# Calculate infection and survey gaps
cov_12 <- cov_12 %>%
  mutate(
    gap_between_surveys = as.integer(collect_date_S2 - collect_date_S1),
    infec_gap_after_S1 = as.integer(nearest_confirmed_after_S1 - collect_date_S1),
    infec_gap_before_S1 = as.integer(collect_date_S1 - nearest_confirmed_before_S1),
    infec_gap_before_S2 = as.integer(collect_date_S2 - nearest_confirmed_before_S2),
    between_S1_S2_infec_cat = case_when(
      is.na(collect_date_S2) ~ NA_character_,
      is.na(infec_gap_after_S1) ~ "no",
      !is.na(infec_gap_after_S1) & infec_gap_after_S1 <= gap_between_surveys ~ "yes",
      TRUE ~ "no"
    )
  )

# Calculate nearest vaccination dates
cov_12 <- cov_12 %>%
  rowwise() %>%
  mutate(
    nearest_vac_before_S1 = nearest_date_before(collect_date_S1, vax.date1, vax.date2, vax.date3, vax.date4, vax.date5),
    nearest_vac_before_S2 = nearest_date_before(collect_date_S2, vax.date1, vax.date2, vax.date3, vax.date4, vax.date5),
    nearest_vac_after_S1 = nearest_date_after(collect_date_S1, vax.date1, vax.date2, vax.date3, vax.date4, vax.date5)
  ) %>%
  ungroup()

# Calculate vaccination gaps
cov_12 <- cov_12 %>%
  mutate(
    vac_gap_after_S1 = as.integer(nearest_vac_after_S1 - collect_date_S1),
    vac_gap_before_S1 = as.integer(collect_date_S1 - nearest_vac_before_S1),
    vac_gap_before_S2 = as.integer(collect_date_S2 - nearest_vac_before_S2),
    between_S1_S2_vac_cat = case_when(
      is.na(collect_date_S2) ~ NA_character_,
      is.na(vac_gap_before_S2) ~ "no",
      vac_gap_before_S1 > 0 & is.na(vac_gap_after_S1) ~ "no",
      vac_gap_after_S1 <= gap_between_surveys ~ "yes",
      TRUE ~ "no"
    )
  )

# Calculate vaccination frequency
cov_12 <- cov_12 %>%
  rowwise() %>%
  mutate(
    vac_before_S1_freq = sum(c(vax.date1, vax.date2, vax.date3, vax.date4, vax.date5) < collect_date_S1, na.rm = TRUE)
  ) %>%
  ungroup()

# Clean numeric columns
cov_12 <- cov_12 %>%
  mutate(
    S_num_S1 = gsub("[<>]", "", S_num_S1) %>% as.numeric(),
    N_num_S1 = gsub("[<>]", "", N_num_S1) %>% as.numeric(),
    S_num_S2 = gsub("[<>]", "", S_num_S2) %>% as.numeric(),
    N_num_S2 = gsub("[<>]", "", N_num_S2) %>% as.numeric()
  )


cov_12 <- 
  cov_12 %>% filter(!is.na(N_num_S2) ) %>%
  mutate(
    collect_date_S1 = as.Date(collect_date_S1),
    collect_date_S2 = as.Date(collect_time_S2),
    cov1_1 = case_when(is.na(conf_date1) | conf_date1>collect_date_S2 ~ 0, # Not confirmed before the 2nd wave
                       conf_date1==collect_date_S1 | conf_date1==collect_date_S2 ~ 9999, # Confirmed on the day of the survey 
                       conf_date1<collect_date_S1 ~ 1, # Confirmed before the 1st wave
                       conf_date1<collect_date_S2 ~ 2), # Confirmed between the 1st & 2nd wave
    cov1_2 = case_when(is.na(conf_date2) |  conf_date2>collect_date_S2 ~ 0,
                       conf_date2==collect_date_S1 | conf_date2==collect_date_S2 ~ 9999,
                       conf_date2<collect_date_S1 ~ 1,
                       conf_date2<collect_date_S2 ~ 2),
    cov_cat = fct_relevel(case_when(cov1_1==2 ~ "New inf",
                                    cov1_1==0 ~ "Never inf",
                                    cov1_1==1 & cov1_2==2 ~ "Re inf",
                                    cov1_1==1 ~ "Not inf",
                                    TRUE ~ "others"),
                          "New inf","Re inf","Not inf","Never inf","others"),
    N_diff =  N_num_S2 - N_num_S1,
    N_log = log2(N_num_S2) - log2(N_num_S1),
    N_cat = paste0(substr(N_cha_S1,1,1),substr(N_cha_S2, 1, 1)),
    wash = case_when((conf_date1>(collect_date_S1-60) & conf_date1<=collect_date_S1) |
                       (conf_date2>(collect_date_S1-60) & conf_date2<=collect_date_S1) ~ 1,
                     (vax.date1>=(collect_date_S1-14) & vax.date1<=collect_date_S1) |
                       (vax.date2>=(collect_date_S1-14) & vax.date2<=collect_date_S1) |
                       (vax.date3>=(collect_date_S1-14) & vax.date3<=collect_date_S1) |
                       (vax.date4>=(collect_date_S1-14) & vax.date4<=collect_date_S1) |
                       (vax.date5>=(collect_date_S1-14) & vax.date5<=collect_date_S1) ~ 2, # for anti-S
                     TRUE ~ 0) ) %>%
  filter(wash==0 ) # id_check==1 means that the individuals were matched with the KDCA confirmation data (others may have incorrect personal IDs)


cov_12 <- cov_12 %>%
  mutate(latest_immunology = ifelse(
    is.na(vac_gap_before_S1), infec_gap_before_S1,
    ifelse(is.na(infec_gap_before_S1), vac_gap_before_S1,
           pmin(vac_gap_before_S1, infec_gap_before_S1, na.rm = TRUE))), 
    infec_gap = infec_gap_after_S1) %>%
  mutate(
    latest_immunology_cat = case_when(
      latest_immunology >= 0 & latest_immunology <= 30 ~ "<1 month",
      latest_immunology > 30 & latest_immunology <= 180 ~ "1-6 months",
      latest_immunology > 180 & latest_immunology <= 365 ~ "6-12 months",
      latest_immunology > 365 ~ ">1 year",
      TRUE ~ "no_event"
    ),
    latest_immunology_cat = factor(
      latest_immunology_cat,
      levels = c("<1 month", "1-6 months", "6-12 months", ">1 year", "no_event")
    )
  )




cov_12_check <- cov_12 %>% 
  filter(!cov_cat %in% c("others", "Not inf", "Re inf")) %>%
  filter(latest_immunology_cat == ">1 year") %>% 
  select(GNO, GNO_S2, N_log, cov_cat)

thresholds <- seq(from = min(cov_12_check$N_log), 
                  to = max(cov_12_check$N_log),
                  by = 0.01)  

total_inf <- sum(cov_12_check$cov_cat == "New inf")
total_not <- sum(cov_12_check$cov_cat == "Never inf")

results <- data.frame(threshold = thresholds) %>%
  rowwise() %>%
  mutate(
    type_I_error_rate  = sum(cov_12_check$cov_cat == "New inf"   & cov_12_check$N_log <= threshold) / total_inf,
    type_II_error_rate = sum(cov_12_check$cov_cat == "Never inf" & cov_12_check$N_log >= threshold) / total_not,
  ) %>%
  ungroup()



png("result/plots/figS2(4).png", width = 1800, height = 1200, res = 300)
par(mar = c(4, 4, 1, 1))
plot_bias_proportion(results)
dev.off()




