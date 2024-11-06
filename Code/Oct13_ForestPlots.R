library(dplyr)
library(tidyr)
library(data.table)
library(grid)
library(forestploter)
library(forestplot)
library(openxlsx)

# # ######################
# # ## Process the data ##
# # ######################
# 
# # load data
# Regression.url <- "Code/TempData/regression_data.rds"
# df <- readRDS(Regression.url)
# 
# # level the characteristics
# df <- df %>%
#   mutate(age_category = case_when(
#     age_base < 20 ~ "<20",
#     age_base >= 20 & age_base <= 40 ~ "20-40",
#     age_base > 40 & age_base <= 60 ~ "40-60",
#     age_base > 60 ~ ">60"
#   ))
# df$age_category <- factor(df$age_category, levels = c("<20", "20-40", "40-60", ">60"))
# 
# df$sex <- factor(df$sex, levels = c("Male", "Female"))
# df$otherdisease_S1 <- factor(df$otherdisease_S1, levels = c("yes", "no"))
# df$infec_cat <- ifelse(df$between_S1_S2_infec_cat == "yes", "confirmed_infec", "non_infec")
# df$infec_cat <- factor(df$infec_cat, levels = c("non_infec", "confirmed_infec"))
# df$latest_immunology_cat <- factor(df$latest_immunology_cat,
#                                    levels = c("<1 month", "1-6 months", "6-12 months", ">1 year", "no_event"))
# df$S_num_S1_cat <- cut(df$S_num_S1,
#                        breaks = c(-Inf, 6000, 15000, 24000, Inf),
#                        labels = c("<6000", "6000-15000", "15000-24000", ">=24000"),
#                        right = FALSE,
#                        include.lowest = TRUE,
#                        addNA = TRUE)
# df$S_num_S1_cat <- factor(df$S_num_S1_cat,
#                           levels = c("<6000", "6000-15000", "15000-24000", ">=24000"))
# 
# df$N_num_S1_cat <- cut(df$N_num_S1,
#                        breaks = c(-Inf, 1, 10, 20, Inf),
#                        labels = c("<1", "1-10", "10-20", ">=20"),
#                        right = FALSE,
#                        include.lowest = TRUE,
#                        addNA = TRUE)
# df$N_num_S1_cat <- factor(df$N_num_S1_cat,
#                           levels = c("<1", "1-10", "10-20", ">=20"))
# 
# df$immune_type <- factor(df$immune_type,
#                            levels = c("hybrid-induced", "vac-induced", "naive"))
# 
# # forest data creating
# # df1 <- df %>%
# #   select(GNO_S1,  age_cat, sex, edu,
# #          income_cat, otherdisease_S1, latest_immunology_cat, vac_before_S1_freq,
# #          infec_cat, induced_index, N_num_S1_cat, S_num_S1_cat)
# 
# 
# df1 <- df %>% filter(immune_type == "hybrid-induced") %>%
#   select(GNO,  age_category, sex, otherdisease_S1, latest_immunology_cat, vac_before_S1_freq,
#          infec_cat, immune_type, N_num_S1_cat, S_num_S1_cat)
# 
# # factors to iterate over
# categories <- c("age_category", "sex", "otherdisease_S1", "latest_immunology_cat",
#                 "vac_before_S1_freq", "immune_type", "N_num_S1_cat", "S_num_S1_cat")
# df1$infection_cat <- ifelse(df1$infec_cat == "non_infec", 1, 0)
# 
# 
# results_list <- list()
# for (char in categories) {
#   # char <- "age_category"
#   summary_df <- df1 %>%
#     group_by(!!sym(char)) %>%
#     summarise(not_infected = sum(infection_cat), infected = n() - sum(infection_cat)) %>%
#     mutate(odds = infected/not_infected,
#            characteristic = char) %>%
#     select(characteristic, group = !!sym(char), infec_num = infected,
#            non_infec_num = not_infected, odds)
#   # Append to results list
#   results_list[[char]] <- summary_df
# }
# results_list$vac_before_S1_freq$group <- factor(results_list$vac_before_S1_freq$group)
# 
# final_results <- rbindlist(results_list, fill = TRUE) %>%
#   filter(!is.na(group)) %>%
#   filter(odds != 0 & odds !=Inf) %>%
#   filter(group != 1)
# 
# ## calculate lower and upper bounds, as well as confidence interval
# final_results <- final_results %>%
#   group_by(characteristic) %>%
#   mutate(
#     ref_infected = first(infec_num),
#     ref_not_infected = first(non_infec_num),
#     ref_odds = first(odds),
#     odds_ratio = odds / ref_odds,
#     # Calculate SE and CI
#     se_log_or = sqrt(1/non_infec_num + 1/non_infec_num +
#                        1/ref_infected + 1/ref_not_infected),
#     lower_ci = exp(log(odds_ratio) - 1.96 * se_log_or),
#     upper_ci = exp(log(odds_ratio) + 1.96 * se_log_or)
#   ) %>%
#   select(-ref_infected, -ref_not_infected, -ref_odds, -odds) %>%
#   ungroup()
# 
# 
# labeltext <- cbind(final_results$characteristic, as.character(final_results$group))
# labeltext <- apply(labeltext, 2, function(x) as.character(factor(x)))
# 
# # The mean estimate (log scale for odds ratios)
# mean <- log(final_results$odds_ratio)
# 
# # Lower and upper bounds of the confidence interval (log scale)
# lower <- log(final_results$lower_ci)
# upper <- log(final_results$upper_ci)
# case <- final_results$infec_num
# control <- final_results$non_infec_num
# 
# # # save the result
# write.csv(final_results, "Code/TempData/Oct13_forest_hybrid_results.csv")
# # write.csv(final_results, "Code/TempData/Oct13_forest_vaccine_results.csv")
# 
# # ######################
# # ######## end #########
# # ######################





# Install and load necessary packages
# install.packages("forestplot")
library(forestplot)
library(dplyr)
library(readxl)
library(openxlsx)

# Load the data (you might already have it loaded)
url <- "Code/TempData/Oct13_forest_data(undetected).xlsx"
vac_df    <- read.xlsx(url, sheet = 1)
hybrid_df <- read.xlsx(url, sheet = 2)

# Prepare the data for forest plot (vaccine-induced)
vac_df <- vac_df %>%
  mutate(
    ci_text = ifelse(is.na(est), "", 
                     sprintf("%.2f (%.2f to %.2f)", est, low, hi)),
    Case = as.character(Case),
    Control = as.character(Control),
    Case = ifelse(is.na(Case), "", Case),
    Control = ifelse(is.na(Control), "", Control)
  )

# Prepare the data for forest plot (hybrid-induced)
hybrid_df <- hybrid_df %>%
  mutate(
    ci_text = ifelse(is.na(est), "", 
                     sprintf("%.2f (%.2f to %.2f)", est, low, hi)),
    Case = as.character(Case),
    Control = as.character(Control),
    Case = ifelse(is.na(Case), "", Case),
    Control = ifelse(is.na(Control), "", Control)
  )

# Define helper function to create the forest plot
create_forestplot <- function(data, title) {
  
  labels <- cbind(data$Subgroup,
                  paste0(data$Case, "/", data$Control),  # Simplified Case/Control display
                  data$ci_text) 
  labels <- gsub("(^/$)|([^0-9]/[^0-9])", "Case/Control", labels)
  
  # Determine which rows are summary rows (for bolding)
  summary_rows <- is.na(data$est)  # NA in 'est' column indicates a summary row
  
  # Create forest plot
  forestplot(
    labeltext = labels,
    mean = data$est,
    lower = data$low,
    upper = data$hi,
    new_page = TRUE,
    is.summary = summary_rows,  # Mark summary rows to bold
    xlog = FALSE,
    clip = c(0.1, 5),  # x-axis limits
    col = forestplot::fpColors(box = "#DC143C", line = "#800000", summary = "#DC143C"),
    zero = 1,  # Reference line
    title = title
  )
}

# Save the forest plot for vaccine-induced to a PDF file
pdf("vaccine_forest_plot_forestplot.pdf", width = 12, height = 6)  # Adjust size as needed
create_forestplot(vac_df, "Vaccine-Induced Immunity")
dev.off()

# Save the forest plot for hybrid-induced to a PDF file
pdf("hybrid_forest_plot_forestplot.pdf", width = 12, height = 6)  # Adjust size as needed
create_forestplot(hybrid_df, "Hybrid-Induced Immunity")
dev.off()


# # install.packages("pdftools")
# library(pdftools)
# pdf_subset("vaccine_forest_plot_forestplot.pdf", pages = setdiff(1:2, 1))

# library(pdftools)
# pdf_subset("Results/plots_updated/vaccine_forest_plot_forestplot(undetected).pdf", pages = setdiff(1:2, 1))

