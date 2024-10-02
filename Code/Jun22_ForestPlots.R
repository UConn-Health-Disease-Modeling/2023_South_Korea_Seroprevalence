# load packages
library(dplyr)
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
# library(deeplr)
library(plotROC)
library(broom)
library(epitools)
library(data.table)
library(grid)
library(forestploter)
library(forestplot)

# ######################
# ## Process the data ##
# ######################

table(df$induced_index)
hyb.2dose <- df |> filter(induced_index == "hybrid-induced") |> filter(vac_before_S1_freq == 2)
hyb.34dose <- df |> filter(induced_index == "hybrid-induced") |> filter(vac_before_S1_freq == 3 | vac_before_S1_freq == 4)

hyb.2dose$latest_immunology
hyb.34dose$latest_immunology

boxplot(hyb.2dose$latest_immunology, hyb.34dose$latest_immunology,
        names = c("hyb.2dose", "hyb.34dose"),
        main = "Comparison of Immunology Sequences",
        ylab = "Immunology Levels",
        col = c("lightblue", "lightgreen"))

# Create density plots for both sequences
density_2dose <- density(hyb.2dose$latest_immunology)
density_34dose <- density(hyb.34dose$latest_immunology)

# Plot the first density (hyb.2dose)
plot(density_2dose, 
     main = "Density Plot of Immunology Sequences",
     xlab = "Immunology Levels",
     ylab = "Density",
     col = "blue", 
     lwd = 2)

lines(density_34dose, 
      col = "green", 
      lwd = 2)

# Add a legend to distinguish the two sequences
legend("topright", 
       legend = c("hyb.2dose", "hyb.34dose"), 
       col = c("blue", "green"), 
       lwd = 2)




hist(hyb.2dose$latest_immunology, 
     breaks = 30, 
     col = rgb(0, 0, 1, 0.5),   # Set semi-transparent blue color
     xlim = range(c(hyb.2dose$latest_immunology, hyb.34dose$latest_immunology)), 
     ylim = c(0, max(hist(hyb.2dose$latest_immunology, plot = FALSE)$counts, 
                     hist(hyb.34dose$latest_immunology, plot = FALSE)$counts)),
     main = "Histogram Comparison of Immunology Sequences",
     xlab = "Immunology Levels",
     ylab = "Frequency")

# Add the second histogram (hyb.34dose) to the same plot
hist(hyb.34dose$latest_immunology, 
     breaks = 30, 
     col = rgb(0, 1, 0, 0.5),   # Set semi-transparent green color
     add = TRUE)

# Add a legend to distinguish the two sequences
legend("topright", 
       legend = c("hyb.2dose", "hyb.34dose"), 
       fill = c(rgb(0, 0, 1, 0.5), rgb(0, 1, 0, 0.5)))

# # load data 
# url <- "Code/TempData/Mar17_Case_Study_Data.csv"
# df <- read.csv(url)
# # class(df$N_num_S1)
# # level the characteristics
# # df$age_cat <- factor(df$age_cat, levels = c("<20", "20-39", "40-59", "60-79", "80+"))
# df <- df %>%
#   mutate(age_category = case_when(
#     age < 20 ~ "<20",
#     age >= 20 & age <= 40 ~ "20-40",
#     age > 40 & age <= 60 ~ "40-60",
#     age > 60 ~ ">60"
#   ))
# df$age_category <- factor(df$age_category, levels = c("<20", "20-40", "40-60", ">60"))
# 
# df$sex <- factor(df$sex, levels = c("Male", "Female"))
# df$edu <- factor(df$edu, levels = c("Primary School", "Middle/High School", "Postsecondary"))
# df$income_cat <- factor(df$income_cat, levels = c("<3000", "3000-6000", "6000-9000", ">9000"))
# df$otherdisease_S1 <- factor(df$otherdisease_S1, levels = c("yes", "no"))
# df$infec_cat <- ifelse(df$case_study == "case", "confirmed_infec", "non_infec")
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
# df$induced_index <- factor(df$induced_index, 
#                            levels = c("hybrid-induced", "vac-induced", "naive"))
# 
# # forest data creating 
# # df1 <- df %>% 
# #   select(GNO_S1,  age_cat, sex, edu, 
# #          income_cat, otherdisease_S1, latest_immunology_cat, vac_before_S1_freq, 
# #          infec_cat, induced_index, N_num_S1_cat, S_num_S1_cat)
# 
# 
# df1 <- df %>% filter(induced_index == "hybrid-induced") %>% 
#   select(GNO_S1,  age_category, sex, edu, 
#          income_cat, otherdisease_S1, latest_immunology_cat, vac_before_S1_freq, 
#          infec_cat, induced_index, N_num_S1_cat, S_num_S1_cat)
# 
# # factors to iterate over 
# categories <- c("age_category", "sex", "edu", "income_cat", "otherdisease_S1", "latest_immunology_cat", 
#                 "vac_before_S1_freq", "induced_index", "N_num_S1_cat", "S_num_S1_cat")
# df1$infection_cat <- ifelse(df1$infec_cat == "non_infec", 1, 0)
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

# # save the result 
# write.csv(final_results, "Code/TempData/forest_hybrid_results.csv")

# # repeat to save 'forest_vaccine_results.csv'

# ######################
# ######## end #########
# ######################

# Load the data
url <- "Code/TempData/forest_data.xlsx"
vac <- read.xlsx(url, sheet = 1)
hybrid <- read.xlsx(url, sheet = 2)

# Prepare the data for the forest plot (Vaccine-induced)
vac$` ` <- paste(rep(" ", 35), collapse = "   ")
vac$`log OR (95% CI)` <- ifelse(is.na(vac$est), "",
                                sprintf("%.2f (%.2f to %.2f)",
                                        vac$est, vac$low, vac$hi))
vac$Case <- as.character(vac$Case)
vac$Control <- as.character(vac$Control)
vac$Case[is.na(vac$Case)] <- ""
vac$Control[is.na(vac$Control)] <- ""

# Prepare the data for the forest plot (Hybrid-induced)
hybrid$` ` <- paste(rep(" ", 35), collapse = "   ")
hybrid$`log OR (95% CI)` <- ifelse(is.na(hybrid$est), "",
                                   sprintf("%.2f (%.2f to %.2f)",
                                           hybrid$est, hybrid$low, hybrid$hi))
hybrid$Case <- as.character(hybrid$Case)
hybrid$Control <- as.character(hybrid$Control)
hybrid$Case[is.na(hybrid$Case)] <- ""
hybrid$Control[is.na(hybrid$Control)] <- ""



x_axis_clip <- c(0.1, 10)  # Example range, adjust based on your data
x_axis_ticks <- c(0.1, 0.5, 1, 2, 5, 10)  # Example ticks, adjust as necessary

# Create the forest plot for vac (vaccine-induced)
p1 <- forest(vac[, c(1:3, 7:8)],
             est = vac$est,
             lower = vac$low, 
             upper = vac$hi,
             ci_column = 4,
             ref_line = 1,  # Add reference line at 1
             xlog = FALSE,
             clip = x_axis_clip,
             ticks_at = x_axis_ticks,
             arrow_lab = c("lower risk", "higher risk"),
             new_page = TRUE)

# Create the forest plot for hybrid-induced
p2 <- forest(hybrid[, c(1:3, 7:8)],
             est = hybrid$est,
             lower = hybrid$low, 
             upper = hybrid$hi,
             ci_column = 4,
             ref_line = 1,  # Add reference line at 1
             xlog = FALSE,
             clip = x_axis_clip,
             ticks_at = x_axis_ticks,
             arrow_lab = c("lower risk", "higher risk"),
             new_page = TRUE)


p1 + xlim(c(.1, 10))

