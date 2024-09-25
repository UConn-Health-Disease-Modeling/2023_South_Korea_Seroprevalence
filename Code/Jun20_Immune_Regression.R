# load packages
library(dplyr)
library(lubridate)
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
library(plotROC)
library(scales)
library(gridExtra)
library(pROC)

# get the url
url <- "Code/TempData/Mar17_Case_Study_Data.csv"
df <- read.csv(url)

# process variables
df <- df %>%
  mutate(age_category = case_when(
    age < 20 ~ "<20",
    age >= 20 & age <= 40 ~ "20-40",
    age > 40 & age <= 60 ~ "40-60",
    age > 60 ~ ">60"
  ))
df$age_category <- factor(df$age_category, levels = c("<20", "20-40", "40-60", ">60"))

# level the characteristics
# df$age_cat <- factor(df$age_cat, levels = c("<20", "20-39", "40-59", "60-79", "80+"))
df$sex <- factor(df$sex, levels = c("Male", "Female"))
df$edu <- factor(df$edu, levels = c("Primary School", "Middle/High School", "Postsecondary"))
df$income_cat <- factor(df$income_cat, levels = c("<3000", "3000-6000", "6000-9000", ">9000"))
df$otherdisease_S1 <- factor(df$otherdisease_S1, levels = c("yes", "no"))
df$case_study <- factor(df$case_study, levels = c("case", "control"))
df$latest_immunology_cat <- factor(df$latest_immunology_cat, 
                                   levels = c("<1 month", "1-6 months", "6-12 months", ">1 year", "no_event"))
df$S_num_S1_cat <- cut(df$S_num_S1,
                       breaks = c(-Inf, 6000, 15000, 24000, Inf),
                       labels = c("<6000", "6000-15000", "15000-24000", ">=24000"),
                       right = FALSE,
                       include.lowest = TRUE,
                       addNA = TRUE)
df$S_num_S1_cat <- factor(df$S_num_S1_cat, levels = c("<6000", "6000-15000", "15000-24000", ">=24000"))
df$induced_index <- factor(df$induced_index, levels = c("hybrid-induced", "vac-induced", "naive"))
# df1 <- df %>% filter(!(case_study == "control" & N_cha_S1 == "Nonreactive"))
df1 <- df

# # define the case and control 
# case <- df %>% filter(case_study == "case")
# control <- df %>% filter(case_study == "control")



# ######################
# #06/20 Logistic regression
# ######################

# Filter the data
df1$response <- ifelse(df1$case_study == "control", 1, 0)
vac_df <- df1 %>% filter(induced_index == "vac-induced")
hybrid_df <- df1 %>% filter(induced_index == "hybrid-induced")

# Fit the models
model <- glm(data = df1, response ~ S_num_S1 + age_category, family = "binomial")
df1$predicted_prob <- predict(model, type = "response")

# Compute the ROC curve
roc_curve <- roc(df1$response, df1$predicted_prob)

# Fit the logistic regression model for the vaccine-induced group
vac_model <- glm(data = vac_df, response ~ S_num_S1 + age_category, family = "binomial")
vac_df$predicted_prob <- predict(vac_model, type = "response")

# Compute the ROC curve for the vaccine-induced group
roc_curve1 <- roc(vac_df$response, vac_df$predicted_prob)

# Fit the logistic regression model for the hybrid-induced group
hybrid_model <- glm(data = hybrid_df, response ~ S_num_S1 + age_category, family = "binomial")
hybrid_df$predicted_prob <- predict(hybrid_model, type = "response")

# Compute the ROC curve for the hybrid-induced group
roc_curve2 <- roc(hybrid_df$response, hybrid_df$predicted_prob)

# Plot the combined ROC curves using ggroc
ggroc(list(`Whole-Population` = roc_curve, `Vaccine-Induced` = roc_curve1, `Hybrid-Induced` = roc_curve2), aes = c("color")) +
  scale_color_manual(values = c("Whole-Population" = "purple", "Vaccine-Induced" = "blue", "Hybrid-Induced" = "red")) +
  ggtitle("ROC Curves for Vaccine-Induced and Hybrid-Induced Groups") +
  xlab("False Positive Rate") + 
  ylab("True Positive Rate") + 
  theme_bw() +
  theme(legend.title = element_blank())


# Generate prediction data for vac model
S_num_S1_range <- seq(min(vac_df$S_num_S1), max(vac_df$S_num_S1), length.out = 100)
age_cat_range <- unique(vac_df$age_category)

combination_df_vac <- expand.grid(S_num_S1 = S_num_S1_range, age_category = age_cat_range)
combination_df_vac$pred <- predict(vac_model, newdata = combination_df_vac, type = "response")

# Plot 1: Vac-induced Group
plot1 <- ggplot(combination_df_vac, aes(x = S_num_S1, y = pred, group = age_category, color = age_category)) +
  geom_line(linewidth = 1) +  # Regression line
  labs(x = "S ab (S1)", y = "Infection Index") +
  ggtitle("Vac-induced Group") + 
  theme_bw() +
  ylim(0.5, 1) +  # Ensure identical y-axis scale
  theme(legend.position = "bottom")  # Move legend to the bottom for both plots

# Plot 2: Hybrid-induced Group (without legend)
plot2 <- ggplot(combination_df_hybrid, aes(x = S_num_S1, y = pred, group = age_category, color = age_category)) +
  geom_line(linewidth = 1) +  # Regression line
  labs(x = "S ab (S1)", y = "Infection Index") +
  ggtitle("Hybrid-induced Group") + 
  theme_bw() +
  ylim(0.5, 1) +  # Ensure identical y-axis scale
  theme(legend.position = "none")  # Remove legend

# Extract the legend from plot1
shared_legend <- cowplot::get_legend(plot1)

# Remove legend from plot1 for combined plotting
plot1 <- plot1 + theme(legend.position = "none")

# Arrange the two plots side by side and add the shared legend below them
combined_plot <- grid.arrange(
  plot2, plot1,
  shared_legend,
  ncol = 2,
  nrow = 2,
  heights = c(10, 1)  # Adjust the heights to give space for the legend
)

  # Print the combined plot
print(combined_plot)






