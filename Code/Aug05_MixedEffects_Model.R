# Load necessary packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(lme4)
library(pROC)
library(cowplot)

# Load the data
url <- "Code/TempData/Mar17_Case_Study_Data.csv"
df <- read.csv(url)

# Process variables
df <- df %>%
  mutate(
    age_category = case_when(
      age < 20 ~ "<20",
      age >= 20 & age <= 40 ~ "20-40",
      age > 40 & age <= 60 ~ "40-60",
      age > 60 ~ ">60"
    ),
    sex = factor(sex, levels = c("Male", "Female")),
    edu = factor(edu, levels = c("Primary School", "Middle/High School", "Postsecondary")),
    income_cat = factor(income_cat, levels = c("<3000", "3000-6000", "6000-9000", ">9000")),
    otherdisease_S1 = factor(otherdisease_S1, levels = c("yes", "no")),
    case_study = factor(case_study, levels = c("case", "control")),
    latest_immunology_cat = factor(latest_immunology_cat, 
                                   levels = c("<1 month", "1-6 months", "6-12 months", ">1 year", "no_event")),
    S_num_S1_cat = cut(S_num_S1, breaks = c(-Inf, 6000, 15000, 24000, Inf),
                       labels = c("<6000", "6000-15000", "15000-24000", ">=24000"), 
                       right = FALSE, include.lowest = TRUE, addNA = TRUE),
    induced_index = factor(induced_index, levels = c("hybrid-induced", "vac-induced", "naive")),
    response = ifelse(case_study == "control", 1, 0)
  )

# Filter data for the mixed-effects model
df1 <- df %>% filter(induced_index %in% c("vac-induced", "hybrid-induced"))

# Fit the mixed-effects model
mixed_model <- glmer(response ~ S_num_S1 * induced_index + age_category * induced_index + 
                       (1 + S_num_S1 + age_category | induced_index), 
                     data = df1, family = "binomial")

# Compute predicted probabilities
df1$predicted_prob <- predict(mixed_model, type = "response")

# ROC curve
roc_curve <- roc(df1$response, df1$predicted_prob)

# Generate data for predictions
S_num_S1_range <- seq(min(df1$S_num_S1), max(df1$S_num_S1), length.out = 100)
age_cat_range <- unique(df1$age_category)

combination_df <- expand.grid(S_num_S1 = S_num_S1_range, age_category = age_cat_range, 
                              induced_index = c("vac-induced", "hybrid-induced"))

combination_df$pred <- predict(mixed_model, newdata = combination_df, type = "response", re.form = NULL)

# Plot for vac-induced group
plot_vac <- ggplot(combination_df %>% filter(induced_index == "vac-induced"), 
                   aes(x = S_num_S1, y = pred, group = age_category, color = age_category)) +
  geom_line(linewidth = 1) +  
  labs(x = "S antibody from surveillance", y = "Probability of Remaining Free from Infection") +
  ggtitle("Vaccine-induced") + theme_bw() +
  ylim(0.5, 1) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Plot for hybrid-induced group
plot_hybrid <- ggplot(combination_df %>% filter(induced_index == "hybrid-induced"), 
                      aes(x = S_num_S1, y = pred, group = age_category, color = age_category)) +
  geom_line(linewidth = 1) +  
  labs(x = "S antibody from surveillance", y = "Probability of Remaining Free from Infection") +
  ggtitle("Hybrid-induced") + theme_bw() +
  ylim(0.5, 1) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Shared legend
shared_legend <- get_legend(plot_vac + theme(legend.position = "right"))

# Arrange the plots
final_plot <- plot_grid(plot_hybrid, plot_vac, shared_legend, ncol = 3, rel_widths = c(1, 1, 0.2))

# Display final plot
print(final_plot)

# Compare ROC curves: simple logistic regression vs mixed-effects model
df1 <- df1 %>% select(response, S_num_S1, age_category, induced_index)

simple_model <- glm(response ~ S_num_S1 + age_category, data = df1, family = "binomial")
df1$simple_predicted_prob <- predict(simple_model, type = "response")

df1$mixed_predicted_prob <- predict(mixed_model, type = "response")

# ROC for simple model
roc_simple <- roc(df1$response, df1$simple_predicted_prob)
roc_mixed <- roc(df1$response, df1$mixed_predicted_prob)

# ROC plot
ggplot() +
  geom_line(data = as.data.frame(coords(roc_simple, "all", ret = c("specificity", "sensitivity"))), 
            aes(x = 1 - specificity, y = sensitivity, color = "Simple Logistic Regression"), linewidth = 1) +
  geom_line(data = as.data.frame(coords(roc_mixed, "all", ret = c("specificity", "sensitivity"))), 
            aes(x = 1 - specificity, y = sensitivity, color = "Mixed-Effects Logistic Regression"), linewidth = 1) +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_bw() +
  ggtitle("ROC Curve") + 
  scale_color_manual(values = c("Simple Logistic Regression" = "blue", "Mixed-Effects Logistic Regression" = "red")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  coord_equal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = c(0.95, 0.05), legend.justification = c("right", "bottom"))

# AUC comparison
auc_simple <- auc(roc_simple)
auc_mixed <- auc(roc_mixed)
print(paste("AUC for Simple Logistic Model:", auc_simple))
print(paste("AUC for Mixed-Effects Model:", auc_mixed))

