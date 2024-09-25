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
library(lme4)
library(cowplot)

################################################################################
# get the url
url <- "Code/TempData/Mar17_Case_Study_Data.csv"
df <- read.csv(url)

# table(df$induced_index)

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



################################################################################
# fit the mixed-effects model
df$response <- ifelse(df$case_study == "control", 1, 0)

df1 <- df %>% 
  dplyr::filter(
    induced_index == "vac-induced" | induced_index == "hybrid-induced"
  )

# Fit the mixed-effects model
mixed_model <- glmer(response ~ S_num_S1 * induced_index + age_category * induced_index + 
                       (1 + S_num_S1 + age_category | induced_index), 
                     data = df1, 
                     family = "binomial")

# Compute predicted probabilities for the mixed model
df1$predicted_prob <- predict(mixed_model, type = "response")

# Compute the ROC curve
roc_curve <- roc(df1$response, df1$predicted_prob)

# Generate prediction data for the vac and hybrid models
S_num_S1_range <- seq(
  min(df1$S_num_S1), max(df1$S_num_S1), length.out = 100
)

age_cat_range <- unique(df1$age_category)

combination_df <- expand.grid(
  S_num_S1 = S_num_S1_range, 
  age_category = age_cat_range, 
  induced_index = c("vac-induced", "hybrid-induced")
)

combination_df$pred <- predict(
  mixed_model, 
  newdata = combination_df, 
  type = "response", 
  re.form = NULL
)

# Plot for vac-induced group
plot_vac <- ggplot(combination_df %>% 
                     filter(induced_index == "vac-induced"), 
                   aes(x = S_num_S1, 
                       y = pred, 
                       group = age_category, 
                       color = age_category)) +
  geom_line(linewidth = 1) +  
  labs(x = "S antibody from the 1\U02E2\U1D57 surveillance", 
       y = "Probability of Remaining Free from Infection") +
  ggtitle("Vaccine-induced") + 
  theme_bw() +
  ylim(0.5, 1) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) # Center and enlarge title

# Plot for hybrid-induced group
plot_hybrid <- ggplot(combination_df %>% 
                        filter(induced_index == "hybrid-induced"), 
                      aes(x = S_num_S1, 
                          y = pred, 
                          group = age_category, 
                          color = age_category)) +
  geom_line(linewidth = 1) +  
  labs(x = "S antibody from the 1\U02E2\U1D57 surveillance", 
       y = "Probability of Remaining Free from Infection") +
  ggtitle("Hybrid-induced") + 
  theme_bw() +
  ylim(0.5, 1) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) # Center and enlarge title

# Extract legend from one of the plots
shared_legend <- get_legend(plot_vac + theme(legend.position = "right"))

# Arrange the two plots with the shared legend
final_plot <- plot_grid(plot_hybrid, plot_vac, shared_legend, 
                        ncol = 3, 
                        rel_widths = c(1, 1, 0.2))

# Display the final plot
final_plot

# Display the final plot
print(final_plot)



################################################################################
# compare the ROC curves between simple logistic regresison and mixed-effects model
df1 <- df1 %>% 
  dplyr::select(
    response, 
    S_num_S1, 
    age_category, 
    induced_index
  )
simple_model <- glm(data = df1, 
                    response ~ S_num_S1 + age_category, 
                    family = "binomial")
df1$simple_predicted_prob <- predict(simple_model, type = "response")

mixed_model <- glmer(response ~ S_num_S1 * induced_index + age_category * induced_index + 
                       (1 + S_num_S1 + age_category | induced_index), 
                     data = df1, 
                     family = "binomial")
df1$mixed_predicted_prob <- predict(mixed_model, type = "response")

summary(simple_model)
summary(mixed_model)



################################################################################
# Compute the ROC curve for the simple model
roc_simple <- roc(df1$response, df1$simple_predicted_prob)
roc_simple_coords <- coords(roc_simple, "all", ret = c("specificity", "sensitivity"))

# Compute the ROC curve for the mixed-effects model
roc_mixed <- roc(df1$response, df1$mixed_predicted_prob)
roc_mixed_coords <- coords(roc_mixed, "all", ret = c("specificity", "sensitivity"))

# Add (0,0) and (1,1) to the coordinates for complete ROC curve plotting
roc_simple_coords <- rbind(c(1, 0), roc_simple_coords, c(0, 1))
roc_mixed_coords <- rbind(c(1, 0), roc_mixed_coords, c(0, 1))

# Convert specificity to 1 - specificity for plotting
roc_simple_coords[, "specificity"] <- 1 - roc_simple_coords[, "specificity"]
roc_mixed_coords[, "specificity"] <- 1 - roc_mixed_coords[, "specificity"]

ggplot() +
  geom_line(data = as.data.frame(roc_simple_coords), 
            aes(x = specificity, y = sensitivity, color = "Simple Logistic Regression"), 
            linewidth = 1) +
  geom_line(data = as.data.frame(roc_mixed_coords), 
            aes(x = specificity, y = sensitivity, color = "Mixed-Effects Logistic Regression"), 
            linewidth = 1) +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       color = "Model") +
  theme_bw() +
  ggtitle("ROC Curve") + 
  scale_color_manual(values = c("Simple Logistic Regression" = "blue", "Mixed-Effects Logistic Regression" = "red")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  coord_equal() +
  xlim(0, 1) +
  ylim(0, 1) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom")
  )




################################################################################
# Compute the ROC curve
roc_simple <- roc(df1$response, df1$simple_predicted_prob)

# Calculate AUC
auc_simple <- auc(roc_simple)
print(paste("AUC for Simple Logistic Model:", auc_simple))

# Compute the ROC curve
roc_mixed <- roc(df1$response, df1$mixed_predicted_prob)

# Calculate AUC
auc_mixed <- auc(roc_mixed)
print(paste("AUC for Mixed-Effects Model:", auc_mixed))



