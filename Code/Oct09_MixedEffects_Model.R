# Load necessary packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(lme4)
library(pROC)
library(cowplot)

# Regression.url <- "Code/TempData/regression_data.rds"
# df <- readRDS(Regression.url)

Cox_Hazard.url <- "Code/TempData/cox_hazard_data.rds"
df <- readRDS(Cox_Hazard.url)

# Process variables
df <- df %>%
  mutate(
    age_category = case_when(
      age_base < 20 ~ "<20",
      age_base >= 20 & age_base <= 40 ~ "20-40",
      age_base > 40 & age_base <= 60 ~ "40-60",
      age_base > 60 ~ ">60"
    ),
    sex = factor(sex, levels = c("Male", "Female")),
    otherdisease_S1 = factor(otherdisease_S1, levels = c("yes", "no")),
    latest_immunology_cat = factor(latest_immunology_cat, 
                                   levels = c("<1 month", "1-6 months", "6-12 months", ">1 year", "no_event")),
    S_num_S1_cat = cut(S_num_S1, breaks = c(-Inf, 6000, 15000, 24000, Inf),
                       labels = c("<6000", "6000-15000", "15000-24000", ">=24000"), 
                       right = FALSE, include.lowest = TRUE, addNA = TRUE), 
    response = ifelse(between_S1_S2_infec_cat == "no", 1, 0), 
    immune_type = factor(immune_type, levels = c("hybrid-induced", "vac-induced"))
  )

df <- df |> 
  select(response, between_S1_S2_infec_cat, age_category, immune_type, 
         S_num_S1, S_num_S1_cat, latest_immunology_cat, sex, otherdisease_S1)

# Fit the mixed-effects model
mixed_model <- glmer(response ~ S_num_S1 * immune_type + age_category * immune_type + 
                       (1 + S_num_S1 + age_category | immune_type), 
                     data = df, family = "binomial")

summary(mixed_model)

df$predicted_prob <- predict(mixed_model, type = "response")
# ROC curve
roc_curve <- roc(df$response, df$predicted_prob)

# Generate data for predictions
S_num_S1_range <- seq(min(df$S_num_S1), max(df$S_num_S1), length.out = 100)
age_cat_range <- unique(df$age_category)

combination_df <- expand.grid(S_num_S1 = S_num_S1_range, age_category = age_cat_range, 
                              immune_type = c("vac-induced", "hybrid-induced"))

combination_df$pred <- predict(mixed_model, newdata = combination_df, type = "response", re.form = NULL)


combination_df$age_category <- factor(combination_df$age_category, levels = c("<20", "20-40", "40-60", ">60"))

# Plot for vac-induced group
plot_vac <- ggplot(combination_df %>% filter(immune_type == "vac-induced"), 
                   aes(x = S_num_S1, y = pred, group = age_category, color = age_category)) +
  geom_line(linewidth = 1) +  
  labs(x = "S antibody from surveillance", y = "Probability of Remaining Free from Infection") +
  ggtitle("Vaccine-induced") + 
  theme_bw() +
  ylim(0.5, 1) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") 

crossing_points <- combination_df %>%
  filter(immune_type == "vac-induced") %>%
  group_by(age_category) %>%
  filter(pred >= 0.8) %>%
  summarise(crossing_x = min(S_num_S1))  

plot_vac1 <- plot_vac + 
  geom_segment(data = crossing_points, aes(x = crossing_x, xend = crossing_x, y = 0.5, yend = 0.8), 
               linetype = "dashed", color = "black") +  
  geom_point(data = crossing_points, aes(x = crossing_x, y = 0.8), shape = 16, size = 2, color = "red") + 
  theme(panel.grid.major = element_blank()) + 
  scale_y_continuous(limits = c(0.5,1), expand = c(0.002, 0.002))



# Plot for hybrid-induced group
# Hybrid-induced plot
plot_hybrid <- ggplot(combination_df %>% filter(immune_type == "hybrid-induced"), 
                      aes(x = S_num_S1, y = pred, group = age_category, color = age_category)) +
  geom_line(linewidth = 1) +  
  labs(x = "S antibody from surveillance", y = "Probability of Remaining Free from Infection") +
  ggtitle("Hybrid-induced") + 
  theme_bw() +
  ylim(0.5, 1) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) 



# Add the vertical lines, points, and grid adjustments similar to the vac-induced plot
plot_hybrid1 <- plot_hybrid + 
  theme(panel.grid.major = element_blank()) + 
  scale_y_continuous(limits = c(0.5, 1), expand = c(0.002, 0.002))


# Shared legend
shared_legend <- get_legend(plot_vac1 + theme(legend.position = "right"))
final_plot <- plot_grid(plot_hybrid1, plot_vac1, shared_legend, ncol = 3, rel_widths = c(1, 1, 0.2))
final_plot

ggsave("Results/plots_updated/mixed_effect_model(conservative_version).pdf", plot = final_plot, width = 12, height = 6)


