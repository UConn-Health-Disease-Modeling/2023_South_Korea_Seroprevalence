library(dplyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(ggsurvfit)
library(survminer)
library(survival)
library(flexsurv)

# load data 
url <- "Code/TempData/cox_hazard_data.rds"
data <- readRDS(url)

data <- data %>%
  mutate(time = ifelse(between_S1_S2_infec_cat == "yes", latest_immunology + infec_gap_after_S1, latest_immunology + gap_between_surveys),
         status = ifelse(between_S1_S2_infec_cat == "yes", 1, 0)) |> 
  mutate(age_category = case_when(
    age_base < 20 ~ "<20",
    age_base >= 20 & age_base <= 40 ~ "20-40",
    age_base > 40 & age_base <= 60 ~ "40-60",
    age_base > 60 ~ ">60"))

data$immune_type <- factor(
  data$immune_type, 
  levels = c("hybrid-induced", "vac-induced")
)

table(data$immune_type, data$age_category)

# Fit the survival model
fit <- survfit(Surv(time / 30, status) ~ immune_type, data = data)

# Define the colors for the groups
colors <- c(
  # Blue shades for "hybrid-induced"
  "#0000FF", 
  
  # Red shades for "vac-induced"
  "#DC143C"
)

# Define the labels for the groups
labels <- c(
  "hybrid-induced",
  "vac-induced"
)

# Visualize the result
ggsurv <- ggsurvplot(
  fit = fit, 
  data = data,
  risk.table = TRUE,  
  risk.table.col = "strata", 
  risk.table.height = 0.25, 
  risk.table.y.text.col = TRUE, 
  risk.table.y.text = TRUE, 
  ncensor.plot = TRUE,
  ncensor.plot.height = 0.25, 
  pval = TRUE,
  conf.int = TRUE, 
  palette = colors,    # Use the updated colors vector
  legend.labs = labels,
  xlab = "Months", 
  ylab = "Overall Survival Probability",
  break.time.by = 6,
  ggtheme = theme_minimal() +
    theme(
      legend.position = "right",  # Position the legend
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      plot.margin = unit(c(1, 4, 1, 1), "cm"),
      panel.border = element_rect(color = "black", fill = NA, size = 1)
    ),
  ylim = c(0.4, 1)  # Set y-axis limits
)


p1 <- ggsurv$plot + 
  theme(
    legend.position = c(0.08, 0.08),
    legend.text = element_text(size = 12),  # Reduce text size
    legend.title = element_text(size = 12),  # Adjust title size if needed
    legend.key.size = unit(0.5, "cm")  # Reduce key size (legend boxes)
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE, 
                               title.position = "top",
                               title.hjust = 0.5,  # Center the title
                               label.position = "right",  # Adjust label positions
                               label.hjust = 0)) + 
  ylab("Probability of protective effectiveness against infection") + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
# ggsave("Results/plots_updated/fig3(1).pdf", plot = p1, width = 10, height = 6)


p2 <- ggsurv$table +
  theme(legend.position = "none") + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
# ggsurv$ncensor.plot
# ggsave("Results/plots_updated/fig3(2).pdf", plot = p2, width = 10, height = 1)




data$immune_type <- factor(data$immune_type, levels = c("vac-induced", "hybrid-induced"))
data$age_category <- factor(data$age_category, levels = c("<20", "20-40", "40-60", ">60"))
# continue to do the cox-hazard regression (Undjusted Model)



# c("weibull", "exponential", "lognormal", "loglogistic", "gaussian", "logistic")

aft_model <- survreg(Surv(time / 30, status) ~ immune_type, data = data, dist = "loglogistic")
summary(aft_model)


data$sex <- factor(data$sex, levels = c("Male", "Female"))
data$immune_type <- factor(data$immune_type, levels = c("hybrid-induced", "vac-induced"))
# AFT model for sex7
aft_model <- survreg(Surv(time / 30, status) ~ sex*strata(immune_type), data = data, dist = "loglogistic")
summary(aft_model)

# AFT model for age_category
aft_model <- survreg(Surv(time / 30, status) ~ age_category*strata(immune_type), data = data, dist = "loglogistic")
summary(aft_model)

# Adjusted AFT model for immune_type, sex, and age_category
aft_model <- survreg(Surv(time / 30, status) ~ strata(immune_type)*sex + strata(immune_type)*age_category, data = data, dist = "loglogistic")
summary(aft_model)


data$sex
data$age_category
