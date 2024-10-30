library(survival)
library(survminer)
library(dplyr)
list.files()

# Load data
url <- "C:/Users/yizhamg/Dropbox/Jo_Franky/2023_South_Korea_Seroprevalence/Code/TempData/cox_hazard_data.rds"
data <- readRDS(url)

data <- data %>%
  mutate(
    time = ifelse(between_S1_S2_infec_cat == "yes", latest_immunology + infec_gap_after_S1, latest_immunology + gap_between_surveys)/30,
    status = ifelse(between_S1_S2_infec_cat == "yes", 1, 0)
  ) |> 
  mutate(age_category = case_when(
    age_base < 20 ~ "<20",
    age_base >= 20 & age_base <= 40 ~ "20-40",
    age_base > 40 & age_base <= 60 ~ "40-60",
    age_base > 60 ~ ">60"
  ))

data <- data %>% dplyr::select(GNO, time, status, age_category, immune_type)

data$immune_type <- factor(data$immune_type, levels = c("hybrid-induced", "vac-induced"))
data$age_category <- factor(data$age_category, levels = c("<20", "20-40", "40-60", ">60"))

aft_model <- survreg(
  Surv(time, status) ~ immune_type + age_category, 
  data = data, 
  dist = "weibull"
)

summary(aft_model)

coefficients <- aft_model$coefficients
scale_param <- aft_model$scale  # Weibull模型的尺度参数

time_points <- seq(1, max(data$time), by = 0.01)

groups <- expand.grid(
  immune_type = levels(data$immune_type),
  age_category = levels(data$age_category)
)

survival_results <- data.frame()

for (i in 1:nrow(groups)) {
  immune_type <- groups$immune_type[i]
  age_category <- groups$age_category[i]
  
  lp <- sum(
    coefficients["immune_typevac-induced"] * (immune_type == "vac-induced"),
    coefficients["age_category20-40"] * (age_category == "20-40"),
    coefficients["age_category40-60"] * (age_category == "40-60"),
    coefficients["age_category>60"] * (age_category == ">60")
  )
  
  survival_prob <- numeric(length(time_points))
  lower_bound <- numeric(length(time_points))
  upper_bound <- numeric(length(time_points))
  
  for (j in seq_along(time_points)) {
    t <- time_points[j]
    
    survival_prob[j] <- exp(-exp((log(t) - lp) / scale_param))
    
    lp_se <- sqrt(sum(diag(vcov(aft_model)) * 
                        c(
                          immune_type == "vac-induced",
                          age_category == "20-40",
                          age_category == "40-60",
                          age_category == ">60"
                        )^2))
    lower_bound[j] <- exp(-exp((log(t) - (lp - 1.96 * lp_se)) / scale_param))
    upper_bound[j] <- exp(-exp((log(t) - (lp + 1.96 * lp_se)) / scale_param))
  }
  
  group_results <- data.frame(
    time = time_points,
    survival_prob = survival_prob,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    immune_type = immune_type,
    age_category = age_category
  )
  
  survival_results <- rbind(survival_results, group_results)
}

head(survival_results)

survival_results$group <- interaction(survival_results$immune_type, survival_results$age_category, sep = ", ")

survival_results$group <- factor(
  survival_results$group, 
  levels = c(
    "hybrid-induced, <20", 
    "hybrid-induced, 20-40", 
    "hybrid-induced, 40-60", 
    "hybrid-induced, >60",
    "vac-induced, <20", 
    "vac-induced, 20-40", 
    "vac-induced, 40-60", 
    "vac-induced, >60"
  )
)

colors <- c(
  "#ADD8E6", "#6495ED",  # Blue shades for "hybrid-induced"
  "#0000FF", "#00008B",
  "#FFA07A", "#FF6347",  # Red shades for "vac-induced"
  "#DC143C", "#800000"
)

labels <- c(
  "hybrid-induced, <20", 
  "hybrid-induced, 20-40", 
  "hybrid-induced, 40-60", 
  "hybrid-induced, >60",
  "vac-induced, <20", 
  "vac-induced, 20-40", 
  "vac-induced, 40-60", 
  "vac-induced, >60"
)

ggplot(survival_results, aes(x = time, y = survival_prob, color = group, fill = group)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2, linetype = 0) +
  geom_line(size = 1) +
  scale_color_manual(values = colors, labels = labels) +
  scale_fill_manual(values = colors, labels = labels) +
  labs(
    x = "Time (Months)", 
    y = "Survival Probability", 
    title = "Survival Curves with Confidence Intervals by Immune Type and Age Category",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )















data$immune_type <- factor(data$immune_type, levels = c("vac-induced", "hybrid-induced"))
data$age_category <- factor(data$age_category, levels = c("<20", "20-40", "40-60", ">60"))
# continue to do the cox-hazard regression (Undjusted Model)
cox_model <- coxph(Surv(time / 30, status) ~ age_category, data = data)
summary(cox_model)



# continue to do the cox-hazard regression (Adjusted Model)
cox_model <- coxph(Surv(time / 30, status) ~ immune_type + sex + age_category, data = data)
summary(cox_model)
