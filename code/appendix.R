library(dplyr)

rm(list = ls())
url <- list(
  N_status = "code/TempData/data_Nstatus.rds", 
  KDCA = "code/TempData/data_KDCA.rds"
)

# load data
data_KDCA <- readRDS(url$KDCA)
# hybrid_KDCA <- data_KDCA %>% filter(immune_type == "inf-induced")
hybrid_KDCA <- data_KDCA %>% filter(immune_type == "hybrid-induced")
hybrid_KDCA$N_log <- log(hybrid_KDCA$N_num_S2/hybrid_KDCA$N_num_S1, base = 2)

thresholds <- seq(from = min(hybrid_KDCA$N_log, na.rm = TRUE), 
                  to = max(hybrid_KDCA$N_log, na.rm = TRUE),
                  by = 0.01)  
hybrid_KDCA <- hybrid_KDCA %>% filter(!is.na(N_log))



total_inf <- sum(hybrid_KDCA$between_S1_S2_infec_cat == "yes")
total_not <- sum(hybrid_KDCA$between_S1_S2_infec_cat == "no")

results <- data.frame(threshold = thresholds) %>%
  rowwise() %>%
  mutate(
    type_I_error_rate  = sum(hybrid_KDCA$between_S1_S2_infec_cat == "yes"   & hybrid_KDCA$N_log <= threshold) / total_inf,
    type_II_error_rate = sum(hybrid_KDCA$between_S1_S2_infec_cat == "no" & hybrid_KDCA$N_log >= threshold) / total_not,
    specificity = 1 - type_I_error_rate, 
    sensitivity = 1 - type_II_error_rate
  ) %>%
  ungroup() %>% 
  select(threshold, specificity, sensitivity)


# results$specificity <- pmin(results$specificity * factor, 1)
# results$sensitivity <- pmin(results$sensitivity * factor, 1)


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
                                  levels = c("specificity", "sensitivity"))
  
  # Define colors using viridis
  categories <- unique(results_long$category)
  colors <- c(
    adjustcolor("red", alpha.f = 0.8),
    adjustcolor("blue", alpha.f = 0.8)
  )
  
  # Filter for highlighted thresholds
  highlight_data <- result[
    result$specificity > 0.88 & result$sensitivity > 0.78,
  ]
  
  # Base plot
  plot(NULL,
       xlim = c(min(results_long$threshold), max(results_long$threshold) + 0.5),
       ylim = range(results_long$proportion, na.rm = TRUE),
       xlab = expression(log[2](N[2]/N[1])),
       ylab = "",
       family = "AppleGothic", 
       cex.lab = 1.4)
  
  # Plot lines by category
  for (i in seq_along(categories)) {
    cat_data <- results_long[results_long$category == categories[i], ]
    lines(cat_data$threshold, cat_data$proportion,
          col = colors[i],
          lwd = 2)
  }

  # Add black rectangles on the x-axis
  if (nrow(highlight_data) > 0) {
    rect(xleft = highlight_data$threshold - 0.05,
         xright = highlight_data$threshold + 0.05,
         ybottom = par("usr")[3],
         ytop = par("usr")[3] + 0.01 * diff(par("usr")[3:4]),
         col = "black",
         border = NA)
  }

  # Add the ribbon (shaded area across full y-range)
  rect(xleft = highlight_data$threshold[1] - 0.05,
       xright = tail(highlight_data$threshold, 1) + 0.05,
       ybottom = par("usr")[3],  # Bottom of plot
       ytop = par("usr")[4],     # Top of plot
       col = adjustcolor("grey", alpha.f = 0.3),  # semi-transparent grey
       border = NA)





  points(x = highlight_data$threshold[1], 
         y = highlight_data$specificity[1],
        pch = 16, col = "black", cex = .5)
  text(x = highlight_data$threshold[1], 
       y = highlight_data$specificity[1] - 0.03,
       labels = paste0(round(highlight_data$specificity[1] * 100, 1), "%"),
       pos = 2, cex = .8, col = "black")

  points(x = highlight_data$threshold[1], 
         y = highlight_data$sensitivity[1],
         pch = 16, col = "black", cex = .5)
  text(x = highlight_data$threshold[1], 
       y = highlight_data$sensitivity[1],
       labels = paste0(round(highlight_data$sensitivity[1] * 100, 1), "%"),
       pos = 2, cex = .8, col = "black")


  points(x = tail(highlight_data$threshold, 1),
         y = tail(highlight_data$specificity, 1),
         pch = 16, col = "black", cex = .5)
  text(x = tail(highlight_data$threshold, 1),
       y = tail(highlight_data$specificity, 1) + 0.02,
       labels = paste0(round(tail(highlight_data$specificity, 1) * 100, 1), "%"),
       pos = 4, cex = .8, col = "black")

  points(x = tail(highlight_data$threshold, 1),
         y = tail(highlight_data$sensitivity, 1),
         pch = 16, col = "black", cex = .5)
  text(x = tail(highlight_data$threshold, 1),
       y = tail(highlight_data$sensitivity, 1) - 0.02,
       labels = paste0(round(tail(highlight_data$sensitivity, 1) * 100, 1), "%"),
       pos = 4, cex = .8, col = "black")
  

  
  # Add legend
  legend("topright",
         legend = c("specificity", "sensitivity"),
         col = colors,
         lwd = 2,
         bty = "o",
         bg = "white",
         box.col = "black",
         inset = c(0.01, 0.01),
         x.intersp = 0.7,
         y.intersp = 0.9, 
         cex = 1)
}


png("result/plots/figS1.png", width = 2400, height = 1600, res = 350)
par(mar = c(4, 2, 1, 1))
plot_bias_proportion(results)
dev.off()







# ------------------------------------------------------------------------------
# Additional check for different age group 

url <- list(
  N_status = "code/TempData/data_Nstatus.rds", 
  KDCA = "code/TempData/data_KDCA.rds"
)

# load data
data_KDCA <- readRDS(url$KDCA)
hybrid_KDCA <- data_KDCA %>% filter(immune_type == "hybrid-induced")
hybrid_KDCA$N_log <- log(hybrid_KDCA$N_num_S2/hybrid_KDCA$N_num_S1, base = 2)

thresholds <- seq(from = min(hybrid_KDCA$N_log, na.rm = TRUE), 
                  to = max(hybrid_KDCA$N_log, na.rm = TRUE),
                  by = 0.01)  


hybrid_KDCA <- hybrid_KDCA %>% filter(!is.na(N_cha_S2))

# stratify by the latest immunological event gap 
# hybrid_KDCA <- hybrid_KDCA %>% filter(sex %in% c("Male"))

hybrid_KDCA <- hybrid_KDCA %>% filter(latest_immunology_cat %in% c("<1 month")) # figS2(1).png
# hybrid_KDCA <- hybrid_KDCA %>% filter(latest_immunology_cat %in% c("6-12 months", ">1 year"))  # figS2(2).png


total_inf <- sum(hybrid_KDCA$between_S1_S2_infec_cat == "yes")
total_not <- sum(hybrid_KDCA$between_S1_S2_infec_cat == "no")

results <- data.frame(threshold = thresholds) %>%
  rowwise() %>%
  mutate(
    type_I_error_rate  = sum(hybrid_KDCA$between_S1_S2_infec_cat == "yes"   & hybrid_KDCA$N_log <= threshold) / total_inf,
    type_II_error_rate = sum(hybrid_KDCA$between_S1_S2_infec_cat == "no" & hybrid_KDCA$N_log >= threshold) / total_not,
    specificity = 1 - type_I_error_rate, 
    sensitivity = 1 - type_II_error_rate
  ) %>%
  ungroup() %>% 
  select(threshold, specificity, sensitivity)



png("../result/plots/figS2(1).png", width = 2400, height = 1600, res = 350)
par(mar = c(4, 4, 1, 1))
plot_bias_proportion(results)
dev.off()




