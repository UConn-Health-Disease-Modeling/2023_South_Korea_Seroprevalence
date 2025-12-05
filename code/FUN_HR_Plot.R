plot_logHR_tt_cox <- function(fit_tv_cox, data, y_range = NULL) {
  
  coef_vec <- coef(fit_tv_cox)
  vcov_mat <- vcov(fit_tv_cox)
  
  t_grid <- seq(0.1, max(data$time, na.rm = TRUE), length.out = 200)
  log_t <- log(t_grid)
  
  # Log-HR and variance for vac
  logHR_vac <- coef_vec["vac"] + coef_vec["tt(vac)"] * log_t
  var_logHR_vac <- vcov_mat["vac", "vac"] +
    log_t^2 * vcov_mat["tt(vac)", "tt(vac)"] +
    2 * log_t * vcov_mat["vac", "tt(vac)"]
  se_logHR_vac <- sqrt(var_logHR_vac)
  
  # Log-HR and variance for inf
  logHR_inf <- coef_vec["inf"] + coef_vec["tt(inf)"] * log_t
  var_logHR_inf <- vcov_mat["inf", "inf"] +
    log_t^2 * vcov_mat["tt(inf)", "tt(inf)"] +
    2 * log_t * vcov_mat["inf", "tt(inf)"]
  se_logHR_inf <- sqrt(var_logHR_inf)
  
  # Confidence intervals
  logHR_vac_low  <- logHR_vac - 1.96 * se_logHR_vac
  logHR_vac_high <- logHR_vac + 1.96 * se_logHR_vac
  
  logHR_inf_low  <- logHR_inf - 1.96 * se_logHR_inf
  logHR_inf_high <- logHR_inf + 1.96 * se_logHR_inf
  
  # y-axis limits
  if (is.null(y_range)) {
    y_min <- min(logHR_vac_low, logHR_inf_low)
    y_max <- max(logHR_vac_high, logHR_inf_high)
  } else {
    y_min <- y_range[1]
    y_max <- y_range[2]
  }
  
  par(family = "serif", mar = c(5, 5, 1, 1), las = 1)
  
  plot(t_grid, logHR_vac, type = "n",
       ylim = c(y_min, y_max),
       xlab = "Time (Months)",
       ylab = "Log-HR",
       cex.lab = 1.4, cex.axis = 1.2)
  
  grid(col = "lightgray", lty = "dotted", lwd = 0.8)
  
  # Confidence ribbons
  polygon(c(t_grid, rev(t_grid)),
          c(logHR_vac_low, rev(logHR_vac_high)),
          col = adjustcolor("forestgreen", alpha.f = 0.25), border = NA)
  
  polygon(c(t_grid, rev(t_grid)),
          c(logHR_inf_low, rev(logHR_inf_high)),
          col = adjustcolor("orange", alpha.f = 0.25), border = NA)
  
  lines(t_grid, logHR_vac, col = "forestgreen", lwd = 2)
  lines(t_grid, logHR_inf, col = "orange", lwd = 2, lty = 2)
  
  abline(h = 0, col = "gray40", lty = 1, lwd = 1)
  
  legend("topright",
         legend = c("Vaccine vs Hybrid", "Infection vs Hybrid"),
         col = c("forestgreen", "orange"),
         lty = c(1, 2),
         lwd = 2,
         bty = "n",
         cex = 1)
}

plot_HR_tt_cox <- function(fit_tv_cox, data) {
  # Extract coefficients and variance-covariance matrix
  coef_vec <- coef(fit_tv_cox)
  vcov_mat <- vcov(fit_tv_cox)
  
  # Create time grid
  t_grid <- seq(0.1, max(data$time, na.rm = TRUE), length.out = 200)
  log_t <- log(t_grid)
  
  # Log-HR and variance for vac
  logHR_vac <- coef_vec["vac"] + coef_vec["tt(vac)"] * log_t
  var_logHR_vac <- vcov_mat["vac", "vac"] +
    log_t^2 * vcov_mat["tt(vac)", "tt(vac)"] +
    2 * log_t * vcov_mat["vac", "tt(vac)"]
  se_logHR_vac <- sqrt(var_logHR_vac)
  
  # Log-HR and variance for inf
  logHR_inf <- coef_vec["inf"] + coef_vec["tt(inf)"] * log_t
  var_logHR_inf <- vcov_mat["inf", "inf"] +
    log_t^2 * vcov_mat["tt(inf)", "tt(inf)"] +
    2 * log_t * vcov_mat["inf", "tt(inf)"]
  se_logHR_inf <- sqrt(var_logHR_inf)
  
  # HRs and confidence intervals
  HR_vac <- exp(logHR_vac)
  HR_vac_lower <- exp(logHR_vac - 1.96 * se_logHR_vac)
  HR_vac_upper <- exp(logHR_vac + 1.96 * se_logHR_vac)
  
  HR_inf <- exp(logHR_inf)
  HR_inf_lower <- exp(logHR_inf - 1.96 * se_logHR_inf)
  HR_inf_upper <- exp(logHR_inf + 1.96 * se_logHR_inf)
  
  # Plot setup
  par(family = "serif", mar = c(4, 4, .5, .5))
  plot(t_grid, HR_vac, type = "n", 
       ylim = range(c(HR_vac_lower, HR_vac_upper, HR_inf_lower, HR_inf_upper)),
       xlab = "Months", ylab = "Hazard ratio", 
       cex.lab = 1.2, cex.axis = 1)
  
  # CI ribbons
  polygon(c(t_grid, rev(t_grid)),
          c(HR_vac_lower, rev(HR_vac_upper)),
          col = adjustcolor("forestgreen", alpha.f = 0.2), border = NA)
  
  polygon(c(t_grid, rev(t_grid)),
          c(HR_inf_lower, rev(HR_inf_upper)),
          col = adjustcolor("orange", alpha.f = 0.2), border = NA)
  
  # HR curves
  lines(t_grid, HR_vac, col = "forestgreen", lwd = 2)
  lines(t_grid, HR_inf, col = "orange", lwd = 2, lty = 2)
  
  # Reference line at HR = 1
  abline(h = 1, col = "gray40", lty = 1)
  
  # Legend
  legend("topright", legend = c("Vaccine vs Hybrid", "Infection vs Hybrid"),
         col = c("forestgreen", "orange"), lty = c(1, 2), lwd = 2, bty = "n", cex = 0.9)
}

extract_logHR_tt_cox <- function(fit_tv_cox, data) {
  # Extract coefficients and variance-covariance matrix
  coef_vec <- coef(fit_tv_cox)
  vcov_mat <- vcov(fit_tv_cox)
  
  # Create time grid
  t_grid <- seq(0.1, max(data$time, na.rm = TRUE), length.out = 200)
  log_t <- log(t_grid)
  
  # Log-HR and variance for vac
  logHR_vac <- coef_vec["vac"] + coef_vec["tt(vac)"] * log_t
  var_logHR_vac <- vcov_mat["vac", "vac"] +
    log_t^2 * vcov_mat["tt(vac)", "tt(vac)"] +
    2 * log_t * vcov_mat["vac", "tt(vac)"]
  se_logHR_vac <- sqrt(var_logHR_vac)
  
  # Log-HR and variance for inf
  logHR_inf <- coef_vec["inf"] + coef_vec["tt(inf)"] * log_t
  var_logHR_inf <- vcov_mat["inf", "inf"] +
    log_t^2 * vcov_mat["tt(inf)", "tt(inf)"] +
    2 * log_t * vcov_mat["inf", "tt(inf)"]
  se_logHR_inf <- sqrt(var_logHR_inf)
  
  # Build data frame
  data.frame(
    time = t_grid,
    logHR_vac = logHR_vac,
    logHR_vac_lower = logHR_vac - 1.96 * se_logHR_vac,
    logHR_vac_upper = logHR_vac + 1.96 * se_logHR_vac,
    logHR_inf = logHR_inf,
    logHR_inf_lower = logHR_inf - 1.96 * se_logHR_inf,
    logHR_inf_upper = logHR_inf + 1.96 * se_logHR_inf
  )
}

extract_HR_tt_cox <- function(fit_tv_cox, data) {
  # Extract coefficients and variance-covariance matrix
  coef_vec <- coef(fit_tv_cox)
  vcov_mat <- vcov(fit_tv_cox)
  
  # Create time grid
  t_grid <- seq(0.1, max(data$time, na.rm = TRUE), length.out = 200)
  log_t <- log(t_grid)
  
  # Log-HR and variance for vac
  logHR_vac <- coef_vec["vac"] + coef_vec["tt(vac)"] * log_t
  var_logHR_vac <- vcov_mat["vac", "vac"] +
    log_t^2 * vcov_mat["tt(vac)", "tt(vac)"] +
    2 * log_t * vcov_mat["vac", "tt(vac)"]
  se_logHR_vac <- sqrt(var_logHR_vac)
  
  # Log-HR and variance for inf
  logHR_inf <- coef_vec["inf"] + coef_vec["tt(inf)"] * log_t
  var_logHR_inf <- vcov_mat["inf", "inf"] +
    log_t^2 * vcov_mat["tt(inf)", "tt(inf)"] +
    2 * log_t * vcov_mat["inf", "tt(inf)"]
  se_logHR_inf <- sqrt(var_logHR_inf)
  
  # Return data frame with HRs
  data.frame(
    time = t_grid,
    HR_vac = exp(logHR_vac),
    HR_vac_lower = exp(logHR_vac - 1.96 * se_logHR_vac),
    HR_vac_upper = exp(logHR_vac + 1.96 * se_logHR_vac),
    HR_inf = exp(logHR_inf),
    HR_inf_lower = exp(logHR_inf - 1.96 * se_logHR_inf),
    HR_inf_upper = exp(logHR_inf + 1.96 * se_logHR_inf)
  )
}









plot_logHR_tt_cox_infRef <- function(fit_tv_cox, data, y_range = NULL) {
  
  coef_vec <- coef(fit_tv_cox)
  vcov_mat <- vcov(fit_tv_cox)
  
  t_grid <- seq(0.1, max(data$time, na.rm = TRUE), length.out = 200)
  log_t <- log(t_grid)
  
  logHR_vac <- coef_vec["vac_vs_inf"] + coef_vec["tt(vac_vs_inf)"] * log_t
  
  var_logHR_vac <- vcov_mat["vac_vs_inf", "vac_vs_inf"] +
    log_t^2 * vcov_mat["tt(vac_vs_inf)", "tt(vac_vs_inf)"] +
    2 * log_t * vcov_mat["vac_vs_inf", "tt(vac_vs_inf)"]
  
  se_logHR_vac <- sqrt(var_logHR_vac)
  
  logHR_vac_low  <- logHR_vac - 1.96 * se_logHR_vac
  logHR_vac_high <- logHR_vac + 1.96 * se_logHR_vac
  
  if (is.null(y_range)) {
    y_min <- min(logHR_vac_low) - 0.5
    y_max <- max(logHR_vac_high) + 0.5
  } else {
    y_min <- y_range[1]
    y_max <- y_range[2]
  }
  
  par(family = "serif", mar = c(5, 5, 1, 1), las = 1)
  
  plot(t_grid, logHR_vac, type = "n",
       ylim = c(y_min, y_max),
       xlab = "Time (Months)",
       ylab = "Log-HR",
       cex.lab = 1.4, cex.axis = 1.2)
  
  grid(col = "lightgray", lty = "dotted", lwd = 0.8)
  
  polygon(c(t_grid, rev(t_grid)),
          c(logHR_vac_low, rev(logHR_vac_high)),
          col = adjustcolor("steelblue2", alpha.f = 0.25), border = NA)
  
  lines(t_grid, logHR_vac, col = "steelblue4", lwd = 2)
  
  abline(h = 0, col = "gray40", lty = 2, lwd = 1.2)
  
  legend("topright",
         legend = c("Vaccine vs Infection"),
         col = c("steelblue4"),
         lty = 1,
         lwd = 2,
         bty = "n",
         cex = 1)
}
