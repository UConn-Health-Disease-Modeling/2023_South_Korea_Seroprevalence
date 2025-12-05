rm(list = ls())

library(ggsurvfit)
library(survminer)
library(survival)
library(flexsurv)
source("code/FUN_HR_Plot.R")

url <- list(
  N_status = "code/TempData/data_Nstatus.rds", 
  KDCA = "code/TempData/data_KDCA.rds"
)

# load
data_Nstatus <- readRDS(url$N_status)
data_KDCA    <- readRDS(url$KDCA)

# clean function
clean_factors <- function(df) {
  df$vac <- as.numeric(df$immune_type == "vac-induced")
  df$inf <- as.numeric(df$immune_type == "inf-induced")
  
  df$sex <- factor(df$sex, levels = c("Male", "Female"))
  df$age_category <- factor(df$age_category,
                            levels = c(">60", "<20", "20-40", "40-60"))
  df
}

data_Nstatus <- clean_factors(data_Nstatus)
data_KDCA    <- clean_factors(data_KDCA)



################################################################################
# KDCA modeling 
fit_tv_cox_KDCA <- coxph(Surv(time, status) ~ vac + inf + age_category + sex +
                           tt(vac) + tt(inf),
                         data = data_KDCA,
                         weights = wgt_1ag,
                         tt = function(x, t, ...) x * log(t))

summary(fit_tv_cox_KDCA)
logHR_df <- extract_logHR_tt_cox(fit_tv_cox_KDCA, data_KDCA)

# Save plot to PNG
tiff("result/plot1/Figure3(A).tif", 
     width = 2400, height = 1600, res = 400, compression = "lzw")
plot_logHR_tt_cox(fit_tv_cox_KDCA, data_KDCA, y_range = c(-1.5, 5))
dev.off()


data_KDCA$vac_vs_inf <- ifelse(data_KDCA$immune_type == "vac-induced", 1, 0)
data_KDCA$hyb_vs_inf <- ifelse(data_KDCA$immune_type == "hybrid-induced", 1, 0)

fit_tv_cox_KDCA_infRef <- coxph(
  Surv(time, status) ~ vac_vs_inf + hyb_vs_inf + age_category + sex +
    tt(vac_vs_inf) + tt(hyb_vs_inf),
  data = data_KDCA,
  weights = wgt_1ag,
  tt = function(x, t, ...) x * log(t)
)
tiff("result/plot1/FigureS1(A).tif", 
     width = 2400, height = 1600, res = 400, compression = "lzw")
plot_logHR_tt_cox_infRef(fit_tv_cox_KDCA_infRef, data_KDCA, y_range = c(0, 4))
dev.off()

################################################################################
# KDCA modeling 
fit_tv_cox_Nstatus <- coxph(Surv(time, status) ~ vac + inf + age_category + sex +
                              tt(vac) + tt(inf),
                            data = data_Nstatus,
                            weights = wgt_1ag,
                            tt = function(x, t, ...) x * log(t))

summary(fit_tv_cox_Nstatus)
logHR_df <- extract_logHR_tt_cox(fit_tv_cox_Nstatus, data_Nstatus)
tiff("result/plot1/Figure3(B).tif",
     width = 2400, height = 1600, res = 400, compression = "lzw")
plot_logHR_tt_cox(fit_tv_cox_Nstatus, data_Nstatus, y_range = c(-1.5, 5))
dev.off()

data_Nstatus$vac_vs_inf <- ifelse(data_Nstatus$immune_type == "vac-induced", 1, 0)
data_Nstatus$hyb_vs_inf <- ifelse(data_Nstatus$immune_type == "hybrid-induced", 1, 0)

fit_tv_cox_Nstatus_infRef <- coxph(
  Surv(time, status) ~ vac_vs_inf + hyb_vs_inf + age_category + sex +
    tt(vac_vs_inf) + tt(hyb_vs_inf),
  data = data_Nstatus,
  weights = wgt_1ag,
  tt = function(x, t, ...) x * log(t)
)
tiff("result/plot1/FigureS1(B).tif", 
     width = 2400, height = 1600, res = 400, compression = "lzw")
plot_logHR_tt_cox_infRef(fit_tv_cox_Nstatus_infRef, data_Nstatus, y_range = c(0, 4))
dev.off()
