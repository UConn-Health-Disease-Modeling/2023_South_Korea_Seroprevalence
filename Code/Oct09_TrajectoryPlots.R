library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)

# ######################
# ## Process the data ##
# ######################

# load the processed data 
url <- "Code/TempData/cox_hazard_data.rds"
df <- readRDS(url)

df <- df %>% mutate(latest_immunology = ifelse(is.na(vac_gap_before_S1), infec_gap_before_S1,
                                               ifelse(is.na(infec_gap_before_S1), vac_gap_before_S1,
                                                      ifelse(vac_gap_before_S1 < infec_gap_before_S1,
                                                             vac_gap_before_S1, infec_gap_before_S1))))

df <- df %>% mutate(latest_immunology2 = ifelse(is.na(vac_gap_before_S2), infec_gap_before_S2,
                                                ifelse(is.na(infec_gap_before_S2), vac_gap_before_S2,
                                                       ifelse(vac_gap_before_S2 < infec_gap_before_S2,
                                                              vac_gap_before_S2, infec_gap_before_S2))))

df <- df %>% mutate(latest_immunology_cat = ifelse(latest_immunology >=0 & latest_immunology <=30, "<1 month",
                                                   ifelse(latest_immunology > 30 &
                                                            latest_immunology <= 180, "1-6 months",
                                                          ifelse(latest_immunology > 180 &
                                                                   latest_immunology <= 365, "6-12 months", ">1 year"))))
df[is.na(df$latest_immunology_cat), ]$latest_immunology_cat <- "no_event"
df$latest_immunology_cat <- factor(df$latest_immunology_cat, levels = c("<1 month", "1-6 months", "6-12 months",
                                                                        ">1 year", "no_event"))

df <- df |> mutate(immune_type = ifelse(group1 == 4, "naive",
                                        ifelse(group1 == 3, "vac-induced",
                                               ifelse(group1 == 2, "inf-induced",
                                                      ifelse(group1 == 1, "hybrid-induced", NA)))))

df <- df %>% filter(immune_type == "hybrid-induced" | immune_type == "vac-induced")
df <- df %>% mutate(immune_type2 = ifelse(immune_type == "hybrid-induced", "hybrid-induced",
                                          ifelse(between_S1_S2_infec_cat == "yes", "hybrid-induced",
                                                 "vac-induced")))
df$immune_type2 <- factor(df$immune_type2, levels = c("hybrid-induced", "vac-induced"))

df <- df |> mutate(age_cat = case_when(
  age_base < 20 ~ "<20",
  age_base >= 20 & age_base <= 40 ~ "20-40",
  age_base > 40 & age_base <= 60 ~ "40-60",
  age_base > 60 ~ ">60"
))

df$age_cat <- factor(df$age_cat, levels = c("<20", "20-40", "40-60", ">60"))

# table(df$immune_type2)
# # hybrid-induced    vac-induced 
# # 4755           2550 
# table(df$immune_type)
# # hybrid-induced    vac-induced 
# # 3976           3329 

df <- df |> select(GNO, 
                    S_num_S1, S_num_S2,
                    latest_immunology, latest_immunology2,
                    age_cat, sex, 
                    immune_type, immune_type2)

df1 <- df |> select(GNO, S_num_S1, latest_immunology, age_cat, sex, immune_type)
df2 <- df |> select(GNO, S_num_S2, latest_immunology2, age_cat, sex, immune_type2)

cols <- c("ID", "S_ab", "latest_immunology", "age", "sex", "immune_type")
colnames(df1) <- cols
colnames(df2) <- cols

df3 <- rbind(df1, df2)

df3 <- df3 %>% filter(!(immune_type == "hybrid-induced" & latest_immunology >= 420 & latest_immunology < 450 & S_ab > 20000))

# saveRDS(df3, file = "Code/TempData/Oct09_TrajectoryPlotsData.rds")

# ######################
# ######## end #########
# ######################





plot_data <- function(data){
  # data <- df
  data <- data %>% filter(S_ab > .8)
  data <- data %>%
    select(ID, S_ab, latest_immunology, age, sex, immune_type) %>%
    mutate(latest_immunology_cat = case_when(
      is.na(latest_immunology) ~ "no_event",
      latest_immunology < 30 ~ "1",
      latest_immunology >= 30 & latest_immunology < 60 ~ "2",
      latest_immunology >= 60 & latest_immunology < 90 ~ "3",
      latest_immunology >= 90 & latest_immunology < 120 ~ "4",
      latest_immunology >= 120 & latest_immunology < 150 ~ "5",
      latest_immunology >= 150 & latest_immunology < 180 ~ "6",
      latest_immunology >= 180 & latest_immunology < 210 ~ "7",
      latest_immunology >= 210 & latest_immunology < 240 ~ "8",
      latest_immunology >= 240 & latest_immunology < 270 ~ "9",
      latest_immunology >= 270 & latest_immunology < 300 ~ "10",
      latest_immunology >= 300 & latest_immunology < 330 ~ "11", 
      latest_immunology >= 330 & latest_immunology < 360 ~ "12", 
      latest_immunology >= 360 & latest_immunology < 390 ~ "13", 
      latest_immunology >= 390 & latest_immunology < 420 ~ "14", 
      latest_immunology >= 420 & latest_immunology < 450 ~ "15", 
      TRUE ~ "over15"
    )) %>%
    mutate(latest_immunology_cat = factor(latest_immunology_cat, levels = c(
      "no_event", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
      "11", "12", "13", "14", "15", 
      "over15"
    ))) %>%
    filter(latest_immunology_cat != "no_event")
  
  df_summary <- data %>%
    group_by(latest_immunology_cat, immune_type
             , age
    ) %>%
    summarize(mean_S_ab = mean(S_ab, na.rm = TRUE),
              sd_S_ab = sd(S_ab, na.rm = TRUE),  # Calculate standard deviation
              .groups = 'drop') %>%
    mutate(ymin = pmax(mean_S_ab - sd_S_ab, 0.1),  # Ensure ymin is positive and non-zero
           ymax = mean_S_ab + sd_S_ab) %>%
    na.omit()  # Remove NAs that may result from sd calculation
  
  
  # Round mean values to integer for labels
  df_summary$mean_S_ab <- round(df_summary$mean_S_ab)
  df_summary$ymin <- round(df_summary$ymin)
  df_summary$ymax <- round(df_summary$ymax)
  return(df_summary)
}





# load the aforementioned data 
url <- "Code/TempData/Oct09_TrajectoryPlotsData.rds"
df <- readRDS(url)


# Define the colors for the groups
colors <- c("#ADD8E6", "#6495ED", "#0000FF", "#191970", "#FFA07A", "#FF6347", "#DC143C", "#800000")
labels <- c("hybrid-induced, <20", "hybrid-induced, 20-40", "hybrid-induced, 40-60", "hybrid-induced, >60",
            "vac-induced, <20", "vac-induced, 20-40", "vac-induced, 40-60", "vac-induced, >60")

# Filter the vac- and hybrid- groups 
plot_df_vac <- plot_df %>% filter(immune_type == "vac-induced")
plot_df_hybrid <- plot_df %>% filter(immune_type == "hybrid-induced")

# Plot 1: Hybrid-induced group (individual points and lines)
plot1 <- ggplot(plot_df_hybrid, aes(x = latest_immunology_cat, y = mean_S_ab)) +
  geom_point(aes(color = age), size = 3) +
  geom_line(aes(group = age, color = age)) + 
  scale_color_manual(values = c("<20" = "#ADD8E6", 
                                "20-40" = "#6495ED", 
                                "40-60" = "#0000FF", 
                                ">60" = "#00008B")) + 
  theme_bw() + 
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        plot.title = element_blank()) +  # Remove individual titles
  ylim(0, 25000) +
  labs(x = "Time Since the Latest Immunology (month)", y = "S Antibody Level")

# Plot 2: Vaccine-induced group (individual points and lines)
plot2 <- ggplot(plot_df_vac, aes(x = latest_immunology_cat, y = mean_S_ab)) +
  geom_point(aes(color = age), size = 3) +
  geom_line(aes(group = age, color = age)) + 
  scale_color_manual(values = c("<20" = "#FFA07A", 
                                "20-40" = "#FF6347", 
                                "40-60" = "#DC143C", 
                                ">60" = "#800000")) + 
  theme_bw() + 
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        plot.title = element_blank()) +  # Remove individual titles
  ylim(0, 25000) +
  labs(x = "Time Since the Latest Immunology (month)", y = "S Antibody Level")

# Plot 3: Hybrid-induced group (average line)
plot3 <- ggplot(plot_df.avg_hybrid, aes(x = latest_immunology_cat, y = mean_S_ab.avg, group = 1)) +  
  geom_point(size = 3, color = "#0000FF") +
  geom_line(color = "#0000FF") + 
  theme_bw() + 
  theme(legend.position = "none", # Hide legend for this plot
        plot.title = element_blank()) +  # Remove individual titles
  ylim(0, 25000) +                                      
  labs(x = "Time Since the Latest Immunology (month)", y = "S Antibody Level")

# Plot 4: Vaccine-induced group (average line)
plot4 <- ggplot(plot_df.avg_vac, aes(x = latest_immunology_cat, y = mean_S_ab.avg, group = 1)) +  
  geom_point(size = 3, color = "#DC143C") +
  geom_line(color = "#DC143C") + 
  theme_bw() + 
  theme(legend.position = "none", # Hide legend for this plot
        plot.title = element_blank()) +  # Remove individual titles
  ylim(0, 25000) +                                      
  labs(x = "Time Since the Latest Immunology (month)", y = "S Antibody Level")

# Combine the four plots, with "Hybrid-induced" on the left and "Vaccine-induced" on the right, titles at the top
combined_plot <- grid.arrange(
  arrangeGrob(plot3, plot1, ncol = 1, top = textGrob("Hybrid-induced", gp = gpar(fontsize = 18, fontface = "bold"), hjust = 0.5)),
  arrangeGrob(plot4, plot2, ncol = 1, top = textGrob("Vaccine-induced", gp = gpar(fontsize = 18, fontface = "bold"), hjust = 0.5)),
  ncol = 2
)

ggsave("Results/plots_updated/fig2.pdf", plot = combined_plot, width = 10, height = 8)
