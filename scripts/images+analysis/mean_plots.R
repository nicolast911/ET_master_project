library(tidyverse)
library(raster)
library(lubridate)
library(viridis)
library(sf)
library(scico)
library(gridExtra)


### DATES 

date_list = c("2015-04-15",
              "2015-07-04",
              "2016-09-08",
              "2016-09-24",
              "2017-04-20",
              "2018-04-07",
              "2018-09-30",
              "2019-06-29",
              "2020-03-27",
              "2020-09-19")


### LOAD DATA 
path_data = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/" # Path data NICOLAS-PC
#path_combined_data = paste(path_data, "Processed/export/combined_data/combined_", date_SSEB, ".csv", sep = "")
path_LU = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/Processed/study_area/LU_raster_combined.csv"
LU = read.csv(path_LU)

path_plots = paste(path_data, "Processed/export/R_plots/General", sep = "")
path_tables = paste(path_data, "Processed/export/R_tables", sep = "")


data_frames_list <- list()


for (date_str in date_list) {
  # Convert date string to Date object
  date <- as.Date(date_str, format = "%Y-%m-%d")
  
  # Convert date to the desired formats
  date_SSEB <- format(date, "%Y_%m_%d") %>%
    str_replace_all("-", "_")
  
  # Create file path
  path_combined_data <- paste(path_data, "Processed/export/combined_data/combined_", date_SSEB, ".csv", sep = "")
  
  # Read CSV file into a data frame
  df_name <- date_str
  assign(df_name, read.csv(path_combined_data))
  
  # Store the data frame in the list
  data_frames_list[[df_name]] <- get(df_name)
}
 
# Add a NA-column to data frame of 2020-03-27 to make it compatible with the others
data_frames_list$`2020-03-27` <-  cbind(data_frames_list$`2020-03-27`, c(NA))
colnames(data_frames_list$`2020-03-27`) <-  c("x", "y", "SEBAL", "METRIC", "WASIM", "diff_METRIC", "diff_WASIM", "SSEB")




### CREATE TIME SERIES DataFrames

SEBAL_ts = data.frame(x = `2015-04-15`$x, y = `2015-04-15`$y, LandUse = LU$values)
SSEB_ts = data.frame(x = `2015-04-15`$x, y = `2015-04-15`$y, LandUse = LU$values)
METRIC_ts = data.frame(x = `2015-04-15`$x, y = `2015-04-15`$y, LandUse = LU$values)
WASIM_ts = data.frame(x = `2015-04-15`$x, y = `2015-04-15`$y, LandUse = LU$values)


for (i in data_frames_list) {
  # Append columns to respective data frames
  SEBAL_ts <- cbind(SEBAL_ts, i$SEBAL)
  SSEB_ts <- cbind(SSEB_ts, i$SSEB)
  METRIC_ts <- cbind(METRIC_ts, i$METRIC)
  WASIM_ts <- cbind(WASIM_ts, i$WASIM)
}



columns = c("x", "y", "LandUse",
            "2015-04-15",
            "2015-07-04",
              "2016-09-08",
              "2016-09-24",
              "2017-04-20",
              "2018-04-07",
              "2018-09-30",
              "2019-06-29",
              "2020-03-27",
              "2020-09-19")



colnames(SEBAL_ts) <- columns
colnames(SSEB_ts) <- columns
colnames(METRIC_ts) <- columns
colnames(WASIM_ts) <- columns



### CALCULATE MEANS and SD All Land Use
means_SEBAL <- apply(SEBAL_ts[, 4:13], 2, mean, na.rm = TRUE)
means_SSEB <- apply(SSEB_ts[, 4:13], 2, mean, na.rm = TRUE)
means_METRIC <- apply(METRIC_ts[, 4:13], 2, mean, na.rm = TRUE)
means_WASIM <- apply(WASIM_ts[, 4:13], 2, mean, na.rm = TRUE)

sds_SEBAL <- apply(SEBAL_ts[, 4:13], 2, sd, na.rm = TRUE)
sds_SSEB <- apply(SSEB_ts[, 4:13], 2, sd, na.rm = TRUE)
sds_METRIC <- apply(METRIC_ts[, 4:13], 2, sd, na.rm = TRUE)
sds_WASIM <- apply(WASIM_ts[, 4:13], 2, sd, na.rm = TRUE)


means_df = data.frame("date" = date_list)
means_df = cbind(means_df, means_SEBAL)
means_df = cbind(means_df, means_SSEB)
means_df = cbind(means_df, means_METRIC)
means_df = cbind(means_df, means_WASIM)


sd_df = data.frame("date" = date_list)
sd_df = cbind(sd_df, sds_SEBAL)
sd_df = cbind(sd_df, sds_SSEB)
sd_df = cbind(sd_df, sds_METRIC)
sd_df = cbind(sd_df, sds_WASIM)

# Coefficient of Variation Table 
cv_df <- means_df
cv_df[,2:5] <- (sd_df[,2:5] / means_df[,2:5])*100



pivot_means_df = pivot_longer(means_df, cols = 2:5)
pivot_sd_df = pivot_longer(sd_df, cols = 2:5)
pivot_cv_df <- pivot_longer(cv_df, cols = 2:5)

pivot_df = cbind(pivot_means_df, pivot_sd_df$value, pivot_cv_df$value)
colnames(pivot_df) = c("date", "name", "mean", "sd", "CV")

pivot_df = pivot_df %>% 
        mutate(name = str_replace(name, "means_SEBAL", "SEBAL")) %>%
        mutate(name = str_replace(name, "means_SSEB", "SSEB")) %>%
        mutate(name = str_replace(name, "means_METRIC", "METRIC")) %>%
        mutate(name = str_replace(name, "means_WASIM", "WASIM")) 
  
pivot_df$name <- factor(pivot_df$name, 
                   levels = c("SEBAL", "SSEB", "METRIC", "WASIM"))


# Export to csv
write.csv(means_df, file = paste(path_tables, "/means_df.csv", sep = ""))
write.csv(sd_df, file = paste(path_tables, "/sd_df.csv", sep = ""))
write.csv(cv_df, file = paste(path_tables, "/cv_df.csv", sep = ""))

##########################################################################
###### PLOT MEANS 
##########################################################################


# All in one Plot, Points
shape_mapping <- c("SEBAL" = 16, "SSEB" = 17, "METRIC" = 18, "WASIM" = 15)
#model_color <- c("SEBAL" = "#D55E00", "SSEB" = "#009f60", "METRIC" = "#E69F00", "WASIM" = "#0072B2")
# More s/w readability/contrast
model_color <- c("SEBAL" = "#841157", "SSEB" = "#009f60", "METRIC" = "#E69F00", "WASIM" = "#0072B2")


mean_points <- ggplot(pivot_df, aes(x = date, y = mean, color = name, shape = name, group = name)) +
  geom_point(size = 5) +
  geom_line() + 
  labs(title = "ETa Mean per Date",
       x = "Date",
       y = "Mean ETa (mm)",
       color = "Model",
       shape = "Model") +
  scale_shape_manual(values = shape_mapping) +  # Set shapes manually
  scale_color_manual(values = model_color) +  # Set colors manually
  theme_bw() +
  theme(axis.title.x = element_text(size = 12.5),  # Adjust the size of x-axis title
        axis.title.y = element_text(size = 12.5),
        plot.title = element_text(size = 14.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12.5)) # Adjust the size of y-axis title

mean_points

ggsave(mean_points, filename = paste(path_plots, "/mean_points.png", sep = ""),
       width = 3212, height = 1942, units = "px")




### BarPlot With Errorbar
mean_bars <- ggplot(pivot_df, aes(x=date, y=mean, fill=name)) + 
  geom_col(position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.4,
                position=position_dodge(0.9)) +
  labs(title = "ETa Mean and SD per Date",
       x = "Date",
       y = "Mean ETa (mm)",
       fill = "Model") +
  scale_shape_manual(values = shape_mapping) +  # Set shapes manually
  scale_fill_manual(values = model_color) +  # Set colors manually
  theme_bw() +
  theme(axis.title.x = element_text(size = 12.5),  # Adjust the size of x-axis title
        axis.title.y = element_text(size = 12.5),
        plot.title = element_text(size = 14.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12.5)) # Adjust the size of y-axis title

mean_bars

ggsave(mean_bars, filename = paste(path_plots, "/mean_bars.png", sep = ""),
       width = 3212, height = 1942, units = "px")





### Coefficient of Variations PLot
cv_points <- ggplot(pivot_df, aes(x = date, y = CV, color = name, shape = name, group = name)) +
  geom_point(size = 5) +
  geom_line() + 
  labs(title = "ETa Coefficient of Variation",
       x = "Date",
       y = "Coefficient of Variation (%)",
       color = "Model",
       shape = "Model") +
  scale_shape_manual(values = shape_mapping) +  # Set shapes manually
  scale_color_manual(values = model_color) +  # Set colors manually
  theme_bw() +
  theme(axis.title.x = element_text(size = 12.5),  # Adjust the size of x-axis title
        axis.title.y = element_text(size = 12.5),
        plot.title = element_text(size = 14.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12.5)) # Adjust the size of y-axis title

cv_points

ggsave(cv_points, filename = paste(path_plots, "/cv_points.png", sep = ""),
       width = 3212, height = 1942, units = "px")






####################################################
####################################################
# Four Subplots Lines
# ggplot(pivot_df, aes(x = date, y = mean, color = name, group = name)) +
#   geom_point(size = 3) +
#   geom_line() + 
#   geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2,
#                 position = position_dodge(0.05)) +
#   labs(title = "Line Plot of Values Over Time",
#        x = "Date",
#        y = "Value",
#        color = "Name") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_wrap(~factor(name, c("SEBAL", "SSEB", "METRIC", "WASIM")), scales = "free_y", nrow = 4) +
#   scale_y_continuous(limits = c(0, 8))
# 
# 
# # Four Subplots Bars 
# ggplot(pivot_df, aes(x = date, y = mean, fill = name, group = name)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7) +
#   geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(0.7), width = 0.25) +
#   labs(title = "Bar Plot with Error Bars of Values Over Time",
#        x = "Date",
#        y = "Value",
#        fill = "Name") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_wrap(~factor(name, c("SEBAL", "SSEB", "METRIC", "WASIM")), scales = "free_y", nrow = 4) +
#   scale_y_continuous(limits = c(0, 8))








###############
###############
### CALCULATE MEANS and SD PER LAND USE
LUm_SEBAL <- aggregate(SEBAL_ts[, 4:13], list(SEBAL_ts$LandUse), mean, na.rm=TRUE) %>%
  pivot_longer(., cols = 2:11) %>%
  cbind(., "model" = c("SEBAL"))
LUm_SSEB <- aggregate(SSEB_ts[, 4:13], list(SSEB_ts$LandUse), mean, na.rm=TRUE) %>%
  pivot_longer(., cols = 2:11) %>%
  cbind(., "model" = c("SSEB"))
LUm_METRIC <- aggregate(METRIC_ts[, 4:13], list(METRIC_ts$LandUse), mean, na.rm=TRUE) %>%
  pivot_longer(., cols = 2:11) %>%
  cbind(., "model" = c("METRIC"))
LUm_WASIM <- aggregate(WASIM_ts[, 4:13], list(WASIM_ts$LandUse), mean, na.rm=TRUE) %>%
  pivot_longer(., cols = 2:11) %>%
  cbind(., "model" = c("WASIM"))


LUsd_SEBAL <- aggregate(SEBAL_ts[, 4:13], list(SEBAL_ts$LandUse), sd, na.rm=TRUE) %>%
  pivot_longer(., cols = 2:11) %>%
  cbind(., "model" = c("SEBAL"))
LUsd_SSEB <- aggregate(SSEB_ts[, 4:13], list(SSEB_ts$LandUse), sd, na.rm=TRUE) %>%
  pivot_longer(., cols = 2:11) %>%
  cbind(., "model" = c("SSEB"))
LUsd_METRIC <- aggregate(METRIC_ts[, 4:13], list(METRIC_ts$LandUse), sd, na.rm=TRUE) %>%
  pivot_longer(., cols = 2:11) %>%
  cbind(., "model" = c("METRIC"))
LUsd_WASIM <- aggregate(WASIM_ts[, 4:13], list(WASIM_ts$LandUse), sd, na.rm=TRUE) %>%
  pivot_longer(., cols = 2:11) %>%
  cbind(., "model" = c("WASIM"))

LU_mean <- c("LUm_SEBAL", "LUm_SSEB", "LUm_METRIC", "LUm_WASIM")
LU_sd <- c("LUsd_SEBAL", "LUsd_SSEB", "LUsd_METRIC", "LUsd_WASIM")

for(df in LU_mean) {
  df.tmp <- get(df)
  names(df.tmp) <- c("LandUse", "date", "mean", "model")
  assign(df, df.tmp)
}

for(df in LU_sd) {
  df.tmp <- get(df)
  names(df.tmp) <- c("LandUse", "date", "sd", "model")
  assign(df, df.tmp)
}







LU_mean_combined <- bind_rows(LUm_SEBAL, LUm_SSEB, LUm_METRIC, LUm_WASIM)
LU_sd_combined <- bind_rows(LUsd_SEBAL, LUsd_SSEB, LUsd_METRIC, LUsd_WASIM)


## Add Strings of Land use Class
string_vector <- c("Agriculture", "Bare Soil", "Grassland", "Urban Area", "Forest")
LU_mean_combined <- LU_mean_combined %>%
  mutate(LandUse_name = case_when(
    LandUse == 1 ~ string_vector[1],
    LandUse == 2 ~ string_vector[2],
    LandUse == 3 ~ string_vector[3],
    LandUse == 4 ~ string_vector[4],
    LandUse == 5 ~ string_vector[5]
  ))



## Export as .csv
write.csv(LU_mean_combined, file = paste(path_tables, "/means_LU_df.csv", sep = ""))
write.csv(LU_sd_combined, file = paste(path_tables, "/sd_LU_df.csv", sep = ""))


LU_pivot_df = cbind(LU_mean_combined, LU_sd_combined$sd)
colnames(LU_pivot_df) = c("LandUse", "date", "mean", "model", "LandUse_name", "sd")
LU_pivot_df = LU_pivot_df[,c(2, 4, 1, 5, 3, 6)]


# Does not work probably
LU_pivot_df$model <- factor(LU_pivot_df$model, 
                        levels = c("SEBAL", "SSEB", "METRIC", "WASIM"))

# does not work --> revise 
LU_pivot_df$LandUse_name <- factor(LU_pivot_df$LandUse_name, 
                        levels = c("Agriculture", "Bare Soil", "Grassland", "Urban Area", "Forest"))



##############
## Find number of observations n
n_SEBAL <- SEBAL_ts %>%
  group_by(LandUse) %>%
  summarise_all(~ sum(!is.na(.))) %>%
  as.data.frame()


n_SSEB <- SSEB_ts %>%
  group_by(LandUse) %>%
  summarise_all(~ sum(!is.na(.))) %>%
  as.data.frame()


n_METRIC <- METRIC_ts %>%
  group_by(LandUse) %>%
  summarise_all(~ sum(!is.na(.))) %>%
  as.data.frame()


n_WASIM <- WASIM_ts %>%
  group_by(LandUse) %>%
  summarise_all(~ sum(!is.na(.))) %>%
  as.data.frame()






########
# Add Land Use names and n for each factor
LU_names <- c("Agriculture  (n ≈ 261.000)", "Bare Soil  (n ≈ 600)", "Grassland  (n ≈ 172.000)", "Urban Area  (n ≈ 34.000)", "Forest  (n ≈ 280.000)")

### PLOT MEANS per Land Use

# All in one Plot, Points

# Factor 1 - Agri
LU_pivot_df %>%
  filter(LandUse == 1) %>%
  ggplot(aes(x = date, y = mean, color = model)) +
    geom_point(size = 3) +
    geom_line() + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05)) +
    labs(title = "Line Plot of Values Over Time",
       x = "Date",
       y = "Value",
       color = "Name") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



# All Factors - 5 Subplots - REVISION
mean_LU <- LU_pivot_df %>%
  filter(LandUse %in% 1:5) %>%
  ggplot(aes(x = date, y = mean, fill = model, group = model)) +
  geom_col(position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.3,
                position=position_dodge(0.9)) +
  labs(title = "ETa Mean per Land Use",
       x = "Date",
       y = "Mean ETa (mm)",
       fill = "Model") +
  theme_bw() +
  scale_fill_manual(values = model_color) +  # Set colors manually
  theme(axis.title.x = element_text(size = 12.5),  # Adjust the size of x-axis title
        axis.title.y = element_text(size = 12.5),
        plot.title = element_text(size = 14.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12.5),
        strip.text = element_text(size = 10)) + # Adjust the size of y-axis title
  facet_wrap(~LandUse, scales = "free_y", ncol = 1, labeller = labeller(LandUse = setNames(LU_names, 1:5)))
  
mean_LU


ggsave(mean_LU, filename = paste(path_plots, "/mean_LU.png", sep = ""),
       width = 3200, height = 2500, units = "px")


### END 


##############################################################################################
##############################################################################################
## Next Take:
# PLot for each Model a Land Use differentiated Mean Plot

shape_mapping <- c("Agriculture" = 16, "Bare Soil" = 20, "Grassland" = 18, "Urban Area" = 15, "Forest" = 17)

# Define custom colors for LandUse_name factors
#land_use_colors <- c("Agriculture" =  "#F0E442", "Bare Soil" = "#616161", "Grassland" = "lightgreen", "Urban Area" ="#DC697D", "Forest" =  "#337538")
# Viridis Adaption
land_use_colors <- c("Agriculture" =  "#FDE725FF", "Bare Soil" = "#616161", "Grassland" = "#7AD151FF", "Urban Area" ="#DC697D", "Forest" =  "#337538")



SEBAL_LU <- LU_pivot_df %>%
  filter(model == "SEBAL") %>%  
  ggplot(aes(x = date, y = mean, color = LandUse_name, shape = LandUse_name, group = LandUse_name)) +
  geom_point(size = 7) +
  geom_line() + 
  labs(title = "SEBAL Mean ETa per Land Use",
       x = "Date",
       y = "Mean ETa (mm)",
       color = "Land Use",
       shape = "Land Use") +  
  theme_bw() +
  scale_shape_manual(values = shape_mapping) +  # Set shapes manually
  scale_color_manual(values = land_use_colors) +  # Set custom colors for LandUse_name
  theme(axis.title.x = element_text(size = 12.5),
        axis.title.y = element_text(size = 12.5),
        plot.title = element_text(size = 14.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12.5))
SEBAL_LU

ggsave(SEBAL_LU, filename = paste(path_plots, "/SEBAL_LU.png", sep = ""),
       width = 3212, height = 1942, units = "px")


######

SSEB_LU <- LU_pivot_df %>%
  filter(model == "SSEB") %>%  
  ggplot(aes(x = date, y = mean, color = LandUse_name, shape = LandUse_name, group = LandUse_name)) +
  geom_point(size = 7) +
  geom_line() + 
  labs(title = "SSEB Mean ETa per Land Use",
       x = "Date",
       y = "Mean ETa (mm)",
       color = "Land Use",
       shape = "Land Use") +  
  theme_bw() +
  scale_shape_manual(values = shape_mapping) +  # Set shapes manually
  scale_color_manual(values = land_use_colors) +  # Set custom colors for LandUse_name
  theme(axis.title.x = element_text(size = 12.5),
        axis.title.y = element_text(size = 12.5),
        plot.title = element_text(size = 14.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12.5))


ggsave(SSEB_LU, filename = paste(path_plots, "/SSEB_LU.png", sep = ""),
       width = 3212, height = 1942, units = "px")

######


METRIC_LU <- LU_pivot_df %>%
  filter(model == "METRIC") %>%  
  ggplot(aes(x = date, y = mean, color = LandUse_name, shape = LandUse_name, group = LandUse_name)) +
  geom_point(size = 7) +
  geom_line() + 
  labs(title = "METRIC Mean ETa per Land Use",
       x = "Date",
       y = "Mean ETa (mm)",
       color = "Land Use",
       shape = "Land Use") +  
  theme_bw() +
  scale_shape_manual(values = shape_mapping) +  # Set shapes manually
  scale_color_manual(values = land_use_colors) +  # Set custom colors for LandUse_name
  theme(axis.title.x = element_text(size = 12.5),
        axis.title.y = element_text(size = 12.5),
        plot.title = element_text(size = 14.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12.5))


ggsave(METRIC_LU, filename = paste(path_plots, "/METRIC_LU.png", sep = ""),
       width = 3212, height = 1942, units = "px")


#######


WASIM_LU <- LU_pivot_df %>%
  filter(model == "WASIM") %>%  
  ggplot(aes(x = date, y = mean, color = LandUse_name, shape = LandUse_name, group = LandUse_name)) +
  geom_point(size = 7) +
  geom_line() + 
  labs(title = "WASIM Mean ETa per Land Use",
       x = "Date",
       y = "Mean ETa (mm)",
       color = "Land Use",
       shape = "Land Use") +  
  theme_bw() +
  scale_shape_manual(values = shape_mapping) +  # Set shapes manually
  scale_color_manual(values = land_use_colors) +  # Set custom colors for LandUse_name
  theme(axis.title.x = element_text(size = 12.5),
        axis.title.y = element_text(size = 12.5),
        plot.title = element_text(size = 14.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12.5))


ggsave(WASIM_LU, filename = paste(path_plots, "/WASIM_LU.png", sep = ""),
       width = 3212, height = 1942, units = "px")


