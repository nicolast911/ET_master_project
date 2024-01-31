library(tidyverse)
library(raster)
library(lubridate)
library(viridis)
library(sf)
library(scico)
library(gridExtra)


### DATES without 2020-03

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
View(LU)

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



pivot_means_df = pivot_longer(means_df, cols = 2:5)
pivot_sd_df = pivot_longer(sd_df, cols = 2:5)
pivot_df = cbind(pivot_means_df, pivot_sd_df$value)
colnames(pivot_df) = c("date", "name", "mean", "sd")

pivot_df = pivot_df %>% 
        mutate(name = str_replace(name, "means_SEBAL", "SEBAL")) %>%
        mutate(name = str_replace(name, "means_SSEB", "SSEB")) %>%
        mutate(name = str_replace(name, "means_METRIC", "METRIC")) %>%
        mutate(name = str_replace(name, "means_WASIM", "WASIM")) 
  


### PLOT MEANS 

# All in one Plot, Points
ggplot(pivot_df, aes(x = date, y = mean, color = name, group = name)) +
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




# All in one Plot, Bars
ggplot(pivot_df, aes(x=date, y=mean, fill=name)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) 




# Four Subplots Lines
ggplot(pivot_df, aes(x = date, y = mean, color = name, group = name)) +
  geom_point(size = 3) +
  geom_line() + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2,
                position = position_dodge(0.05)) +
  labs(title = "Line Plot of Values Over Time",
       x = "Date",
       y = "Value",
       color = "Name") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~factor(name, c("SEBAL", "SSEB", "METRIC", "WASIM")), scales = "free_y", nrow = 4) +
  scale_y_continuous(limits = c(0, 8))


# Four Subplots Bars 
ggplot(pivot_df, aes(x = date, y = mean, fill = name, group = name)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(0.7), width = 0.25) +
  labs(title = "Bar Plot with Error Bars of Values Over Time",
       x = "Date",
       y = "Value",
       fill = "Name") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~factor(name, c("SEBAL", "SSEB", "METRIC", "WASIM")), scales = "free_y", nrow = 4) +
  scale_y_continuous(limits = c(0, 8))








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

LU_pivot_df = cbind(LU_mean_combined, LU_sd_combined$sd)
colnames(LU_pivot_df) = c("LandUse", "date", "mean", "model", "sd")
LU_pivot_df = LU_pivot_df[,c(2, 4, 1, 3, 5)]



# Add Land Use names for eacht factor
LU_names <- c("Agriculture", "Bare Soil", "Grassland", "Urban Area", "Forest")

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



# All Factors - 5 Subplots
LU_pivot_df %>%
  filter(LandUse %in% 1:5) %>%
  ggplot(aes(x = date, y = mean, color = model, group = model)) +
  geom_point(size = 3) +
  geom_line() + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05)) +
  labs(title = "Line Plot of Values Over Time",
       x = "Date",
       y = "Value",
       color = "Name") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~LandUse, scales = "free_y", ncol = 1, labeller = labeller(LandUse = setNames(LU_names, 1:5)))
