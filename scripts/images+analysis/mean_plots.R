library(tidyverse)
library(raster)
library(rgdal)
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
              "2020-09-19")


### LOAD DATA 
path_data = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/" # Path data NICOLAS-PC
#path_combined_data = paste(path_data, "Processed/export/combined_data/combined_", date_SSEB, ".csv", sep = "")
path_LU = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/Processed/study_area/LU_raster_combined.csv"



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
  df_name <- date_SSEB
  assign(df_name, read.csv(path_combined_data))
  
  # Store the data frame in the list
  data_frames_list[[df_name]] <- get(df_name)
}
 

SEBAL_ts = data.frame(x = `2015-04-15`$x, y = `2015-04-15`$y)
SSEB_ts = data.frame(x = `2015-04-15`$x, y = `2015-04-15`$y)
METRIC_ts = data.frame(x = `2015-04-15`$x, y = `2015-04-15`$y)
WASIM_ts = data.frame(x = `2015-04-15`$x, y = `2015-04-15`$y)


for (i in data_frames_list) {
  # Append columns to respective data frames
  SEBAL_ts <- cbind(SEBAL_ts, i$SEBAL)
  SSEB_ts <- cbind(SSEB_ts, i$SSEB)
  METRIC_ts <- cbind(METRIC_ts, i$METRIC)
  WASIM_ts <- cbind(WASIM_ts, i$WASIM)
}

columns = c("x", "y", 
            "2015-04-15",
            "2015-07-04",
              "2016-09-08",
              "2016-09-24",
              "2017-04-20",
              "2018-04-07",
              "2018-09-30",
              "2019-06-29",
              "2020-09-19")



colnames(SEBAL_ts) <- columns
colnames(SSEB_ts) <- columns
colnames(METRIC_ts) <- columns
colnames(WASIM_ts) <- columns

SEBAL_ts[,3:11]

means <- colMeans(SEBAL_ts[,3:11], na.rm = TRUE)
means
time_series <- ts(means, start = c(2015, 4), frequency = 12)
plot(time_series, type = "l", xlab = "Time", ylab = "Mean Value",
    main = "Time Series of Column Means")
