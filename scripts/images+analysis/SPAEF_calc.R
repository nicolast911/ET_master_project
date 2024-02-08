### SPAEF Calculation
# Demirel, M.C., Koch, J., Stisen, S., 2018. SPAEF: SPAtial EFficiency. GitHub. https://doi.org/10.5281/ZENODO.1158890
# Function taken from "SPAEF_dempv6.R" / https://github.com/cuneyd/spaef


library(tidyverse)
library(raster)
library(lubridate)
library(viridis)
library(sf)
library(scico)
library(gridExtra)

# for SPAEF
library(pracma)


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

date_list_SSEB = c("2015-04-15",
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


######################################################
######################################################
# SPAEF Calculation
# Function 
spaef2 <- function(dataframe, obs_col, sim_col) {
  
  #### DATA PREPARATION
  data <-  dataframe
  
  # Only use overlapping pixels
  Subset <- subset(data, (!is.na(data[,obs_col])) & (!is.na(data[,sim_col])))
  obs = Subset[,obs_col]
  sim = Subs1[,sim_col]
  
  #### CALCULATION
  # 1. Correlation Alpha
  alpha=cor(obs, sim)
  
  # 2. Coefficient of variation Beta/CV
  cv_obs=sd(obs, na.rm = TRUE)/mean(obs, na.rm = TRUE);
  cv_sim=sd(sim, na.rm = TRUE)/mean(sim, na.rm = TRUE);
  beta=cv_sim/cv_obs;
  
  # 3. HISTOmatch Gamma
  obs=(obs-mean(obs, na.rm = TRUE))/sd(obs, na.rm = TRUE)
  sim=(sim-mean(sim, na.rm = TRUE))/sd(sim, na.rm = TRUE)
  
  bins=floor(sqrt(length(obs)))
  
  h1 <- hist(obs, breaks=bins, freq=TRUE, plot=TRUE)
  h2 <- hist(sim, breaks=bins, freq=TRUE, plot=TRUE) #False makes Density instead of frequency, try it
  
  a=histc(obs, h1$breaks)
  b=histc(sim, h1$breaks)
  c=cbind(a$cnt, b$cnt)
  d <- pmin(c[,1],c[,2])
  overlap=sum(d)
  histogram_match=overlap/sum(a$cnt)
  gamma=histogram_match
  
  # 4. Resulting SPAEF
  spaef = 1- sqrt( (alpha-1)^2 + (beta-1)^2 + (gamma-1)^2 )
  
  #print(paste0("SPAEF: ", round(spaef, digits = 6)))
  # cat("SPAEF: ", spaef, "alpha: ", alpha, "beta: ", beta, "gamma: ", gamma)
  return(c(spaef, alpha, beta, gamma))
}



#######
# Function 2 Test for two dataframes
spaef <- function(obs_df, sim_df, date_col) {
  
  #### DATA PREPARATION
  # Only use overlapping pixels
  obs_df_sub <- subset(obs_df, (!is.na(obs_df[,date_col])) & (!is.na(sim_df[,date_col])))
  sim_df_sub <- subset(sim_df, (!is.na(sim_df[,date_col])) & (!is.na(obs_df[,date_col])))
  
  obs = obs_df_sub[,date_col]
  sim = sim_df_sub[,date_col]
  
  #### CALCULATION
  # 1. Correlation Alpha
  alpha=cor(obs, sim)
  
  # 2. Coefficient of variation Beta/CV
  cv_obs=sd(obs, na.rm = TRUE)/mean(obs, na.rm = TRUE);
  cv_sim=sd(sim, na.rm = TRUE)/mean(sim, na.rm = TRUE);
  beta=cv_sim/cv_obs;
  
  # 3. HISTOmatch Gamma
  obs=(obs-mean(obs, na.rm = TRUE))/sd(obs, na.rm = TRUE)
  sim=(sim-mean(sim, na.rm = TRUE))/sd(sim, na.rm = TRUE)
  
  bins=floor(sqrt(length(obs)))
  
  h1 <- hist(obs, breaks=bins, freq=TRUE, plot=TRUE)
  h2 <- hist(sim, breaks=bins, freq=TRUE, plot=TRUE) #False makes Density instead of frequency, try it
  
  a=histc(obs, h1$breaks)
  b=histc(sim, h1$breaks)
  c=cbind(a$cnt, b$cnt)
  d <- pmin(c[,1],c[,2])
  overlap=sum(d)
  histogram_match=overlap/sum(a$cnt)
  gamma=histogram_match
  
  # 4. Resulting SPAEF
  spaef = 1- sqrt( (alpha-1)^2 + (beta-1)^2 + (gamma-1)^2 )
  
  #print(paste0("SPAEF: ", round(spaef, digits = 6)))
  # cat("SPAEF: ", spaef, "alpha: ", alpha, "beta: ", beta, "gamma: ", gamma)
  result <- c(spaef, alpha, beta, gamma)
  return(result)
}

######################################
######################################
## In for-loop


spaef_df <- function(obs_df, sim_df) {
  # Create emtpy df
  df <- data.frame(Date = as.Date(date_list), 
                       SPAEF = numeric(10), 
                       alpha = numeric(10), 
                       beta = numeric(10), 
                       gamma = numeric(10))
  
  # Run SPAEF 
  for (i in 1:10) {
    date_col <- i + 3
    
    result <- spaef(obs_df, sim_df, date_col)
    
    df[i,]$SPAEF <- result[1]
    df[i,]$alpha <- result[2]
    df[i,]$beta <- result[3]
    df[i,]$gamma <- result[4]
  }
  
  
  return(df)
}

###

spaef_df_SSEB <- function(obs_df, sim_df) {
  # Create emtpy df
  df <- data.frame(Date = as.Date(date_list), 
                   SPAEF = numeric(10), 
                   alpha = numeric(10), 
                   beta = numeric(10), 
                   gamma = numeric(10))
  
  # Run SPAEF 
  for (i in 1:9) {
    if (i <= 8) {
      date_col <- i + 3
    } 
    else {
      date_col <- i+4
    }
    
    result <- spaef2(obs_df, sim_df, date_col)
    
    df[date_col-3,]$SPAEF <- result[1]
    df[date_col-3,]$alpha <- result[2]
    df[date_col-3,]$beta <- result[3]
    df[date_col-3,]$gamma <- result[4]
    
  }

    return(df)
}



df <- spaef_df_SSEB(SEBAL_ts, SSEB_ts)
df


spaef2(SEBAL_ts, SSEB_ts, 13)
