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
library("scatterplot3d") 

# for SPAEF
library(pracma)


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

#path_combined_data = paste(path_data, "Processed/export/combined_data/combined_", date_SSEB, ".csv", sep = "")

path_data = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/" # Path data NICOLAS-PC
path_tables = paste(path_data, "Processed/export/R_tables/", sep = "")


data_frames_list = c("SEBAL_ts.csv", 
                     "SSEB_ts.csv", 
                     "METRIC_ts.csv", 
                     "WASIM_ts.csv")

ts_names = c("SEBAL_ts", 
            "SSEB_ts", 
            "METRIC_ts", 
            "WASIM_ts")

result_list <- list()

# Check if the combined CSV file already exists, else source the code to create timeseries dataframes
for (i in 1:4) {
  
  csv <- data_frames_list[i]
  ts_name <- ts_names[i]
  # Path to CSV
  table <- paste(path_tables, csv, sep = "")
  
  if (file.exists(table)) {
    df <- read.csv(table)
  } else {
    source("create_timeseries.R")
    df <- read.csv(table)
  }
  
  result_list[[ts_name]] <- df
  
}


SEBAL_ts <- result_list[["SEBAL_ts"]]
SSEB_ts <- result_list[["SSEB_ts"]]
METRIC_ts <- result_list[["METRIC_ts"]]
WASIM_ts <- result_list[["WASIM_ts"]]
rm(c(df, result_list))

######################################################
######################################################
# SPAEF Calculation
# Function 



#######
# Function for two data frames 
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


### Function 2 whith dataframe input 
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
    
    if (date_col <= 11) {
      result_col <- i
    } else {
      result_col <- i+1
    }
    
    result <- spaef(obs_df, sim_df, date_col)
    
    df[result_col,]$SPAEF <- result[1]
    df[result_col,]$alpha <- result[2]
    df[result_col,]$beta <- result[3]
    df[result_col,]$gamma <- result[4]
    
  }

    return(df)
}


#########################################
#########################################
###  Create SPAEF Data Frames 
SEBAL_SSEB <- spaef_df_SSEB(SEBAL_ts, SSEB_ts)
SEBAL_METRIC <- spaef_df(SEBAL_ts, METRIC_ts)
SEBAL_WASIM <- spaef_df(SEBAL_ts, WASIM_ts)

SSEB_METRIC <- spaef_df_SSEB(SSEB_ts, METRIC_ts)
SSEB_WASIM <- spaef_df_SSEB(SSEB_ts, WASIM_ts)
METRIC_WASIM <- spaef_df(METRIC_ts, WASIM_ts)


SEBAL_SSEB["Models"] <- "SEBAL_SSEB"
SEBAL_METRIC["Models"] <- "SEBAL_METRIC"
SEBAL_WASIM["Models"] <- "SEBAL_WASIM"

SSEB_METRIC["Models"] <- "SSEB_METRIC"
SSEB_WASIM["Models"] <- "SSEB_WASIM"
METRIC_WASIM["Models"] <- "METRIC_WASIM"




#### SAVE Spaed dfs
path_spaef = paste(path_data, "Processed/export/R_tables/SPAEF", sep = "")

write.csv(SEBAL_SSEB, file = paste(path_spaef, "/SEBAL_SSEB.csv", sep = ""))
write.csv(SEBAL_METRIC, file = paste(path_spaef, "/SEBAL_METRIC.csv", sep = ""))
write.csv(SEBAL_WASIM, file = paste(path_spaef, "/SEBAL_WASIM.csv", sep = ""))

write.csv(SSEB_METRIC, file = paste(path_spaef, "/SSEB_METRIC.csv", sep = ""))
write.csv(SSEB_WASIM, file = paste(path_spaef, "/SSEB_WASIM.csv", sep = ""))
write.csv(METRIC_WASIM, file = paste(path_spaef, "/METRIC_WASIM.csv", sep = ""))

########################################
########################################
### Create SPAEF Plots

spaef_list <- list(SEBAL_SSEB,
                   SEBAL_METRIC,
                   SEBAL_WASIM,
                   SSEB_METRIC,
                   SSEB_WASIM,
                   METRIC_WASIM)

spaef_pivot <- pivot_longer(spaef_list[[1]], cols = 2:5)
for (i in 2:6) {
  spaef_pivot <- rbind(spaef_pivot, pivot_longer(spaef_list[[i]], cols = 2:5))
}
# Turn Date into factor
spaef_pivot$Date <- factor(spaef_pivot$Date)
spaef_pivot$name <- factor(spaef_pivot$name)
spaef_pivot$Models <- factor(spaef_pivot$Models, 
                        levels = c("SEBAL_SSEB",
                                   "SEBAL_METRIC",
                                   "SEBAL_WASIM",
                                   "SSEB_METRIC",
                                   "SSEB_WASIM",
                                   "METRIC_WASIM"))

# Turn values from 2020-03 with SSEB to NA
spaef_pivot <- spaef_pivot %>%
  mutate(value = if_else((Models %in% c("SEBAL_SSEB", "SSEB_METRIC", "SSEB_WASIM") & Date %in% "2020-03-27"),NA,value))

spaef_pivot %>%
  filter(Models %in% c("SEBAL_SSEB", "SSEB_METRIC", "SSEB_WASIM") & Date %in% "2020-03-27")


## Create Average column
spaef_mean <- spaef_pivot %>%
  filter(name %in% "SPAEF") %>%
  group_by(Models) %>%
  dplyr::summarize(SPAEF = mean(value, na.rm = TRUE), Date = "Mean") %>%
  pivot_longer(col = 2)


spaef_pivot_mean <- rbind(spaef_pivot, spaef_mean)
spaef_pivot <- spaef_pivot_mean

### PLOTS


spaef_pivot %>%
  filter(name == "SPAEF") %>%
  ggplot(aes(x = Date, y = value, color = Models)) +
  geom_point()

spaef_heatmap <- spaef_pivot %>% 
  filter(name %in% "SPAEF") %>%
  mutate(Models = factor(Models, levels = rev(c("SEBAL_SSEB",
                                                   "SEBAL_METRIC",
                                                   "SEBAL_WASIM",
                                                   "SSEB_METRIC",
                                                   "SSEB_WASIM",
                                                   "METRIC_WASIM")))) %>%
  ggplot(aes(x = Date, y = Models, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2), color = abs(value) > 0.4)) +
  scale_color_manual(guide = FALSE, values = c("black", "white")) +
  scale_fill_scico(palette = "vik", midpoint = 0, direction = -1) +
  labs(title = "SPAEF per Date and Model pair",
       x = "Date",
       y = "Model pair",
       fill = "SPAEF") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12.5),  # Adjust the size of x-axis title
        axis.title.y = element_text(size = 12.5),
        plot.title = element_text(size = 14.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12.5)) # Adjust the size of y-axis title



path_spaef_plots = paste(path_data, "Processed/export/R_plots/SPAEF", sep = "")

ggsave(spaef_heatmap, filename = paste(path_spaef_plots, "/spaef_heatmap.png", sep = ""),
       width = 3212, height = 1942, units = "px")
  



### 3D PLot PLotly

library(plotly)
spaef_wider <- pivot_wider(spaef_pivot)
spaef_wider



fig <- spaef_wider %>%
  filter(!(Date %in% "Mean")) %>%
  filter(!(Models %in% c("SEBAL_SSEB", "SSEB_METRIC", "SSEB_WASIM") & Date %in% "2020-03-27")) %>% 
  plot_ly(x = ~alpha, y = ~beta, z = ~gamma,
               marker = list(color = ~SPAEF, showscale = TRUE, colorscale="vik", 
                             reversescale = TRUE),
          hoverinfo = "text",
          hovertext = ~paste0("alpha: ", round(alpha, 2),
                              "\nbeta: ", round(beta, 2),
                              "\ngamma: ", round(gamma, 2))) %>% 
  layout(scene = list(xaxis = list(title = 'Correlation (alpha)'),
                                   yaxis = list(title = "CV (beta)"),
                                   zaxis = list(title = "Histo Match (gamma)")),
                      annotations = list(x = 1.06,
                        y = 1.02,
                        text = 'SPAEF',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                        ))

fig







