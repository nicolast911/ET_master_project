library(tidyverse)
library(viridis)
library(ggpointdensity)


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



# Set date(s) of modeling/data retrieval
# date <- "2015-07-04"  # Initial LS Picture
date <- date_list[9]

date <- as.Date(date, format = "%Y-%m-%d")
# Date in meteo_raster_daily Path Format
date_met <- format(date, "%Y-%m-%d")
# Date in WASIM-Raster Path Format
date_wasim <- format(date, "%Y_%m_%d") %>%
  str_replace_all("_0", "_") %>%
  str_replace_all("_", "_")
# Date in ETp-FAO56-Raster Path Format
date_etp <- format(date, "%Y_%m_%d") %>%
  str_replace_all("_0", "-") %>%
  str_replace_all("_", "-")
# Date SSEB Format
date_SSEB <- format(date, "%Y_%m_%d") %>%
  str_replace_all("-", "_")
# Date in Landsat Path Format
date_LS <- format(date, "%Y%m%d")




### LOAD DATA 
path_data = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/" # Path data NICOLAS-PC
path_combined_data = paste(path_data, "Processed/export/combined_data/combined_", date_SSEB, ".csv", sep = "")

# Check if the combined CSV file already exists, else source the code to create a combined dataframe
if (file.exists(path_combined_data)) {
  df <- read.csv(path_combined_data)
} else {
  source("combine_data_ohneSSEB.R")
  df <- read.csv(path_combined_data)
}


### PLOTS
path_plots = paste(path_data, "Processed/export/R_plots/", date_SSEB, sep="")

# Number of bins with sqrt(n)

sqrt(length(na.omit(df$SEBAL)))
sqrt(length(na.omit(df$WASIM)))

# Maximum Limit of Plot axis 

lapply(df_max, max, na.rm=TRUE)


df_max_METRIC <- df[, c("SEBAL", "METRIC")]
max_val_METRIC <-  max(df_max_METRIC , na.rm = TRUE)

df_max_WASIM <- df[, c("SEBAL", "WASIM")]
max_val_WASIM <-  max(df_max_WASIM, na.rm = TRUE)

### SCATTERPLOT  # 2D Hist/Density scatter

########################
# METRIC
METRIC_scatter <- ggplot(df, aes(x = SEBAL, y = METRIC)) +
                      geom_bin2d(bins = 900) +
                      scale_fill_viridis(option = "plasma") +
                      theme_bw() +
                      labs(x = "SEBAL",       # Set x-axis label to the column name of x-data
                           y = "METRIC",       # Set y-axis label to the column name of y-data
                           fill = "Count",              # Set legend title to "Count"
                           title = paste("SEBAL ~ METRIC", date_met)) +  # Set plot title
                      coord_cartesian(xlim = c(0, max_val_METRIC), ylim = c(0, max_val_METRIC)) +  # Set axis limits AUTOMATICAL
                      #coord_cartesian(xlim = c(0, 6), ylim = c(0, 6)) +                             # Set axis limits MANUAL
                      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")  # Add 1:1 line

METRIC_scatter

ggsave(METRIC_scatter, filename = paste(path_plots, "/METRIC_scatter.png"))


########################
# WASIM
WASIM_scatter <- ggplot(df, aes(x = SEBAL, y = WASIM)) +
                      geom_bin2d(bins = 650) +
                      scale_fill_viridis(option = "plasma") +
                      theme_bw() +
                      labs(x = "SEBAL",       # Set x-axis label to the column name of x-data
                           y = "WASIM",       # Set y-axis label to the column name of y-data
                           fill = "Count",              # Set legend title to "Count"
                           title = paste("SEBAL ~ WASIM", date_met)) +  # Set plot title
                      coord_cartesian(xlim = c(0, max_val_WASIM), ylim = c(0, max_val_WASIM)) +  # Set axis limits AUTOMATICAL
                      #coord_cartesian(xlim = c(0, 4), ylim = c(0, 4)) +                           # Set axis limits MANUAL
                      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")  # Add 1:1 line
WASIM_scatter

ggsave(WASIM_scatter, filename = paste(path_plots, "/WASIM_scatter.png"))



