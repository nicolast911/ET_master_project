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
             "2020-09-19")

# List of Metric Limits 
METRIC_limit_list <- c(6, 9, 5, 4.5, 4.5, 4.5, 4, 9, 6)

  
  
  
# Start of For loop

for (i in 1:9) {
  MLimit <- METRIC_limit_list[i]
  
  
  # Set date(s) of modeling/data retrieval
  # date <- "2015-07-04"  # Initial LS Picture
  date <- date_list[i]
  
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
    source("combine_data.R")
    df <- read.csv(path_combined_data)
  }
  
  
  ### PLOTS
  path_plots = paste(path_data, "Processed/export/R_plots/", date_SSEB, sep="")
  
  # Number of bins with sqrt(n)
  
  sqrt(length(na.omit(df$SEBAL)))
  sqrt(length(na.omit(df$WASIM)))
  
  # Maximum Limit of Plot axis 
  
  lapply(df, max, na.rm=TRUE)
  
  df_max_SSEB <- df[, c("SEBAL", "SSEB")]
  max_val_SSEB <-  max(df_max_SSEB, na.rm = TRUE)
  
  df_max_METRIC <- df[, c("SEBAL", "METRIC")]
  max_val_METRIC <-  max(df_max_METRIC , na.rm = TRUE)
  
  df_max_WASIM <- df[, c("SEBAL", "WASIM")]
  max_val_WASIM <-  max(df_max_WASIM, na.rm = TRUE)
  
  
  
  ######################################################################
  ######################################################################
  ### SCATTERPLOT  # 2D Hist/Density scatter
  
  ########################
  # SSEB
  
  # Calculate Measures
  rsquared <- (cor(df$SSEB, df$SEBAL, use = "complete.obs"))^2
  rmse <- sqrt(mean((df$SSEB - df$SEBAL)^2, na.rm = TRUE))
  rel_rmse <- (rmse / mean(df$SSEB, na.rm = TRUE)) *100
  
  
  SSEB_scatter <- ggplot(df, aes(x = SEBAL, y = SSEB)) +
    geom_bin2d(bins = 900) +
    scale_fill_viridis(option = "plasma") +
    theme_bw() +
    labs(x = "SEBAL  ETa (mm)",
         y = "SSEB  ETa (mm)",
         fill = "Count",
         title = paste("SEBAL ~ SSEB ", date_met)) +
    coord_cartesian(xlim = c(0, max_val_SSEB), ylim = c(0, max_val_SSEB)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    annotate("text", x = 0.03 , y = 1 * max_val_SSEB, label = paste("R² = ", round(rsquared, 2)), hjust = 0, size = 4.2) +
    annotate("text", x = 0.03 , y = 0.96 * max_val_SSEB, label = paste("RMSE = ", round(rmse, 2), "mm"), hjust = 0, size = 4.2) +
    annotate("text", x = 0.03 , y = 0.92 * max_val_SSEB, label = paste("rRMSE = ", round(rel_rmse, 2), "%"), hjust = 0, size = 4.2) +
    theme(axis.title.x = element_text(size = 12.5),  # Adjust the size of x-axis title
          axis.title.y = element_text(size = 12.5),
          plot.title = element_text(size = 14.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)) # Adjust the size of y-axis title
  
  
  SSEB_scatter
  
  ggsave(SSEB_scatter, filename = paste(path_plots, "/SSEB_scatter.png"),
         width = 1564, height = 1635, units = "px")
  
  
  
  ########################
  # METRIC
  
  # Calculate Measures
  rsquared <- (cor(df$METRIC, df$SEBAL, use = "complete.obs"))^2
  rmse <- sqrt(mean((df$METRIC - df$SEBAL)^2, na.rm = TRUE))
  rel_rmse <- (rmse / mean(df$METRIC, na.rm = TRUE)) *100
  
  
  
  METRIC_scatter <- ggplot(df, aes(x = SEBAL, y = METRIC)) +
    geom_bin2d(bins = 900) +
    scale_fill_viridis(option = "plasma") +
    theme_bw() +
    labs(x = "SEBAL  ETa (mm)",       # Set x-axis label to the column name of x-data
         y = "METRIC  ETa (mm)",       # Set y-axis label to the column name of y-data
         fill = "Count",              # Set legend title to "Count"
         title = paste("SEBAL ~ METRIC ", date_met)) +  # Set plot title
    #coord_cartesian(xlim = c(0, max_val_METRIC), ylim = c(0, max_val_METRIC)) +  # Set axis limits AUTOMATICAL
    coord_cartesian(xlim = c(0, MLimit), ylim = c(0, MLimit)) +                             # Set axis limits MANUAL
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    annotate("text", x = 0.03 , y = 1 * MLimit, label = paste("R² = ", round(rsquared, 2)), hjust = 0, size = 4.2) +
    annotate("text", x = 0.03 , y = 0.96 * MLimit, label = paste("RMSE = ", round(rmse, 2), "mm"), hjust = 0, size = 4.2) +
    annotate("text", x = 0.03 , y = 0.92 * MLimit, label = paste("rRMSE = ", round(rel_rmse, 2), "%"), hjust = 0, size = 4.2) +
    theme(axis.title.x = element_text(size = 12.5),  # Adjust the size of x-axis title
          axis.title.y = element_text(size = 12.5),
          plot.title = element_text(size = 14.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)) # Adjust the size of y-axis title# Add 1:1 line
  
  METRIC_scatter
  
  ggsave(METRIC_scatter, filename = paste(path_plots, "/METRIC_scatter.png"),
         width = 1564, height = 1635, units = "px")
  
  
  ########################
  # WASIM
  
  # Calculate Measures
  rsquared <- (cor(df$WASIM, df$SEBAL, use = "complete.obs"))^2
  rmse <- sqrt(mean((df$WASIM - df$SEBAL)^2, na.rm = TRUE))
  rel_rmse <- (rmse / mean(df$WASIM, na.rm = TRUE)) *100
  
  
  
  WASIM_scatter <- ggplot(df, aes(x = SEBAL, y = WASIM)) +
    geom_bin2d(bins = 650) +
    scale_fill_viridis(option = "plasma") +
    theme_bw() +
    labs(x = "SEBAL  ETa (mm)",       # Set x-axis label to the column name of x-data
         y = "WASIM  ETa (mm)",       # Set y-axis label to the column name of y-data
         fill = "Count",              # Set legend title to "Count"
         title = paste("SEBAL ~ WASIM  ", date_met)) +  # Set plot title
    coord_cartesian(xlim = c(0, max_val_WASIM), ylim = c(0, max_val_WASIM)) +  # Set axis limits AUTOMATICAL
    #coord_cartesian(xlim = c(0, 4), ylim = c(0, 4)) +                           # Set axis limits MANUAL
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add 1:1 line
    annotate("text", x = 0.03 , y = 1 * max_val_WASIM, label = paste("R² = ", round(rsquared, 2)), hjust = 0, size = 4.2) +
    annotate("text", x = 0.03 , y = 0.96 * max_val_WASIM, label = paste("RMSE = ", round(rmse, 2), "mm"), hjust = 0, size = 4.2) +
    annotate("text", x = 0.03 , y = 0.92 * max_val_WASIM, label = paste("rRMSE = ", round(rel_rmse, 2), "%"), hjust = 0, size = 4.2) +
    theme(axis.title.x = element_text(size = 12.5),  # Adjust the size of x-axis title
          axis.title.y = element_text(size = 12.5),
          plot.title = element_text(size = 14.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)) # Adjust the size of y-axis title
  
  WASIM_scatter
  
  ggsave(WASIM_scatter, filename = paste(path_plots, "/WASIM_scatter.png"),
         width = 1564, height = 1635, units = "px")
  
  
  
  
  
  ########################
  ### EXTRA #####
  ########################
  # METRIC vs. SSEB
  
  # Calculate Measures
  rsquared <- (cor(df$SSEB, df$METRIC, use = "complete.obs"))^2
  rmse <- sqrt(mean((df$SSEB - df$METRIC)^2, na.rm = TRUE))
  rel_rmse <- (rmse / mean(df$SSEB, na.rm = TRUE)) *100
  
  
  METRIC_SSEB_scatter <- ggplot(df, aes(x = METRIC, y = SSEB)) +
    geom_bin2d(bins = 900) +
    scale_fill_viridis(option = "plasma") +
    theme_bw() +
    labs(x = "METRIC  ETa (mm)",       # Set x-axis label to the column name of x-data
         y = "SSEB  ETa (mm)",       # Set y-axis label to the column name of y-data
         fill = "Count",              # Set legend title to "Count"
         title = paste("METRIC ~ SSEB ", date_met)) +  # Set plot title
    coord_cartesian(xlim = c(0, MLimit), ylim = c(0, MLimit)) +  # Set axis limits AUTOMATICAL
    #coord_cartesian(xlim = c(0, 4), ylim = c(0, 4)) +                           # Set axis limits MANUAL
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add 1:1 line
    annotate("text", x = 0.03 , y = 1 * MLimit, label = paste("R² = ", round(rsquared, 2)), hjust = 0, size = 4.2) +
    annotate("text", x = 0.03 , y = 0.96 * MLimit, label = paste("RMSE = ", round(rmse, 2), "mm"), hjust = 0, size = 4.2) +
    annotate("text", x = 0.03 , y = 0.92 * MLimit, label = paste("rRMSE = ", round(rel_rmse, 2), "%"), hjust = 0, size = 4.2) +
    theme(axis.title.x = element_text(size = 12.5),  # Adjust the size of x-axis title
          axis.title.y = element_text(size = 12.5),
          plot.title = element_text(size = 14.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)) # Adjust the size of y-axis title
  
  
  
  METRIC_SSEB_scatter
  
  ggsave(METRIC_SSEB_scatter, filename = paste(path_plots, "/METRIC_SSEB_scatter.png"),
         width = 1564, height = 1635, units = "px")




}





