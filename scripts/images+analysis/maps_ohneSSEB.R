library(tidyverse)
library(raster)
library(rgdal)
library(lubridate)
library(viridis)
library(sf)
library(scico)

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

# Find maximum value of all four models to scale the colour gradient to the overall maximum

df_max <- df[, c("SEBAL", "METRIC", "WASIM")]
lapply(df_max, max, na.rm=TRUE)
max_val = max(df_max, na.rm = TRUE)

# Find max and min for Difference Rasters
df_diff = df[,c("diff_METRIC", "diff_WASIM")]
diff_limits = c(min(df_diff, na.rm = TRUE), max(df_diff, na.rm = TRUE))

########################################
# SEBAL 

SEBAL_raster <- ggplot(df, aes(x = x, y = y, fill = SEBAL)) +
                    geom_raster() +
                    scale_fill_viridis(option = "turbo", na.value = "transparent", direction = -1, limits = c(0, max_val)) +  # Inverse color fill gradient
                    theme_bw() +
                    coord_equal() +
                    labs(title = paste("SEBAL", date_met),
                         fill = "ETa (mm)") +
                    theme(plot.title = element_text(hjust = 0.5),  # Center the title
                          plot.margin = unit(c(0, 0, 0, 0), "cm"),  # Remove space around the figure / Oben, Rechts, unten, Links
                          axis.title = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text=element_blank(),
                          legend.position = c(0.1, 0.1),
                          legend.justification = c("left", "bottom"),
                          legend.margin = margin(2, 2, 2, 2))  # Remove axis Label


ggsave(SEBAL_raster, filename = paste(path_plots, "/SEBAL_raster.png"),
       width = 2.94, height = 5.19, units = "in")




########################################
# METRIC

METRIC_raster <- ggplot(df, aes(x = x, y = y, fill = METRIC)) +
                    geom_raster() +
                    scale_fill_viridis(option="turbo", na.value = "transparent", direction = -1, limits = c(0, max_val)) +  # Inverse color fill gradient
                    theme_bw() +
                    coord_equal() +
                    labs(title = paste("METRIC", date_met),
                         fill = "ETa (mm)") +
                    theme(plot.title = element_text(hjust = 0.5),  # Center the title
                          plot.margin = unit(c(0, 0, 0, 0), "cm"),  # Remove space around the figure / Oben, Rechts, unten, Links
                          axis.title = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text=element_blank(),
                          legend.position = c(0.1, 0.1),
                          legend.justification = c("left", "bottom"),
                          legend.margin = margin(2, 2, 2, 2))  # Remove axis Label


ggsave(METRIC_raster, filename = paste(path_plots, "/METRIC_raster.png"),
       width = 2.94, height = 5.19, units = "in")



METRIC_diff <- ggplot(df, aes(x = x, y = y, fill = diff_METRIC)) +
                  geom_raster() +
                  scale_fill_scico(palette = "vik", na.value = "transparent", midpoint = 0, limits = c(diff_limits[1], diff_limits[2])) + 
                  theme_bw() +
                  coord_equal() +
                  labs(title = paste("SEBAL-METRIC", date_met),
                       fill = "ETa (mm)") +
                  theme(plot.title = element_text(hjust = 0.5),  # Center the title
                        plot.margin = unit(c(0, 0, 0, 0), "cm"),  # Remove space around the figure / Oben, Rechts, unten, Links
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text=element_blank(),
                        legend.position = c(0.1, 0.1),
                        legend.justification = c("left", "bottom"),
                        legend.margin = margin(2, 2, 2, 2))  # Remove axis Label


ggsave(METRIC_diff, filename = paste(path_plots, "/METRIC_diff.png"),
       width = 2.94, height = 5.19, units = "in")




########################################
# WASIM

WASIM_raster <- ggplot(df, aes(x = x, y = y, fill = WASIM)) +
                    geom_raster() +
                    scale_fill_viridis(option="turbo", na.value = "transparent", direction = -1, limits = c(0, max_val)) +  # Inverse color fill gradient
                    theme_bw() +
                    coord_equal() +
                    labs(title = paste("WASIM", date_met),
                         fill = "ETa (mm)") +
                  theme(plot.title = element_text(hjust = 0.5),  # Center the title
                        plot.margin = unit(c(0, 0, 0, 0), "cm"),  # Remove space around the figure / Oben, Rechts, unten, Links
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text=element_blank(),
                        legend.position = c(0.1, 0.1),
                        legend.justification = c("left", "bottom"),
                        legend.margin = margin(2, 2, 2, 2))  # Remove axis Label


ggsave(WASIM_raster, filename = paste(path_plots, "/WASIM_raster.png"),
       width = 2.94, height = 5.19, units = "in")



WASIM_diff <- ggplot(df, aes(x = x, y = y, fill = diff_WASIM)) +
                  geom_raster() +
                  scale_fill_scico(palette = "vik", na.value = "transparent", midpoint = 0, limits = c(diff_limits[1], diff_limits[2])) + 
                  theme_bw() +
                  coord_equal() +
                  labs(title = paste("SEBAL-WASIM", date_met),
                       fill = "ETa (mm)") +
                  theme(plot.title = element_text(hjust = 0.5),  # Center the title
                        plot.margin = unit(c(0, 0, 0, 0), "cm"),  # Remove space around the figure / Oben, Rechts, unten, Links
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text=element_blank(),
                        legend.position = c(0.1, 0.1),
                        legend.justification = c("left", "bottom"),
                        legend.margin = margin(2, 2, 2, 2))  # Remove axis Label
ggsave(WASIM_diff, filename = paste(path_plots, "/WASIM_diff.png"),
       width = 2.94, height = 5.19, units = "in")







