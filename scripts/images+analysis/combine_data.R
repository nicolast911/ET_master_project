library(tidyverse)
library(raster)
library(rgdal)
library(lubridate)

### Paths
path_data = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/" # Path data NICOLAS-PC


# SEBAL
SEBAL_path = paste(path_data, "Processed/SEBAL_raster_export/SEBAL_", date_SSEB, ".tif", sep = "")
SEBAL = raster(SEBAL_path)
SEBAL_df <- as.data.frame(SEBAL, xy = TRUE)
names(SEBAL_df)[3] <- "values"

# SSEB
SSEB_path = paste(path_data, "Processed/SSEB_raster_export/SSEB_", date_SSEB, ".tif", sep = "")
SSEB = raster(SSEB_path)
SSEB_df <- as.data.frame(SSEB, xy = TRUE)
names(SSEB_df)[3] <- "values"

# METRIC
METRIC_path = paste(path_data, "Processed/METRIC_raster_export/METRIC_", date_SSEB, ".tif", sep = "")
METRIC = raster(METRIC_path)
METRIC_df <- as.data.frame(METRIC, xy = TRUE)
names(METRIC_df)[3] <- "values"

# WASIM
#path_wasim_kelle = paste(path_data, "Original/WASIM_raster/etr_kelle_", date_SSEB, ".tif", sep = "")
# New Path with WASIM Rasters aligning to SEBAL map
path_wasim_kelle = paste(path_data, "Processed/WASIM_raster_export/etr_kelle_", date_SSEB, ".tif", sep = "")
WASIM = raster(path_wasim_kelle)
WASIM_df <- as.data.frame(WASIM, xy = TRUE)
names(WASIM_df)[3] <- "values"


# Difference Data
diff_SSEB <- SEBAL_df$values - SSEB_df$values 
diff_METRIC <- SEBAL_df$values - METRIC_df$values 
diff_WASIM <-  SEBAL_df$values - WASIM_df$values 




# DataFrame all Models
df_models <- data.frame("x" = SEBAL_df$x,
                        "y" = SEBAL_df$y, 
                        "SEBAL" =  SEBAL_df$values,
                        "SSEB" = SSEB_df$values, 
                        "METRIC" = METRIC_df$values, 
                        "WASIM" = WASIM_df$values,
                        "diff_SSEB" = diff_SSEB,
                        "diff_METRIC" = diff_METRIC,
                        "diff_WASIM" = diff_WASIM)

path_combined_data = paste(path_data, "Processed/export/combined_data/combined_", date_SSEB, ".csv", sep = "")

write.csv(df_models, file = path_combined_data, row.names=FALSE)