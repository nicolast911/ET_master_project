library(tidyverse)
library(raster)
library(rgdal)
library(lubridate)
library(viridis)
library(sf)
library(scico)
library(gridExtra)


# Load LandUse Map
LU_path = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/Processed/study_area/LU_raster.tif"
LU = raster(LU_path)




# LandUse Classes
LU_agri = c(211, 212, 221, 222, 223, 241, 242, 243, 244)
LU_bare = c(331, 332, 333, 334, 131)
LU_grass = c(231, 321, 322, 323, 324, 411, 412)
LU_urban = c(111, 112, 121, 122, 123, 124, 132, 133, 141, 142)
LU_forest = c(311, 312, 313)
LU_water = c(511, 512, 335)


LU_copy = LU

# Replace Values
# AGRI = 1
for (i in LU_agri){
  LU_copy[LU_copy == i] <- 1
}

# BARE = 2
for (i in LU_bare){
  LU_copy[LU_copy == i] <- 2
}

# GRASS = 3
for (i in LU_grass){
  LU_copy[LU_copy == i] <- 3
}

# URBAN = 4
for (i in LU_urban){
  LU_copy[LU_copy == i] <- 4
}

# FOREST = 5
for (i in LU_forest){
  LU_copy[LU_copy == i] <- 5
}

# WATER = 6
for (i in LU_water){
  LU_copy[LU_copy == i] <- 6
}

plot(LU_copy)
hist(LU_copy)




# Export as Tiff
path_LU_export_tif = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/Processed/study_area/LU_raster_combined.tif"
writeRaster(LU_copy, file = path_LU_export_tif, format="GTiff", overwrite = TRUE)



# Export as csv Dataframe
LU_df <- as.data.frame(LU_copy, xy = TRUE)
names(LU_df)[3] <- "values"

path_LU_export_csv = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/Processed/study_area/LU_raster_combined.csv"

write.csv(LU_df, file = path_LU_export_csv, row.names=FALSE)

