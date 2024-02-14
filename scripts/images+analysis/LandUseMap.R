library(tidyverse)
library(raster)
library(lubridate)
library(viridis)



# Load LandUse Map
LU_path = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/Processed/study_area/LU_raster.tif"
LU = raster(LU_path)

path_data = "D:/Nicolas_D/Geodaten/Masterarbeit/DATA_MesoHyd_MA-SEBAL/" # Path data NICOLAS-PC
path_plots = paste(path_data, "Processed/export/R_plots/General", sep = "")



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



#################
#################
# PLOT
## Add Strings of Land use Class
string_vector <- c("Agriculture", "Bare Soil", "Grassland", "Urban Area", "Forest")

LU_df <- LU_df %>%
  mutate(LandUse_name = case_when(
    values == 1 ~ string_vector[1],
    values == 2 ~ string_vector[2],
    values == 3 ~ string_vector[3],
    values == 4 ~ string_vector[4],
    values == 5 ~ string_vector[5]
  ))

LU_df$LandUse_name <- factor(LU_df$LandUse_name, 
                                   levels = c("Agriculture", "Bare Soil", "Grassland", "Urban Area", "Forest"))

## Plot
land_use_colors <- c("Agriculture" =  "#FDE725FF", "Bare Soil" = "#616161", "Grassland" = "#7AD151FF", "Urban Area" ="#DC697D", "Forest" =  "#337538")




LU_raster <- ggplot(LU_df, aes(x = x, y = y, fill = LandUse_name)) +
  geom_raster() +
  theme_bw() +
  coord_equal() +
  scale_fill_manual(values = land_use_colors, na.value = "transparent") +
  labs(title = paste("CORINE Land Cover"),
       fill = "Land Use Class") +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        plot.margin = unit(c(0, 0, 0, 0), "cm"),  # Remove space around the figure / Oben, Rechts, unten, Links
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        legend.position = c(0.1, 0.1),
        legend.justification = c("left", "bottom"),
        legend.margin = margin(2, 2, 2, 2),
        legend.text = element_text(size = 10))  # Remove axis Label

LU_raster


ggsave(LU_raster, filename = paste(path_plots, "/LU_raster.png"),
       width = 2.94, height = 5.19, units = "in")
