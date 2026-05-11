library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(tidyr)
library(lubridate)

# base_path  <- "/Volumes/T7 Shield/FRES/DB_Comunale"
base_path <- '/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard'

# NUTS processing
nuts_munic_codes_file <- file.path(base_path, "micro_dashboard/NUTS_Municipal_codes.xlsx")

#nuts_shp_files <- c(
#  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS0.shp'),
#  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS1.shp'),
#  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS2.shp'),
#  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS3.shp')
#)

nuts_shp_files <- c(
  file.path(base_path, 'data/Geometrie/Shapefile_NUTS0.shp'),
  file.path(base_path, 'data/Geometrie/Shapefile_NUTS1.shp'),
  file.path(base_path, 'data/Geometrie/Shapefile_NUTS2.shp'),
  file.path(base_path, 'data/Geometrie/Shapefile_NUTS3.shp')
)


shape_names <- c("NUTS0", "NUTS1", "NUTS2", "NUTS3")
shapes_df_list <- lapply(nuts_shp_files, function(x) st_read(x, quiet = TRUE))
names(shapes_df_list) <- shape_names
shapes_df_list <- lapply(shapes_df_list, function(x) {
  x %>% filter(str_detect(.[[1]], "IT"))
})

shapes_df_list <- lapply(shapes_df_list, function(df) {
  names(df)[1] <- substr(names(df)[1], 1, 5)
  df
})

# municipal_data_merged <- readRDS(file.path(base_path, "RData/Merged/municipal_data_merged.RDS"))
# municipal_data_merged <- readRDS(file.path(base_path, "data/municipal_data_merged.RDS"))
municipal_data_merged <- readRDS("/Volumes/T7 Shield/FRES/DB_Comunale/RData/Merged/municipal_data_merged_NEW.RDS")

test <- TRUE 

if(isTRUE(test)) {
  n <- 1000
  sampled_codes <- sample(municipal_data_merged$PRO_COM_T, n, replace = FALSE)
  
  municipal_data_merged <- municipal_data_merged %>% 
    filter(
      PRO_COM_T %in% sampled_codes
    )
}

