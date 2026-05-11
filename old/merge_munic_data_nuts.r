# Merge municipal data with NUTS codes

library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(tidyr)

source('/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/LOAD_DATA.r')

standardize_names <- function(df) {
  df %>%
    rename_with(~ .x %>%
                  str_to_lower() %>%
                  str_replace_all("[^a-z0-9]+", "_") %>%
                  str_remove_all("^_+|_+$"))
}


nuts_munic_codes_file <- "NUTS_Municipal_codes.xlsx"

nuts_shp_files <- c(
  '/Volumes/T7 Shield/FRES/macro_dashboard/data/Geometrie/Shapefile_NUTS0.shp',
  '/Volumes/T7 Shield/FRES/macro_dashboard/data/Geometrie/Shapefile_NUTS1.shp',
  '/Volumes/T7 Shield/FRES/macro_dashboard/data/Geometrie/Shapefile_NUTS2.shp',
  '/Volumes/T7 Shield/FRES/macro_dashboard/data/Geometrie/Shapefile_NUTS3.shp'
)

shape_names <- c("NUTS0", "NUTS1", "NUTS2", "NUTS3")

shapes_df_list <- lapply(nuts_shp_files, function(x) st_read(x,quiet = TRUE))
names(shapes_df_list) <- shape_names
shapes_df_list <- lapply(shapes_df_list, function(x) {
  x %>% 
    filter(
      str_detect(.[[1]], "IT")
    )
})

shapes_df_list <- lapply(shapes_df_list, function(df) {
  names(df)[1] <- substr(names(df)[1], 1, 5)
  df
})


nuts_munic_codes <- read_excel(nuts_munic_codes_file)
nuts_munic_codes <- nuts_munic_codes %>% 
  standardize_names() %>% 
  rename(
    PRO_COM_T = codice_comune_alfanumerico,
    NUTS3_Code = codice_nuts3_2024
  ) %>% 
  select(
    PRO_COM_T, comune, NUTS3_Code
  ) %>% 
  mutate(
    NUTS2_Code = str_sub(NUTS3_Code, 1, 4),
    NUTS1_Code = str_sub(NUTS3_Code, 1, 3),
    NUTS0_Code = str_sub(NUTS3_Code, 1, 2)
  ) %>%
  mutate(comune = case_when(
    comune == "Murisengo Monferrato" ~ "Murisengo",
    comune == "Castegnero Nanto" ~ "Castegnero",
    comune == "Tripi - Abakainon" ~ "Tripi",
    TRUE ~ comune
  )) %>% 
  left_join(
  municipal_data_merged %>% 
    select(COMUNE, PRO_COM_T) %>% 
    rename(
      comune = COMUNE
    ) %>% 
    st_drop_geometry() %>% 
    unique(), 
  by = "comune") %>% 
  select(
    3:7
  ) %>% 
  rename(
    PRO_COM_T = PRO_COM_T.y
  )



municipal_data_nuts <- left_join(municipal_data_merged, nuts_munic_codes, by = "PRO_COM_T") %>% 
  select(-`ISTAT region code`) %>% 
  rename(
    NUTS3 = NUTS3_Code,
    NUTS2 = NUTS2_Code,
    NUTS1 = NUTS1_Code,
    NUTS0 = NUTS0_Code,
  )



rm(list = setdiff(ls(), c("municipal_data_nuts", "shapes_df_list")))


