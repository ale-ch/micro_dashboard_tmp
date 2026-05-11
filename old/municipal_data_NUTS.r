library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(tidyr)

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

shapes <- lapply(nuts_shp_files, function(x) st_read(x,quiet = TRUE))
names(shapes) <- shape_names


shapes_IT <- lapply(shapes, function(x) {
  x %>% 
    filter(
      str_detect(.[[1]], "IT")
    )
})


nuts_munic_codes <- read_excel(nuts_munic_codes_file)
nuts_munic_codes <- nuts_munic_codes %>% 
  standardize_names() %>% 
  rename(
    PRO_COM_T = codice_comune_alfanumerico,
    NUTS3_Code = codice_nuts3_2024
  )


nuts_munic_codes_selected <- nuts_munic_codes %>% 
  select(
    PRO_COM_T, comune, NUTS3_Code
  )


nuts_munic_codes_selected <- nuts_munic_codes_selected %>% 
  mutate(
    NUTS2_Code = str_sub(NUTS3_Code, 1, 4),
    NUTS1_Code = str_sub(NUTS3_Code, 1, 3),
    NUTS0_Code = str_sub(NUTS3_Code, 1, 2)
  )


munic_data_nuts <- left_join(
  municipal_data_merged, 
  nuts_munic_codes_selected %>% select(-comune), 
  by = "PRO_COM_T") %>% 
  select(-`ISTAT region code`) %>% 
  rename(
    NUTS3 = NUTS3_Code,
    NUTS2 = NUTS2_Code,
    NUTS1 = NUTS1_Code,
    NUTS0 = NUTS0_Code,
  )



nuts_code <- "NUTS1"

munic_data_nuts %>%
  group_by(
    .data[[nuts_code]]
    #NUTS2_Code, 
    # year
  ) %>% 
  reframe(
    across(14:163, \(x) median(x, na.rm = TRUE))
  ) %>% 
  filter(
    !is.na(.data[[nuts_code]])
  ) %>% 
  View()


municipal_data_nuts %>%
  group_by(
    .data[[nuts_code]]
    #NUTS2_Code, 
    # year
  ) %>% 
  reframe(
    across(14:163, \(x) median(x, na.rm = TRUE))
  ) %>% 
  filter(
    !is.na(.data[[nuts_code]])
  ) %>% 
  View()



