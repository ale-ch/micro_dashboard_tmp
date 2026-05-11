### QUALITY CONTROL ###

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



names(municipal_data_merged)



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


which(!(unique(nuts_munic_codes_selected$PRO_COM_T) %in% unique(municipal_data_merged$PRO_COM_T)))
not_common_keys <- unique(nuts_munic_codes_selected$PRO_COM_T)[which(!(unique(nuts_munic_codes_selected$PRO_COM_T) %in% unique(municipal_data_merged$PRO_COM_T)))]
nuts_munic_codes_selected %>% 
  filter(
    PRO_COM_T %in% not_common_keys
  ) %>% 
  group_by(
    NUTS2_Code
  ) %>% 
  count()


nuts_munic_codes_selected %>% 
  filter(
    PRO_COM_T %in% not_common_keys,
    NUTS2_Code %in% "ITG2"
  )




merged_codes <- left_join(
  nuts_munic_codes_selected, 
  municipal_data_merged %>% 
    select(COMUNE, PRO_COM_T) %>% 
    rename(
      comune = COMUNE
    ) %>% 
    st_drop_geometry() %>% 
    unique(), 
  by = "comune")


merged_codes %>% 
  summarise(
    across(everything(), \(x) sum(is.na(x)))
    )

merged_codes %>% 
  filter(is.na(PRO_COM_T.y)) %>% 
  select(comune)


nuts_munic_codes_selected_renamed <- nuts_munic_codes_selected %>%
  mutate(comune = case_when(
    comune == "Murisengo Monferrato" ~ "Murisengo",
    comune == "Castegnero Nanto" ~ "Castegnero",
    comune == "Tripi - Abakainon" ~ "Tripi",
    TRUE ~ comune
  ))


merged_codes2 <- left_join(
  nuts_munic_codes_selected_renamed, 
  municipal_data_merged %>% 
    select(COMUNE, PRO_COM_T) %>% 
    rename(
      comune = COMUNE
    ) %>% 
    st_drop_geometry() %>% 
    unique(), 
  by = "comune")


nuts_munic_codes <- left_join(
  nuts_munic_codes_selected_renamed, 
  municipal_data_merged %>% 
    select(COMUNE, PRO_COM_T) %>% 
    rename(
      comune = COMUNE
    ) %>% 
    st_drop_geometry() %>% 
    unique(), 
  by = "comune")





