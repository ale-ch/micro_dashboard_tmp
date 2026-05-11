library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(tidyr)
library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(tidyr)
library(dplyr)
library(lubridate)


standardize_names <- function(df) {
  df %>%
    rename_with(~ .x %>%
                  str_to_lower() %>%
                  str_replace_all("[^a-z0-9]+", "_") %>%
                  str_remove_all("^_+|_+$"))
}


# comuni <- readRDS("/Volumes/T7 Shield/FRES/DB_Comunale/RData/TO_CLEAN/db_comuni_sampled.RDS")

comuni <- readRDS('/Volumes/T7 Shield/FRES/DB_Comunale/RData/TO_CLEAN/comuni_nogeom.RDS')


####################

comuni <- comuni %>%
  as.data.frame() %>% 
  mutate(
    pro_com_t = str_pad(as.character(pro_com), width = 6, side = "left", pad = "0")
  ) %>% 
  select(-(38:45))


comuni_region <- comuni %>% 
  select(pro_com_t, comune, regione,macro_area4, ripartizione_istat5) %>% 
  filter(!duplicated(pro_com_t))


comuni_sum <- comuni %>% 
  group_by(pro_com_t, anno) %>% 
  reframe(
    across(3:7, median, na.rm = TRUE),
    across(25:33, sum, na.rm = TRUE)
  )


joined <- left_join(comuni_sum, comuni_region) %>% 
  rename(
    PRO_COM_T = pro_com_t,
    year = anno
  )

# saveRDS(joined, "/Volumes/T7 Shield/FRES/DB_Comunale/RData/TO_CLEAN/CLEANED/db_comuni_summed_total.RDS")

# db_comuni_summed <- readRDS("/Volumes/T7 Shield/FRES/DB_Comunale/RData/TO_CLEAN/CLEANED/db_comuni_summed.RDS")


fuel_prices_summed <- readRDS("/Volumes/T7 Shield/FRES/DB_Comunale/RData/TO_CLEAN/CLEANED/db_comuni_summed_total.RDS")
