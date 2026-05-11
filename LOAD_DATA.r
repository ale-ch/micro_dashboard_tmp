library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(tidyr)
library(lubridate)

# ---------------- PARAMS ----------------
base_path  <- "/Volumes/T7 Shield/FRES/DB_Comunale"
setwd(base_path)
rdata_path <- file.path(base_path, "RData")
draw_samples <- FALSE

# ---------------- HELPERS ----------------
standardize_names <- function(df) {
  df %>%
    rename_with(~ .x %>%
                  str_to_lower() %>%
                  str_replace_all("[^a-z0-9]+", "_") %>%
                  str_remove_all("^_+|_+$"))
}

prep_procom_t <- function(df, ref_area, time) {
  ref_area <- rlang::ensym(ref_area)
  time     <- rlang::ensym(time)
  
  df %>%
    as.data.frame() %>% 
    filter(str_detect(!!ref_area, "^[0-9.]+$")) %>%
    rename(PRO_COM_T = !!ref_area,
           year      = !!time) %>%
    mutate(
      year = as.integer(year),
      PRO_COM_T = str_pad(as.character(PRO_COM_T), width = 6, side = "left", pad = "0")
    ) %>%
    tidyr::complete(
      PRO_COM_T,
      year = 2014:2024
    ) %>% 
    fill(PRO_COM_T, .direction = "down")
}

# ---------------- LOAD SHAPE ----------------
comuni <- st_read(
  file.path(base_path, "Limiti01012025/Com01012025/Com01012025_WGS84.shp"),
  quiet = TRUE
)

# ---------------- PREP DATA 2: FUEL PRICES ----------------
comuni_nogeom <- readRDS(file.path(base_path, "RData/TO_CLEAN/comuni_nogeom.RDS"))

comuni_nogeom <- comuni_nogeom %>%
  as.data.frame() %>% 
  mutate(
    pro_com_t = str_pad(as.character(pro_com), width = 6, side = "left", pad = "0")
  ) %>% 
  select(-(38:45))

comuni_region <- comuni_nogeom %>% 
  select(pro_com_t, comune, regione, macro_area4, ripartizione_istat5) %>% 
  filter(!duplicated(pro_com_t))

comuni_sum <- comuni_nogeom %>% 
  group_by(pro_com_t, anno) %>% 
  reframe(
    across(3:7, median, na.rm = TRUE),
    across(25:33, sum, na.rm = TRUE)
  )

fuel_prices_summed <- left_join(comuni_sum, comuni_region, by = "pro_com_t") %>% 
  rename(
    PRO_COM_T = pro_com_t,
    year = anno
  )

# ---------------- PREP DATA 3: PNNR ----------------
load(file.path(base_path, "RData/TO_CLEAN/PNNR.RData")) # Loads Data4

df_pnnr <- Data4 %>% 
  select(1:12, 17:28, Data.Inizio.Progetto.Prevista)

df_renamed <- df_pnnr[, 13:26] %>% 
  mutate(across(1:11, as.numeric))

df_renamed <- standardize_names(df_renamed)

df2 <- df_pnnr[, 1:12] %>% 
  st_drop_geometry() %>% 
  bind_cols(df_renamed) %>% 
  st_as_sf() %>%
  mutate(date_col = dmy(data_inizio_progetto_prevista),
         year = year(date_col))

PNNR_summed <- df2 %>%
  group_by(year, PRO_COM_T) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    PRO_COM_T = str_pad(as.character(PRO_COM_T), width = 6, side = "left", pad = "0")
  )

# ---------------- PREP MAIN DB ----------------
raw_env <- new.env()
rdata_files <- list.files(rdata_path, pattern="\\.RData$", full.names=TRUE)
walk(rdata_files, ~ load(.x, envir = raw_env))

data_list <- mget(ls(raw_env), envir = raw_env) %>%
  keep(is.data.frame) %>%
  map(standardize_names)

rm(raw_env)

# Prepping individual tables
data_list$addetti_TOT <- prep_procom_t(data_list$addetti_TOT, ref_area, time_period) %>%
  rename(n_workers = osservazione) %>% select(-data_type)

data_list$Altitudine_TOT_2014_2022 <- prep_procom_t(data_list$Altitudine_TOT_2014_2022, codice_istat_del_comune_alfanumerico, anno) %>%
  mutate(altitudine_del_centro_metri = as.numeric(altitudine_del_centro_metri))

names(data_list$panel_italiana)[3:26] <- paste0(names(data_list$panel_italiana)[3:26], "_italian")
data_list$panel_italiana <- prep_procom_t(data_list$panel_italiana, codice_comune, anno) %>%
  mutate(across(3:26, as.numeric))

names(data_list$panel_straniera)[3:26] <- paste0(names(data_list$panel_straniera)[3:26], "_foreign")
data_list$panel_straniera <- prep_procom_t(data_list$panel_straniera, codice_comune, anno) %>%
  mutate(across(3:26, as.numeric))

data_list$db_capacita_tota <- prep_procom_t(data_list$db_capacita_tota, na, x2014) %>%
  mutate(across(3:26, as.numeric)) %>% 
  select(-starts_with("na"))

data_list$Redditi_tot <- prep_procom_t(data_list$Redditi_tot, codice_istat_comune, anno_di_imposta)

data_list$results_gini_con_mediana <- prep_procom_t(data_list$results_gini_con_mediana, codice_istat_comune, anno_di_imposta)

data_list$UL_TOT <- prep_procom_t(data_list$UL_TOT, ref_area, time_period) %>%
  rename(n_firms = osservazione) %>% select(-data_type)

data_list$comuni_stats_all <- prep_procom_t(data_list$comuni_stats_all, pro_com, anno)

data_list$amministrazioni_comunali <- prep_procom_t(data_list$amministrazioni_comunali, istat_codice_comune, anno)

data_list$df_coesione_fine_mergiato_cut <- prep_procom_t(data_list$df_coesione_fine_mergiato_cut, cod_comune, anno)

data_list$MAQUI <- prep_procom_t(data_list$MAQUI, codice_comune, anno)

# Merge internal main datasets
comuni_meta <- comuni %>% st_drop_geometry() %>% as.data.frame()

merge_meta_T <- function(df) {
  left_join(comuni_meta, df, by = "PRO_COM_T")
}

all_dfs <- list(
  merge_meta_T(data_list$addetti_TOT),
  merge_meta_T(data_list$Altitudine_TOT_2014_2022),
  merge_meta_T(data_list$Redditi_tot),
  merge_meta_T(data_list$results_gini_con_mediana),
  merge_meta_T(data_list$UL_TOT),
  merge_meta_T(data_list$panel_italiana),
  merge_meta_T(data_list$panel_straniera),
  merge_meta_T(data_list$comuni_stats_all),
  merge_meta_T(data_list$db_capacita_tota),
  merge_meta_T(data_list$amministrazioni_comunali),
  merge_meta_T(data_list$df_coesione_fine_mergiato_cut),
  merge_meta_T(data_list$MAQUI)
)

data_merged <- Reduce(
  function(x, y) full_join(x, y, by = intersect(names(x), names(y))),
  all_dfs
) %>% as.data.frame()

municipal_data_merged <- left_join(
  comuni,
  data_merged,
  by = intersect(names(comuni_meta), names(data_merged))
) %>% 
  select(all_of(1:13), where(is.numeric), -cod_comune) 

# Rename large variable set
municipal_data_merged <- municipal_data_merged %>% rename(
  `Number of workers` = `n_workers`,
  `Altitude of the center in meters` = `altitudine_del_centro_metri`,
  `Coastal municipality` = `comune_litoraneo`,
  `Coastal zones` = `zone_costiere`,
  `Degree of urbanization` = `grado_di_urbanizzazione`,
  `ISTAT region code` = `codice_istat_regione`,
  `Number of taxpayers` = `numero_contribuenti`,
  `Income from buildings frequency` = `reddito_da_fabbricati_frequenza`,
  `Income from buildings total amount` = `reddito_da_fabbricati_ammontare`,
  `Income from employment and similar frequency` = `reddito_da_lavoro_dipendente_e_assimilati_frequenza`,
  `Income from employment and similar total amount` = `reddito_da_lavoro_dipendente_e_assimilati_ammontare`,
  `Income from pension frequency` = `reddito_da_pensione_frequenza`,
  `Income from pension total amount` = `reddito_da_pensione_ammontare`,
  `Income from self employment including null values frequency` = `reddito_da_lavoro_autonomo_comprensivo_dei_valori_nulli_frequenza`,
  `Income from self employment including null values total amount` = `reddito_da_lavoro_autonomo_comprensivo_dei_valori_nulli_ammontare`,
  `Income of entrepreneur in ordinary accounting including null values frequency` = `reddito_di_spettanza_dell_imprenditore_in_contabilita_ordinaria_comprensivo_dei_valori_nulli_frequenza`,
  `Income of entrepreneur in ordinary accounting including null values total amount` = `reddito_di_spettanza_dell_imprenditore_in_contabilita_ordinaria_comprensivo_dei_valori_nulli_ammontare`,
  `Income of entrepreneur in simplified accounting including null values frequency` = `reddito_di_spettanza_dell_imprenditore_in_contabilita_semplificata_comprensivo_dei_valori_nulli_frequenza`,
  `Income of entrepreneur in simplified accounting including null values total amount` = `reddito_di_spettanza_dell_imprenditore_in_contabilita_semplificata_comprensivo_dei_valori_nulli_ammontare`,
  `Income from participation including null values frequency` = `reddito_da_partecipazione_comprensivo_dei_valori_nulli_frequenza`,
  `Income from participation including null values total amount` = `reddito_da_partecipazione_comprensivo_dei_valori_nulli_ammontare`,
  `Taxable income frequency` = `reddito_imponibile_frequenza`,
  `Taxable income total amount` = `reddito_imponibile_ammontare`,
  `Net tax frequency` = `imposta_netta_frequenza`,
  `Net tax total amount` = `imposta_netta_ammontare`,
  `Additional taxable income frequency` = `reddito_imponibile_addizionale_frequenza`,
  `Additional taxable income total amount` = `reddito_imponibile_addizionale_ammontare`,
  `Regional surcharge due frequency` = `addizionale_regionale_dovuta_frequenza`,
  `Regional surcharge due total amount` = `addizionale_regionale_dovuta_ammontare`,
  `Municipal surcharge due frequency` = `addizionale_comunale_dovuta_frequenza`,
  `Municipal surcharge due total amount` = `addizionale_comunale_dovuta_ammontare`,
  `Total income less than zero euro frequency` = `reddito_complessivo_minore_di_zero_euro_frequenza`,
  `Total income less than zero euro total amount` = `reddito_complessivo_minore_di_zero_euro_ammontare`,
  `Total income between 0 and 10000 euro frequency` = `reddito_complessivo_da_0_a_10000_euro_frequenza`,
  `Total income between 0 and 10000 euro total amount` = `reddito_complessivo_da_0_a_10000_euro_ammontare`,
  `Total income between 10000 and 15000 euro frequency` = `reddito_complessivo_da_10000_a_15000_euro_frequenza`,
  `Total income between 10000 and 15000 euro total amount` = `reddito_complessivo_da_10000_a_15000_euro_ammontare`,
  `Total income between 15000 and 26000 euro frequency` = `reddito_complessivo_da_15000_a_26000_euro_frequenza`,
  `Total income between 15000 and 26000 euro total amount` = `reddito_complessivo_da_15000_a_26000_euro_ammontare`,
  `Total income between 26000 and 55000 euro frequency` = `reddito_complessivo_da_26000_a_55000_euro_frequenza`,
  `Total income between 26000 and 55000 euro total amount` = `reddito_complessivo_da_26000_a_55000_euro_ammontare`,
  `Total income between 55000 and 75000 euro frequency` = `reddito_complessivo_da_55000_a_75000_euro_frequenza`,
  `Total income between 55000 and 75000 euro total amount` = `reddito_complessivo_da_55000_a_75000_euro_ammontare`,
  `Total income between 75000 and 120000 euro frequency` = `reddito_complessivo_da_75000_a_120000_euro_frequenza`,
  `Total income between 75000 and 120000 euro total amount` = `reddito_complessivo_da_75000_a_120000_euro_ammontare`,
  `Total income over 120000 euro frequency` = `reddito_complessivo_oltre_120000_euro_frequenza`,
  `Total income over 120000 euro total amount` = `reddito_complessivo_oltre_120000_euro_ammontare`,
  `Income per capita` = `redditi_procapite`,
  `Share of poverty` = `share_of_poverty`,
  `Share of rich` = `share_of_rich`,
  `Gini coefficient with median` = `gini_con_mediana`,
  `Number of firms` = `n_firms`,
  `Municipality name` = `comune`,
  `Starting population male Italian` = `popolazione_inizio_maschi_italian`,
  `Births male Italian` = `nati_maschi_italian`,
  `Deaths male Italian` = `morti_maschi_italian`,
  `Internal registrations male Italian` = `iscritti_interni_maschi_italian`,
  `Internal cancellations male Italian` = `cancellati_interni_maschi_italian`,
  `Foreign registrations male Italian` = `iscritti_estero_maschi_italian`,
  `Foreign cancellations male Italian` = `cancellati_estero_maschi_italian`,
  `Ending population male Italian` = `popolazione_fine_maschi_italian`,
  `Starting population female Italian` = `popolazione_inizio_femmine_italian`,
  `Births female Italian` = `nati_femmine_italian`,
  `Deaths female Italian` = `morti_femmine_italian`,
  `Internal registrations female Italian` = `iscritti_interni_femmine_italian`,
  `Internal cancellations female Italian` = `cancellati_interni_femmine_italian`,
  `Foreign registrations female Italian` = `iscritti_estero_femmine_italian`,
  `Foreign cancellations female Italian` = `cancellati_estero_femmine_italian`,
  `Ending population female Italian` = `popolazione_fine_femmine_italian`,
  `Starting population total Italian` = `popolazione_inizio_totale_italian`,
  `Births total Italian` = `nati_totale_italian`,
  `Deaths total Italian` = `morti_totale_italian`,
  `Internal registrations total Italian` = `iscritti_interni_totale_italian`,
  `Internal cancellations total Italian` = `cancellati_interni_totale_italian`,
  `Foreign registrations total Italian` = `iscritti_estero_totale_italian`,
  `Foreign cancellations total Italian` = `cancellati_estero_totale_italian`,
  `Starting population male foreign` = `popolazione_inizio_maschi_foreign`,
  `Births male foreign` = `nati_maschi_foreign`,
  `Deaths male foreign` = `morti_maschi_foreign`,
  `Internal registrations male foreign` = `iscritti_interni_maschi_foreign`,
  `Internal cancellations male foreign` = `cancellati_interni_maschi_foreign`,
  `Foreign registrations male foreign` = `iscritti_estero_maschi_foreign`,
  `Foreign cancellations male foreign` = `cancellati_estero_maschi_foreign`,
  `Ending population male foreign` = `popolazione_fine_maschi_foreign`,
  `Starting population female foreign` = `popolazione_inizio_femmine_foreign`,
  `Births female foreign` = `nati_femmine_foreign`,
  `Deaths female foreign` = `morti_femmine_foreign`,
  `Internal registrations female foreign` = `iscritti_interni_femmine_foreign`,
  `Internal cancellations female foreign` = `cancellati_interni_femmine_foreign`,
  `Foreign registrations female foreign` = `iscritti_estero_femmine_foreign`,
  `Foreign cancellations female foreign` = `cancellati_estero_femmine_foreign`,
  `Ending population female foreign` = `popolazione_fine_femmine_foreign`,
  `Starting population total foreign` = `popolazione_inizio_totale_foreign`,
  `Births total foreign` = `nati_totale_foreign`,
  `Deaths total foreign` = `morti_totale_foreign`,
  `Internal registrations total foreign` = `iscritti_interni_totale_foreign`,
  `Internal cancellations total foreign` = `cancellati_interni_totale_foreign`,
  `Foreign registrations total foreign` = `iscritti_estero_totale_foreign`,
  `Foreign cancellations total foreign` = `cancellati_estero_totale_foreign`,
  `Area in square kilometers` = `area_km2`,
  `Number of segments` = `n_segments`,
  `Road kilometers total` = `road_km_total`,
  `Share of primary roads` = `share_primary`,
  `Share of secondary roads` = `share_secondary`,
  `Share of residential roads` = `share_residential`,
  `Share of paved roads` = `share_paved`,
  `Share of unpaved roads` = `share_unpaved`,
  `Average maximum speed` = `avg_maxspeed`,
  `Road density kilometers per square kilometer` = `road_density_km_km2`,
  `Intersection count` = `intersection_count`,
  `Intersection density` = `intersection_density`,
  `Female mayor` = `sindaco_donna`,
  `Graduated mayor` = `sindaco_laureato`,
  `Majority councilors with degree` = `consiglieri_maggioranza_laureati`,
  `Majority assessors with degree` = `assessori_maggioranza_laureati`,
  `Mayor age` = `eta_sindaco`,
  `Average age of councilors` = `eta_media_consiglieri`,
  `Average age of assessors` = `eta_media_assessori`,
  `Mayor under 30` = `sindaco_under_30`,
  `Mayor under 40` = `sindaco_under_40`,
  `Mayor over 50` = `sindaco_over_50`,
  `Civic list` = `lista_civica`,
  `Municipality above 15000 inhabitants` = `comune_sopra_15k_abitanti`,
  `Total funding sum` = `finanziamenti_tot_sum`,
  `Programmer dummy frequency` = `dummy_programmatore_freq`,
  `Transport and mobility frequency` = `trasporti_mobilit_freq`,
  `Environment frequency` = `ambiente_freq`,
  `Social inclusion and health frequency` = `inclusione_sociale_salute_freq`,
  `Education and training frequency` = `istruzione_formazione_freq`,
  `Business competitiveness frequency` = `competitivit_imprese_freq`,
  `Culture and tourism frequency` = `cultura_turismo_freq`,
  `Digital services frequency` = `reti_servizi_digitali_freq`,
  `Administrative capacity frequency` = `capacit_amministrativa_freq`,
  `Energy frequency` = `energia_freq`,
  `Research and innovation frequency` = `ricerca_innovazione_freq`,
  `Employment and labor frequency` = `occupazione_lavoro_freq`,
  `Transport and mobility sum` = `trasporti_mobilit_sum`,
  `Environment sum` = `ambiente_sum`,
  `Social inclusion and health sum` = `inclusione_sociale_salute_sum`,
  `Education and training sum` = `istruzione_formazione_sum`,
  `Business competitiveness sum` = `competitivit_imprese_sum`,
  `Culture and tourism sum` = `cultura_turismo_sum`,
  `Digital services sum` = `reti_servizi_digitali_sum`,
  `Administrative capacity sum` = `capacit_amministrativa_sum`,
  `Energy sum` = `energia_sum`,
  `Research and innovation sum` = `ricerca_innovazione_sum`,
  `Employment and labor sum` = `occupazione_lavoro_sum`,
  `Region code` = `cod_region`,
  `Pillar 1 Bureaucracy` = `pillar1_bur`,
  `Pillar 2 Politics` = `pillar2_pol`,
  `Pillar 3 Economy` = `pillar3_econ`,
  `Municipal Administrative Quality Index` = `maqi`,
  `Number of 5 star deluxe hotels` = `x5_stelle_e_5_stelle_lusso_5_stars_5_stars_deluxe`,
  `Number of 4 star hotels` = `x4_stelle_4_stars`,
  `Number of 3 star hotels` = `x3_stelle_3_stars`,
  `Number of 2 star hotels` = `x2_stelle_2_stars`,
  `Number of 1 star hotels` = `x1_stella_1_star`,
  `Number of hotel tourism residences` = `residenze_turistico_alberghiere_hotel_tourism_residences`,
  `Number of total hotels and similar establishments` = `totale_alberghi_total_hotels_and_similar_establishments`,
  `Number of tourist campsites and villages` = `campeggi_e_villaggi_turistici_tourist_camp_sites`,
  `Number of holiday dwellings managed entrepreneurially` = `alloggi_in_affitto_gestiti_in_forma_imprenditoriale_holiday_dwellings`,
  `Number of farmhouses` = `agriturismi_farmhouses`,
  `Number of youth hostels` = `ostelli_per_la_giovent_youth_hostels`,
  `Number of tourist dormitories` = `case_per_ferie_tourist_dormitories`,
  `Number of mountain huts` = `rifugi_alpini_mountain_hut`,
  `Number of other accommodation establishments` = `altri_esercizi_ricettivi_other_accommodation`,
  `Number of bed and breakfast establishments` = `bed_breakfast_bed_breakfast`,
  `Number of total extra hotel establishments` = `totale_esercizi_extra_alberghieri_total_other_collective_accommodation_establishments`,
  `Number of total accommodation establishments` = `totale_esercizi_ricettivi_numero`,
  `Number of total accommodation beds` = `totale_esercizi_ricettivi_letti`,
)

# NUTS processing
nuts_munic_codes_file <- file.path(base_path, "micro_dashboard/NUTS_Municipal_codes.xlsx")

nuts_shp_files <- c(
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS0.shp'),
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS1.shp'),
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS2.shp'),
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS3.shp')
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

nuts_munic_codes <- read_excel(nuts_munic_codes_file) %>% 
  standardize_names() %>% 
  rename(
    PRO_COM_T = codice_comune_alfanumerico,
    NUTS3_Code = codice_nuts3_2024
  ) %>% 
  select(PRO_COM_T, comune, NUTS3_Code) %>% 
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
      rename(comune = COMUNE) %>% 
      st_drop_geometry() %>% 
      unique(), 
    by = "comune") %>% 
  select(3:7) %>% 
  rename(PRO_COM_T = PRO_COM_T.y)

municipal_data_nuts <- left_join(municipal_data_merged, nuts_munic_codes, by = "PRO_COM_T") %>% 
  select(-`ISTAT region code`) %>% 
  rename(
    NUTS3 = NUTS3_Code,
    NUTS2 = NUTS2_Code,
    NUTS1 = NUTS1_Code,
    NUTS0 = NUTS0_Code,
  )

duplicated_codes <- municipal_data_nuts %>% 
  group_by(PRO_COM_T) %>% 
  reframe(count = n()) %>% 
  filter(count > 11)  %>% 
  select(PRO_COM_T) %>% 
  unname() %>% 
  unlist()

municipal_data_nuts_filtered <- municipal_data_nuts %>% 
  filter(PRO_COM_T %in% duplicated_codes, !is.na(year)) %>% 
  distinct(PRO_COM_T, year, .keep_all = TRUE)

municipal_data_nuts <- municipal_data_nuts %>% 
  filter(!(PRO_COM_T %in% duplicated_codes)) %>% 
  bind_rows(municipal_data_nuts_filtered)

# Sampling
if(isTRUE(draw_samples)) {
  set.seed(123)
  sampled_codes <- sample(unique(municipal_data_nuts$PRO_COM_T), 100)
  municipal_data_nuts <- municipal_data_nuts %>% filter(PRO_COM_T %in% sampled_codes)
}

municipal_data_merged <- municipal_data_nuts %>% 
  left_join(shapes_df_list[["NUTS0"]] %>% st_drop_geometry()) %>%
  left_join(shapes_df_list[["NUTS1"]] %>% st_drop_geometry()) %>%
  left_join(shapes_df_list[["NUTS2"]] %>% st_drop_geometry()) %>%
  left_join(shapes_df_list[["NUTS3"]] %>% st_drop_geometry())

# ---------------- FINAL MERGE ----------------
joined <- left_join(municipal_data_merged %>% st_drop_geometry(), 
                    PNNR_summed %>% 
                      st_drop_geometry() %>% 
                      select(-(3:11)),
                    by = intersect(names(municipal_data_merged %>% st_drop_geometry()), 
                                   names(PNNR_summed %>% st_drop_geometry() %>% select(-(3:11))))
)

joined <- left_join(joined, 
                    fuel_prices_summed %>% 
                      st_drop_geometry() %>% 
                      select(-(17:20)),
                    by = intersect(names(joined), 
                                   names(fuel_prices_summed %>% st_drop_geometry() %>% select(-(17:20))))
)

##### Merge ORBIS firms data ####
orbis_firms = read_csv("/Volumes/T7 Shield/Downloads/processed_data/MASTER_ITA_AGGREGATED_FINAL_filtered.csv")

orbis_firms <- orbis_firms %>% 
  rename(
    COMUNE = citt_latin_alphabet,
    `Total production value thousand USD (Orbis)` = `totale_valore_della_produzione_migl_usd`,
    `Number of employees (Orbis)` = `numero_dipendenti`,
    `Gross revenue thousand USD (Orbis)` = `fatturato_lordo_migl_usd`,
    `Net revenue thousand USD (Orbis)` = `fatturato_netto_migl_usd`,
    `Number of active firms (Orbis)` = `active`,
    `Number of quoted firms (Orbis)` = `quoted`,
    `Number of branch firms (Orbis)` = `branch`,
    `Number of firms with own data (Orbis)` = `owndata`,
    `Number of WOCO firms (Orbis)` = `woco`,
    `Number of unique companies (Orbis)` = `unique_companies_count`,
  ) %>% 
  select(-starts_with("nuts"))


joined <- joined %>% 
  mutate(
    COMUNE_UPPER = toupper(COMUNE)
  ) %>% 
  # st_drop_geometry() %>% 
  left_join(orbis_firms %>% rename(COMUNE_UPPER = COMUNE), by = c("COMUNE_UPPER", "year")) %>% 
  select(-COMUNE_UPPER) %>% 
  select(-contains("WOCO"))


########



joined_shp <- left_join(joined, comuni, by = intersect(names(joined), names(comuni))) %>% 
  st_as_sf()

renaming_map <- c(
  "finanziamento_pnrr"           = "PNRR Funding",
  "finanziamento_regione"        = "Regional Funding",
  "finanziamento_provincia"      = "Provincial Funding",
  "finanziamento_comune"         = "Municipal Funding",
  "finanziamento_altro_pubblico" = "Other Public Funding",
  "finanziamento_privato"        = "Private Funding",
  "finanziamento_da_reperire"    = "Funding to be Sourced",
  "finanziamento_pnc"            = "PNC Funding",
  "finanziamento_totale"         = "Total Funding",
  "finanziamento_ue_no_pnrr"     = "EU Non-PNRR Funding",
  "finanziamento_altri_fondi"    = "Other Funds",
  "prezzo_Altro"                 = "Price: Other",
  "prezzo_Benzina"               = "Price: Petrol",
  "prezzo_Gasolio"               = "Price: Diesel",
  "prezzo_GPL"                   = "Price: LPG",
  "prezzo_Metano"                = "Price: Methane",
  "covid_naz_raw"                = "COVID National Measures",
  "covid_emergenza"              = "COVID State of Emergency",
  "covid_reg"                    = "COVID Regional Measures",
  "covid_prov"                   = "COVID Provincial Measures",
  "guerra_ukr"                   = "Ukraine War",
  "fase_invasione"               = "UKR War Invasion Phase",
  "fase_crisi_energetica"        = "Energy Crisis Phase",
  "obbligo_prezzo_medio"         = "Average Price Display Obligation",
  "modifica_obbligo"             = "Obligation Amendment"
)

names(joined_shp)[names(joined_shp) %in% names(renaming_map)] <- renaming_map[names(joined_shp)[names(joined_shp) %in% names(renaming_map)]]

# saveRDS(joined_shp, file.path(base_path, "RData/Merged/municipal_data_merged_NEW.RDS"))

# Clean up environment to keep only essential object if needed (optional)
rm(list = setdiff(ls(), c("joined_shp", "shapes_df_list")))