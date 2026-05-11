# TEST

# source("/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/merge_munic_data_nuts.r")
# source("/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/LOAD_DATA_v2.r")

compute_median_by_nuts <- function(municipal_data_nuts, nuts_code, variables) {
  municipal_data_nuts %>%
    st_drop_geometry() %>% 
    group_by(
      .data[[nuts_code]],
      #NUTS2_Code, 
      year
    ) %>% 
    reframe(
      across(all_of(variables), \(x) median(x, na.rm = TRUE))
    ) %>% 
    filter(
      !is.na(.data[[nuts_code]])
    ) %>% 
    left_join(
      shapes_df_list[[nuts_code]]
    ) %>% 
    st_as_sf()
}


# Example usage 

#compute_median_by_nuts(municipal_data_merged, "NUTS3")
#compute_median_by_nuts(municipal_data_merged, "NUTS2")
#compute_median_by_nuts(municipal_data_merged, "NUTS1")
#compute_median_by_nuts(municipal_data_merged, "NUTS0")
