# TEST

# source("/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/merge_munic_data_nuts.r")
# source("/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/LOAD_DATA_v2.r")

# c("sum", "mean", "median")

aggregate_by_nuts <- function(municipal_data_nuts, nuts_code, variables, aggregation) {
  grouped_data <- municipal_data_nuts %>%
    st_drop_geometry() %>% 
    group_by(
      .data[[nuts_code]],
      year
    ) 
  
  if(aggregation == "Sum") {
    summarized_data <- grouped_data %>% 
      reframe(
        across(all_of(variables), \(x) sum(x, na.rm = TRUE))
      ) %>% 
      ungroup()
      
  } 
  
  if (aggregation == "Mean") {
    summarized_data <- grouped_data %>% 
      reframe(
        across(all_of(variables), \(x) mean(x, na.rm = TRUE))
      ) %>% 
      ungroup()
  } 
  
  if (aggregation == "Median") {
    summarized_data <- grouped_data %>% 
      reframe(
        across(all_of(variables), \(x) median(x, na.rm = TRUE))
      ) %>% 
      ungroup()
  } 
  
  summarized_data %>% 
    filter(
      !is.na(.data[[nuts_code]])
    ) %>% 
      left_join(
        shapes_df_list[[nuts_code]]
      ) %>% 
      st_as_sf()
}


# Example usage 

# VARIABLES_CHOICES <- names(municipal_data_merged)[14:164]
# aggregate_by_nuts(municipal_data_merged, "NUTS2", VARIABLES_CHOICES, "Sum")
