# WRAPPER FOR RUNNING PLANGEA BASED ON AN INPUT JSON CONFIG FILE

plangea_wrapper = function(config_json_filename='../plangea-legacy/global_config.json'){
  source('plangea_functions.R')
  
  # Read JSON config file
  config = fromJSON(config_json_filename)
  
  # Define configuration objects
  targets=as.numeric(config$scenarios$targets); names(targets) = config$scenarios$target_names
  
  # Prepare data structures for sub-region analyses if relevant
  if (config$scenarios$`sub-region_scenarios`$include_subregion_scenarios){
    sub_region_ras = raster(paste0(config$io$preprocessed_data_location_name, config$scenarios$`sub-region_scenarios`$`sub-region_folder`, config$scenarios$`sub-region_scenarios`$`sub-region_raster_name`))
    sub_region_table = read.csv(paste0(config$io$preprocessed_data_location_name, config$scenarios$`sub-region_scenarios`$`sub-region_folder`, config$scenarios$`sub-region_scenarios`$`sub-region_names_table`))
    if (!config$scenarios$`sub-region_scenarios`$`sub-region_flat_targets`){
      sub_region_targets = calc_sparable_area(read.csv(paste0(config$io$preprocessed_data_location_name, config$scenarios$`sub-region_scenarios`$`sub-region_folder`, config$scenarios$`sub-region_scenarios`$`sub-region_targets`)))
    }
  }
  

  
  
  sum.anthropic = 30000000
  targets[[which(sapply(targets, is.na))]] = sum.anthropic
}
