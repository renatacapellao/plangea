# WRAPPER FOR RUNNING PLANGEA BASED ON AN INPUT JSON CONFIG FILE ----------


plangea_wrapper = function(config_json_filename='plangea_config.json'){
  source('plangea_functions.R')
  
  # Read JSON config file
  cfg = fromJSON(config_json_filename)
  
  # Directory names ---------------------------------------------------------
  # set the directory names from which to load all the data for the optimisation
  base_dir = cfg$io$base_path                                             # root directory
  rawdata_dir = paste0(base_dir, cfg$io$rawdata_path)                     # input raw data files
  lu_dir = paste0(rawdata_dir, cfg$io$lu_path)                            # input land-use maps
  past_lu_dir = paste0(rawdata_dir, cfg$io$lu_path, cfg$io$past_lu_path)  # input past land-use maps
  er_dir = paste0(rawdata_dir, cfg$io$lu_path, cfg$io$ecoregions_path)    # input ecoregions / original LC maps
  var_dir = paste0(rawdata_dir, cfg$io$variables_path)                    # input variables maps
  spp_dir = paste0(rawdata_dir, cfg$io$species_path)                      # input species maps
  in_dir = paste0(base_dir, cfg$io$processed_path)                        # preprocessed Rdata
  out_dir = paste0(base_dir, cfg$io$output_path)                          # output directory
  
  
  # Harmonize data module ----------------------------------------------------
  source('plangea_harmonize.R')
  allvar_list = plangea_harmonize(cfg, config_json_filename = config_json_filename)
  
  
  # Scenarios module ---------------------------------------------------------
  source('plangea_scenarios.R')
  scen_list = plangea_scenarios(cfg, allvar_list)
  
  
  # Run-optimization module --------------------------------------------------
  source('plangea_call_solver.R')
  res = plangea_call_solver(cfg, allvar_list, scen_list)
  
  return(res)
}



