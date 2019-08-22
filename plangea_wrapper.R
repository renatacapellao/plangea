# WRAPPER FOR RUNNING PLANGEA BASED ON AN INPUT JSON CONFIG FILE ----------


plangea_wrapper = function(config_json_filename='./example-global/plangea_config.json'){
  source('plangea_functions.R')
  source('plangea_refresh_vars.R')
  
  # Read JSON config file
  cfg = fromJSON(config_json_filename)
  
  # Directory names ---------------------------------------------------------
  # set the directory names from which to load all the data for the optimisation
  base_dir = cfg$io$base_path                                             # root directory
  rawdata_dir = paste0(cfg$io$rawdata_path)                     # input raw data files
  lu_dir = paste0(rawdata_dir, cfg$io$lu_relative_path)                            # input land-use maps
  past_lu_dir = paste0(rawdata_dir, cfg$io$past_lu_relative_path)  # input past land-use maps
  er_dir = paste0(rawdata_dir, cfg$io$ecoregions_relative_path)    # input ecoregions / original LC maps
  var_dir = paste0(rawdata_dir, cfg$io$variables_relative_path)                    # input variables maps
  spp_dir = paste0(rawdata_dir, cfg$io$species_relative_path)                      # input species maps
  sr_dir = paste0(rawdata_dir, cfg$scenarios$`sub-region_scenarios`$`sub-region_relative_path`) # sub-regions data
  in_dir = paste0(base_dir, cfg$io$processed_relative_path)                        # preprocessed Rdata
  out_dir = paste0(base_dir, cfg$io$output_relative_path)                          # root directory for all output files
  res_dir = paste0(base_dir, cfg$io$solver_results_relative_path)                   # directory for all solver runs
  proc_dir = paste0(base_dir, cfg$io$post_processed_relative_path)                  # directory for all pre-processed files
  run_dir = paste0(res_dir, Sys.Date(), '/')                              # directory for current solver runs
  scripts_dir = paste0(base_dir, cfg$io$extra_scripts_relative_path)      # directory for all extra scripts included
  
  if (!dir.exists(base_dir)) dir.create(base_dir)
  if (!dir.exists(rawdata_dir)) dir.create(rawdata_dir)
  if (!dir.exists(lu_dir)) dir.create(lu_dir)
  if (!dir.exists(past_lu_dir)) dir.create(past_lu_dir)
  if (!dir.exists(er_dir)) dir.create(er_dir)
  if (!dir.exists(var_dir)) dir.create(var_dir)
  if (!dir.exists(spp_dir)) dir.create(spp_dir)
  if (!dir.exists(sr_dir)) dir.create(sr_dir)
  if (!dir.exists(in_dir)) dir.create(in_dir)
  if (!dir.exists(out_dir)) dir.create(out_dir)
  if (!dir.exists(res_dir)) dir.create(res_dir)
  if (!dir.exists(proc_dir)) dir.create(proc_dir)
  if (!dir.exists(run_dir)) dir.create(run_dir)

  
  # Harmonize data module ------------------------------------------------------
  source('plangea_harmonize.R')
  in_data = plangea_harmonize(cfg, config_json_filename = config_json_filename,
                              verbose=T, force_comp = T)
  
  
  # Process module -------------------------------------------------------------
  
  #in_data = pigz_load('./example-global/processed/harmonize_full_envir')
  #lu_ras = pigz_load('./example-global/processed/lu_ras')
  #bg = lu_ras[[1]] / lu_ras[[1]]
  #bg[bg==1] = 0
  source('plangea_scenarios.R')
  scen_list = plangea_scenarios(cfg=cfg, in_data)
  
  
  # Post-process module --------------------------------------------------------

  
  return(res)
}



