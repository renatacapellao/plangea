# WRAPPER FOR RUNNING PLANGEA BASED ON AN INPUT JSON CONFIG FILE ----------


plangea_wrapper = function(config_json_filename='plangea_config.json'){
  source('plangea_functions.R')
  
  # Read JSON config file
  cfg = fromJSON(config_json_filename)
  
  # Directory names ---------------------------------------------------------
  # set the directory names from which to load all the data for the optimisation
  base.dir = cfg$io$base_path
  rawdata.dir = paste0(base.dir, cfg$io$rawdata_path)
  lu.dir = paste0(rawdata.dir, cfg$io$lu_path)
  past.lu.dir = paste0(rawdata.dir, cfg$io$lu_path, cfg$io$past_lu_path)
  er.dir = paste0(rawdata.dir, cfg$io$lu_path, cfg$io$ecoregions_path)
  var.dir = paste0(rawdata.dir, cfg$io$variables_path)
  spp.dir = paste0(rawdata.dir, cfg$io$species_path)
  in.dir = paste0(base.dir, cfg$io$preprocessed_path)
  out.dir = paste0(base.dir, cfg$io$output_path)
  
  
  # Harmonize data module ----------------------------------------------------
  source('plangea_harmonize.R')
  allvar.list = plangea_harmonize(cfg)
  
  
  # Scenarios module ---------------------------------------------------------
  source('plangea_scenarios.R')
  scen.list = plangea_scenarios(cfg, allvar.list)
  
  
  # Run-optimization module --------------------------------------------------
  source('plangea_call_solver.R')
  res = plangea_call_solver(cfg, allvar.list, scen.list)
  
  return(res)
}



