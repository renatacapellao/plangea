# WRAPPER FOR RUNNING PLANGEA BASED ON AN INPUT JSON CONFIG FILE ----------


plangea_wrapper = function(config_json_filename='../plangea-legacy/plangea_config.json'){
  source('plangea_functions.R')
  
  # Read JSON config file
  config = fromJSON(config_json_filename)
  
  # List with scenario targets (overall)
  targets=as.numeric(config$scenarios$targets); names(targets) = config$scenarios$target_names
  
  # Prepare data structures for sub-region analyses if relevant
  if (config$scenarios$`sub-region_scenarios`$include_subregion_scenarios){
    # Raster with sub-regions
    sr_ras = raster(paste0(config$io$preprocessed_data_location_name,
                           config$scenarios$`sub-region_scenarios`$`sub-region_folder`,
                           config$scenarios$`sub-region_scenarios`$`sub-region_raster_name`))
    
    # Table with sub-region labels for classes in sr_ras
    sr_tbl = read.csv(paste0(config$io$preprocessed_data_location_name,
                             config$scenarios$`sub-region_scenarios`$`sub-region_folder`,
                             config$scenarios$`sub-region_scenarios`$`sub-region_names_table`))
    
    # Sub-region-targets data.frame
    if (!config$scenarios$`sub-region_scenarios`$`sub-region_flat_targets`){
      sr_targets = calc_sparable_area(read.csv(paste0(config$io$preprocessed_data_location_name,
                                                      config$scenarios$`sub-region_scenarios`$`sub-region_folder`,
                                                      config$scenarios$`sub-region_scenarios`$`sub-region_targets`)))
    }
  }
  
  # List of names for Benchmark scenarios
  bm_list = lapply(config$scenarios$benchmark_scenarios,
                   function(x){paste0('scen-',paste0(x, collapse="-"))})
  
  # List of names for upper-bounds-limits scenarios
  ub_list = lapply(config$scenarios$upper_bounds_limits,
                   function(x){paste0('ublim-',x)})

  # List of number of steps in each benchmark scenario
  nsteps_list = config$scenarios$nsteps_per_benchmark
  
  #allvar.list = list(cb,bd,oc)
  #names(allvar.list) = config$variables$variable_names
  #iter.varnames = config$scenarios$benchmark_scenarios[[5]]
  #iter.ptr = names(allvar.list) %in% iter.varnames
  #var.list = allvar.list[iter.ptr]
  #type.list = config$variables$variable_types[iter.ptr]
  #iter.obj = calc_objective_function(var.list,type.list)
  
  
  sum.anthropic = 30000000
  targets[[which(sapply(targets, is.na))]] = sum.anthropic
  
  # PREPROCESSING MODULE ----------------------------------------------------
  plangea_preprocess(config)
}



