plangea_scenarios = function(cfg, in_data){
  # List with scenario targets (overall)
  targets=as.numeric(cfg$scenarios$targets)
  names(targets) = cfg$scenarios$target_names
  targets = as.list(targets)

  # Filling empty targets with total area of interest
  targets[[which(sapply(targets, is.na))]] = in_data$overall_area
    
  # Limits to overall targets for trade-off curves
  tradeoff_lims = cfg$scenarios$tradeoff_curve_percent_step_size *
    cfg$scenarios$tradeoff_curve_percent_nstep_range[1]:cfg$scenarios$tradeoff_curve_percent_nstep_range[2]
  
  # Sub-region-targets data.frame
  if (!cfg$scenarios$`sub-region_scenarios`$`sub-region_flat_targets`){
    sr_targets = calc_sparable_area(read.csv(paste0(cfg$io$rawdata_path,
                                                    cfg$scenarios$`sub-region_scenarios`$`sub-region_folder`,
                                                    cfg$scenarios$`sub-region_scenarios`$`sub-region_targets`)))
  }
  
  
  
  # List of names for Benchmark scenarios
  bm_list = lapply(cfg$scenarios$benchmark_scenarios,
                   function(x){paste0('scen-',paste0(x, collapse="-"))})
  
  # List of names for upper-bounds-limits scenarios
  ublim_list = lapply(cfg$scenarios$upper_bounds_limits,
                   function(x){paste0('ublim-',x)})
  
  # List of number of steps in each benchmark scenario
  nsteps_list = cfg$scenarios$nsteps_per_benchmark
  
  # Code to be used after preprocessing is done - the lines below takes a
  # named list of variables output by the preprocessing module -- already in
  # vector format (wrt to a master_index), and named according to
  # $variables_names -- and computes the objective function

   for (i in 1:length(cfg$scenarios$benchmark_scenarios)){
     iter_varnames = cfg$scenarios$benchmark_scenarios[[i]]
     iter_ptr = names(in_data$allvar_list) %in% iter_varnames
     var_list = in_data$allvar_list[iter_ptr]
     type_list = cfg$variables$variable_types[iter_ptr]
     iter_obj = calc_objective_function(var_list,type_list)
     
     # iter_res = plangea_scenarios_solver(cfg, iter_obj, ...)
   }


} # end of plangea_scenarios function
