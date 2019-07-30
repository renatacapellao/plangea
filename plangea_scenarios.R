plangea_scenarios = function(cfg, in_data, verbose=T){
  
  # Unpacking in_data variables
  for (i in 1:length(in_data)) {assign(names(in_data)[i], in_data[[i]])}
  
  # Variables scaling factor ---------------------------------------------------
  # (global values were cb 1e-3, bd 1e2, oc 1e-4)
  sf_list = as.numeric(cfg$variables$variable_scaling_factor)
  names(sf_list) = cfg$variables$variable_names
  
  # Names of variables with empty scaling factors (to be computed from max vals)
  auto_sf_vars = cfg$variables$variable_names[which(sapply(sf_list, is.na))]
  
  # Computing scaling factors of variables in auto_sf_vars
  auto_sf_vals = lapply(allvar_list[auto_sf_vars], function(x){(10^(1.5)) / max(x, na.rm=T)})
  
  # Filling sf_list with auto_sf_vals
  sf_list[auto_sf_vars] = auto_sf_vals
  
  
  # List with scenario targets (overall) ---------------------------------------
  targets = as.numeric(cfg$scenarios$target_areas_sqkm)
  names(targets) = cfg$scenarios$target_names
  targets = as.list(targets)
  
  # Filling empty targets with total area of interest
  targets[[which(sapply(targets, is.na))]] = sum(ub_vals * px_area)
  
  # Area scaling factor (global value was 1e4)
  g_scalar_area = as.numeric(cfg$landscape_features$area_scaling_factor)
  
  # Filling up g_scalar_area if value is missing in config json
  if(is.na(g_scalar_area)) {g_scalar_area = (10^(3.5)) / max(unlist(targets))}
   
  # Trade-off limits -----------------------------------------------------------
  # Limits to overall targets for trade-off curves
  tradeoff_lims = cfg$scenarios$tradeoff_curve_percent_step_size *
    cfg$scenarios$tradeoff_curve_percent_nstep_range[1]:cfg$scenarios$tradeoff_curve_percent_nstep_range[2]
  
  # List of names for Benchmark scenarios
  bm_list = lapply(cfg$scenarios$benchmark_scenarios,
                   function(x){paste0('scen-',paste0(x, collapse="-"))})
  
  # List of names for upper-bounds-limits scenarios
  ublim_list = lapply(cfg$scenarios$upper_bounds_limits,
                      function(x){paste0('ublim-',x)})
  
  # Limits to overall targets for building the trade-off curves
  tradeoff_lims = cfg$scenarios$tradeoff_curve_percent_step_size *
    cfg$scenarios$tradeoff_curve_percent_nstep_range[1]:
    cfg$scenarios$tradeoff_curve_percent_nstep_range[2]
  
  # Limits to overall targets for purpose of refreshing variables
  refresh_stepsizes = 1 / cfg$scenarios$refresh_nsteps_per_benchmark

  
  source('plangea_process_solver.R')
  source('plangea_refresh_vars.R')
  
  start_time = Sys.time()
  
  for (iter_target_name in cfg$scenarios$target_names) {
    # Actual value for the iteration's overall target from iteration's target name
    iter_cfg_targets = as.numeric(targets[cfg$scenarios$target_names == iter_target_name])
    
    for (iter_ublim in cfg$scenarios$upper_bounds_limits) {
      # Result file suffix for ublim loop
      iter_ublim_prt = paste0('_ublim_', iter_ublim)
      
      for (iter_scencode in 1:length(cfg$scenarios$benchmark_scenarios)) {
        # Names of variables included in the iteration's benchmark scenario
        iter_varnames = unlist(cfg$scenarios$benchmark_scenarios[iter_scencode])
        # Result file suffix for scenarios loop
        iter_scen_prt = paste0('_scen_', paste(iter_varnames, collapse = '-'))
        iter_refresh_stepsize = refresh_stepsizes[iter_scencode]
        iter_refresh_points = seq(from=iter_refresh_stepsize, to=1, by=iter_refresh_stepsize)
        iter_ub = ub_vals * iter_ublim
        # Pointer for variables to be included in the iteration's objective calculation
        iter_ptr = names(allvar_list) %in% iter_varnames
        # List of the types of the variables included
        type_list = cfg$variables$variable_types[iter_ptr]
        # List of the scaling factors for the variables included
        iter_sf = as.numeric(sf_list[iter_ptr])
        # List of all combinations of weights for the variables included
        wgt_list = expand.grid(cfg$variables$variable_weights[iter_ptr])
        
        for (wgt_row in 1:nrow(wgt_list)) {
          iter_wgt = wgt_list[wgt_row,]
          var_list = mapply('*', iter_wgt * iter_sf, allvar_list[iter_ptr], SIMPLIFY = F)
          
          # Result file suffix for weight loop
          iter_wgt_prt = paste0('_wgts_', paste(iter_wgt, collapse = '-'))
          
          # Computes the objective function for the variables and weights of the iteration
          iter_obj = calc_objective_function(var_list, type_list)
          
          for (iter_tradeoff in tradeoff_lims) {
            # Result file suffix for tradeoff curve loop
            iter_tradeoff_prt = paste0('_tradeofflim_', iter_tradeoff)
            if (cfg$scenarios$`sub-region_scenarios`$include_subregion_scenarios){
              # Substituting total available area in sr_targets for iter_target
              sr_targets$total[nrow(sr_targets)] = iter_cfg_targets
              # Resizing targets per sub-region using the iteration's iter_tradeoff
              iter_targets_tradeoff = sr_targets$total * iter_tradeoff
              # For sub-regions, the problem matrix is given by sr_coefs
              problem_matrix = sr_coefs * px_area
            } else { #that is, include subregion flag in the config json is false
              iter_targets_tradeoff = iter_cfg_targets * iter_tradeoff
              # in this case, the problem matrix is trivial
              #problem_matrix = matrix(rep(1, length(master_index)), nrow=1)
              problem_matrix = matrix(px_area, nrow=1)
            }
            
            # Resets object storing the cumulative area restored in each refresh-step
            iter_res_cum = rep(0, length(master_index))
            
            for (iter_refresh_lim in iter_refresh_points) {
              # Actual target to be sent to the solver, resized to a refresh-step
              iter_targets = iter_targets_tradeoff * iter_refresh_lim
              
              # Result file suffix for refresh loop
              iter_refresh_prt = paste0('_refresh-step_',
                                        which(iter_refresh_points==iter_refresh_lim))
              # Name to save the resulting file for the solver call of the iteration
              iter_filename = paste0(iter_target_name, iter_ublim_prt, iter_scen_prt,
                           iter_wgt_prt, iter_tradeoff_prt, iter_refresh_prt)
              if (verbose) {print(iter_filename)}
              
              iter_start = Sys.time()
              
              return(mget(objects()))
                            
              iter_res = plangea_process_solver(obj = iter_obj,
                                                mat = problem_matrix,
                                                rhs = iter_targets,
                                                ub = iter_ub,
                                                iter_filename = iter_filename)
              if (verbose){cat(paste('Solver time:', round(Sys.time() - iter_start, digits=2),
                                       '| Total time elapsed:', round(Sys.time() - start_time, digits=2),
                                       '\n'))}
              
              pigz_save(iter_res, file = paste0(run_dir, iter_filename))

              # Updates upper bounds to remove selected areas
              iter_ub = iter_ub - iter_res
              
              # Updates object storing the cumulative area restored in each refresh-step
              iter_res_cum = iter_res_cum + iter_res

              #if (verbose){#print(hist(iter_res[iter_res>0]))
                #print(sum(iter_res * px_area))
                #print(spplot_vals(iter_res_cum, base_ras = bg, master_index))
                #}
                            
              # Refreshes variables (if the iteration's scenario have refresh steps)
              if (length(iter_refresh_points) > 1) {
                refreshed_vars = plangea_refresh_vars(cfg,
                                                      upper_env = mget(objects()),
                                                      verbose=verbose)
                # Refreshed varlist
                refreshed_varlist = mapply('*', iter_wgt * iter_sf, refreshed_vars[iter_ptr], SIMPLIFY = F)
                
                # Recomputes objective function
                iter_obj = calc_objective_function(var_list = refreshed_varlist, type_list)
                }
              
            } # end of iter_refresh lim loop
          } # end of iter_tradeoff loop
        } # end of wgt_row loop
      } # end of iter_scencode (benchmark scenarios) loop
    } # end of iter_ublim loop
  } # end of iter_targets loop 
  
  
} # end of plangea_scenarios function
