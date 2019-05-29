plangea_refresh_vars = function(cfg, upper_env, verbose=T){
  # Names of refreshable variables
  refresh_names = cfg$variables$variable_names[!cfg$variables$ready_variables]
  
  # Removing from refresh_vars names related to OC and BD variables
  oc_name = cfg$variables$calc_oc$oc_variable_name
  bd_name = cfg$variables$calc_bd$bd_variable_name
  refresh_names = refresh_vars[!(refresh_vars %in% c(oc_name, bd_name))]
  
  # Proceed to refresh BD variable if this function is being called from within
  # the plangea_scenario loop (iter_res exists)
  if (exists('iter_res_cum')){
    hab_iter_areas = update_hab(hab_now_areas, restored_area = iter_res_cum,
                                prop_restore, usphab_proc,
                                usphab_index, species_index_list_proc)
    bd = calc_bd(slp = calc_extinction_slope(hab_iter_areas, hab_pot_areas),
                 prop_restore, usphab_proc, usphab_index,
                 species_index_list_proc)
  }
  
  # source scripts to compute refreshable variables
  refresh_scripts = cfg$variables$refresh_variables_script_names
  
  # Updates refreshed variables
  allvar_list$bd = bd
  
  return(allvar_list)
}