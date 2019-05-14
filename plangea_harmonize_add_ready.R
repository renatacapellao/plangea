plangea_harmonize_add_ready = function(cfg, file_log, flag_log, master_index,
                                       verbose=T, force_comp=F){
  
  # Adding 'ready' control flag to flag_log
  flag_log$ready_var = F
  
  # Loading variables raster names
  var_ras_names = cfg$variables$ready_variables_raster_names
  
  # Subsetting variable raster by ready variables
  var_ready_names = var_ras_names[cfg$variables$ready_variables]
  
  # Stores info on files to be used
  present_var_info = file.info(dir(var_dir, full.names = T)[dir(var_dir) %in% var_ready_names], extra_cols = F)
  
  if (is.null(file_log$ready_var)){file_log$ready_var = present_var_info}
  
  # Update checks
  nfiles_check = (nrow(present_var_info) != nrow(file_log$ready_var))      # number of files is not the same
  ctimes_check = (prod(present_var_info$ctime > file_log$ready_var$ctime)==1)    # creation times not the same
  rdata_check = (!file.exists(paste0(in_dir, 'var_vals.Rdata')))  # resulting processed data file not found

  file_log$ready_var = file.info(dir(var_dir, full.names = T)[dir(var_dir) %in% var_ready_names], extra_cols = F)
  
  if (nfiles_check | ctimes_check | rdata_check | force_comp){
    # Modifies control structures to indicate ready variables will be computed
    flag_log$ready_var = T
    
    # Prints info on why lu_res is being computed
    if (verbose) {cat(paste0('Updating ready variables results. Reason(s): \n',
                             ifelse(nfiles_check, 'different number of input files \n', ''),
                             ifelse(ctimes_check, 'newer input files \n', ''),
                             ifelse(rdata_check, 'absent Rdata file \n', ''),
                             ifelse(force_comp, 'because you said so! \n', '')
    ))}
    
    var_vals = lapply(paste0(var_dir, var_ready_names),
                      function(x){load_raster(x, master_index)})
    names(var_vals) = cfg$variables$variable_names[cfg$variables$ready_variables]
    
    save(var_vals, file = paste0(in_dir, 'var_vals.Rdata'))
  } else {
    if (verbose) {cat('Loading ready-variables-values data \n')}
    load(file = paste0(in_dir, 'var_vals.Rdata'))
  }
  
  # Build list of all variables (allvar_list) ----------------------------------
  allvar_list = as.list(cfg$variables$variable_names)
  names(allvar_list) = allvar_list
  
  # Include in allvar list the variables already loaded
  allvar_list[names(allvar_list) %in% names(var_vals)] = var_vals
  
  return(list(allvar_list = allvar_list, harmonize_log = file_log, update_flag = flag_log))
}