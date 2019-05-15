plangea_harmonize_oc = function(cfg, file_log, flag_log, master_index, lu_val_list,
                           verbose=T, force_comp=F){
  
  # Adding 'oc' control flag to flag_log
  flag_log$oc = F
  
  # Stores info on files to be used
  present_oc_info = file.info(dir(var_dir, full.names = T)[dir(var_dir) %in% cfg$variables$calc_oc$oc_files], extra_cols = F)
  
  if (is.null(file_log$oc)){file_log$oc = present_oc_info}
  
  # Update checks
  nfiles_check = (nrow(present_oc_info) != nrow(file_log$oc))         # number of files is not the same
  ctimes_check = (prod(present_oc_info$ctime > file_log$oc$ctime)==1) # creation times are not the same
  rdata_check = (!file.exists(paste0(in_dir, 'harmonize_oc.Rdata')))  # resulting processed data file not found
  ready_check = !cfg$variables$ready_variables[cfg$variables$variable_names %in% # oc is not labeled as ready
                                                 (cfg$variables$calc_oc$oc_variable_name)]
  dependencies = flag_log$master
  names_check = (length(which(dir(var_dir) %in% cfg$variables$calc_oc$oc_files)) ==
                   length(cfg$variables$calc_oc$oc_names))            # all rasters have names, and vice versa
  
  # Adding / updating 'lu' data to file_log (must be done *after* checks)
  file_log$oc = present_oc_info  
  
  if ((nfiles_check | ctimes_check | rdata_check | dependencies | force_comp) & (ready_check | names_check)){
    # Modifies control structures to indicate oc will be computed
    flag_log$oc = T
    
    # Prints info on why oc is being computed
    if (verbose) {cat(paste0('Computing opportunity costs results. Reason(s): \n',
                             ifelse(nfiles_check, 'different number of input files \n', ''),
                             ifelse(ctimes_check, 'newer input files \n', ''),
                             ifelse(rdata_check, 'absent Rdata file \n', ''),
                             ifelse(dependencies, 'dependencies changed \n', ''),
                             ifelse(force_comp, 'because you said so! \n', '')
    ))}
    
    oc_ras_names = cfg$variables$calc_oc$oc_files
    
    cost_list = lapply(paste0(var_dir, oc_ras_names),
                       function(x){load_raster(x, master_index)})
    
    names(cost_list) = cfg$variables$calc_oc$oc_names
    
    map_list = lu_val_list[names(lu_val_list) %in% cfg$variables$calc_oc$corresponding_lu_classes]
    
    oc = Reduce('+', mapply('*', cost_list, map_list, SIMPLIFY=F))
    
    save(oc, file = paste0(in_dir, 'oc.Rdata'))
  } else {
    if (verbose) {cat('Loading opportunity-cost data \n')}
    load(paste0(in_dir, 'oc.Rdata'))
  }
  
  oc_res = list(oc = oc, harmonize_log = file_log, update_flag = flag_log)
  
  save(oc_res, file = paste0(in_dir, 'harmonize_oc.Rdata'))
  return(oc_res)
}