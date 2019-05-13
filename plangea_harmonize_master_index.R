plangea_harmonize_master_index = function(cfg, file_log, flag_log, lu_ras, lu_aux,
                                          verbose=T, force_comp=F){
  
  # Adding 'master' control flag to flag_log
  flag_log$master = F
  
  # Update checks
  type_check = (cfg$scenarios$problem_type != file_log$start$scenarios$problem_type) # problem type changed
  lu_check = flag_log$lu                                                     # land-use files were updated
  rdata_check = (!file.exists(paste0(in_dir, 'master_index.Rdata')))         # resulting data file not found
  
  if (type_check | lu_check | rdata_check){
    # Modifies control structures to indicate master_index will be computed
    flag_log$master = T
    
    # Prints info on why master_index is being computed
    if (verbose) {cat(paste0('Computing master_index results. Reason(s): \n',
                             ifelse(type_check, 'different type of problem \n', ''),
                             ifelse(rdata_check, 'land-use variables were changed \n', ''),
                             ifelse(force_comp, 'because you said so! \n', '')
    ))}
    
    
    # Unpacking needed quantities from lu_res
    lu_class_types = lu_aux$lu_class_types
    
    # Loading type of optimisation problem
    # Possible types of optimisation: "C"onservation, "R"estoration
    optim_type = cfg$scenarios$problem_type
    
    # Building list relating type of LU to type of optimisation
    lu_to_optim = lu_class_types
    lu_to_optim[lu_to_optim == 'N'] = 'C'
    lu_to_optim[lu_to_optim == 'A'] = 'R'
    
    # Builds raster of interest areas
    interest_areas = Reduce('+', lu_ras[lu_to_optim %in% optim_type])
    
    # Building master_index of pixels of interest
    master_index = which(values(interest_areas > 0))
    
    save(master_index, file = paste0(in_dir, 'master_index.Rdata'))  
  } else {
    load(paste0(in_dir, 'master_index.Rdata'))
}
  return(list(master_index, file_log, flag_log))
}