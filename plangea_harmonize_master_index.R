plangea_harmonize_master_index = function(cfg, file_log, flag_log, lu_ras, lu_aux,
                                          verbose=T, force_comp=F){
  
  # Adding 'master' control flag to flag_log
  flag_log$master = F
  
  # Update checks
  type_check = (cfg$scenarios$problem_type != file_log$start$scenarios$problem_type) # problem type changed
  lu_check = flag_log$lu                                                     # land-use files were updated
  rdata_check = (!file.exists(paste0(in_dir, 'master_index.Rdata')))         # resulting data file not found
  
  if (type_check | lu_check | rdata_check | force_comp){
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
    
    # Computing overall area of interest
    overall_area = sum(interest_areas[master_index])
    
    # Computing pixel area
    px_area = area(lu_ras[[1]])[master_index]
    
    mi_aux = list(overall_area = overall_area, px_area = px_area)
    
    save(master_index, file = paste0(in_dir, 'master_index.Rdata'))  
    save(mi_aux, file = paste0(in_dir, 'mi_aux.Rdata'))
  } else {
    if (verbose) {cat('Loading master index data \n')}
    load(paste0(in_dir, 'mi_aux.Rdata'))
    overall_area = mi_aux$overall_area
    px_area = mi_aux$px_area
}
  return(list(master_index = master_index, overall_area = overall_area,
              px_area = px_area, file_log = file_log, flag_log = flag_log))
}