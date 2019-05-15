plangea_harmonize_subregions = function(cfg, file_log, flag_log, master_index, 
                                verbose=T, force_comp=F){
  
  # Adding 'sr' control flag to flag_log
  flag_log$sr = F
  
  # Stores info on files to be used
  present_sr_info = file.info(dir(sr_dir, full.names = T)[dir(sr_dir) %in% cfg$scenarios$`sub-region_scenarios`$`sub-region_raster_name`], extra_cols = F)
  
  if (is.null(file_log$sr)){file_log$sr = present_sr_info}
  
  # Update checks
  nfiles_check = (nrow(present_sr_info) != nrow(file_log$sr))         # number of files is not the same
  ctimes_check = (prod(present_sr_info$ctime > file_log$sr$ctime)==1) # creation times are not the same
  rdata_check = (!file.exists(paste0(in_dir, 'harmonize_sr.Rdata')))  # resulting processed data file not found
  dependencies = flag_log$master
  config_check = cfg$scenarios$`sub-region_scenarios`$include_subregion_scenarios # JSON asks to include SRs
  
  # Adding / updating 'sr' data to file_log (must be done *after* checks)
  file_log$sr = present_sr_info
  
  if ((nfiles_check | ctimes_check | rdata_check | dependencies | force_comp) & (config_check)){
    # Modifies control structures to indicate sr will be computed
    flag_log$sr = T
    
    # Prints info on why sr is being computed
    if (verbose) {cat(paste0('Computing sub-regions results. Reason(s): \n',
                             ifelse(nfiles_check, 'different number of input files \n', ''),
                             ifelse(ctimes_check, 'newer input files \n', ''),
                             ifelse(rdata_check, 'absent Rdata file \n', ''),
                             ifelse(dependencies, 'dependencies changed \n', ''),
                             ifelse(force_comp, 'because you said so! \n', '')
    ))}
    
    # Raster with sub-regions
    sr_vals = load_raster(paste0(sr_dir,
                                 cfg$scenarios$`sub-region_scenarios`$`sub-region_raster_name`),
                          master_index)
    
    # Table with sub-region labels for classes in sr_ras
    sr_tbl = read.csv(paste0(sr_dir,
                             cfg$scenarios$`sub-region_scenarios`$`sub-region_names_table`))
    
    # Coefficients for constraint equations pointing to pixels pertaining to each sub-region
    sr_coefs = matrix(t(sapply(sr_tbl$CODE, function(x){as.integer(sr_vals==x)})),
                           nrow=length(sr_tbl$CODE))
    
    # Adding entry in sR_coefs pointing to all px in master_index (to enable use of global constraints)
    sr_coefs = rbind(sr_coefs, rep(1,length(master_index)))
    
    sr_res = list(sr_vals = sr_vals, sr_tbl = sr_tbl, sr_coefs = sr_coefs,
                  harmonize_log = file_log, update_flag = flag_log)
    
    save(sr_res, file = paste0(in_dir, 'harmonize_sr.Rdata'))
  } else {
    if (verbose) {cat('Loading sub-regions data \n')}
    load(paste0(in_dir, 'harmonize_sr.Rdata'))
    sr_res$flag_log$sr = F
  }
  
  return(sr_res)
}