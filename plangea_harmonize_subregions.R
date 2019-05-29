plangea_harmonize_subregions = function(cfg, file_log, flag_log, master_index,
                                        ub_vals, verbose=T, force_comp=F){
  
  # Adding 'sr' control flag to flag_log
  flag_log$sr = F
  
  # Stores info on files to be used
  present_sr_info = file.info(dir(sr_dir, full.names = T)[dir(sr_dir) %in% cfg$scenarios$`sub-region_scenarios`$`sub-region_raster_name`], extra_cols = F)
  
  if (is.null(file_log$sr)){file_log$sr = present_sr_info}
  
  # Update checks
  nfiles_check = (nrow(present_sr_info) != nrow(file_log$sr))         # number of files is not the same
  ctimes_check = (prod(present_sr_info$ctime > file_log$sr$ctime)==1) # creation times are not the same
  rds_check = (!file.exists(paste0(in_dir, 'harmonize_sr')))  # resulting processed data file not found
  dependencies = (flag_log$master) |
    (file_log$start$scenarios$`sub-region_scenarios`$`sub-region_flat_targets` !=
       cfg$scenarios$`sub-region_scenarios`$`sub-region_flat_targets`) 

  # Adding / updating 'sr' data to file_log (must be done *after* checks)
  file_log$sr = present_sr_info
  
  if (nfiles_check | ctimes_check | rds_check | dependencies | force_comp){
    # Modifies control structures to indicate sr will be computed
    flag_log$sr = T
    
    # Prints info on why sr is being computed
    if (verbose) {cat(paste0('Computing sub-regions results. Reason(s): \n',
                             ifelse(nfiles_check, 'different number of input files \n', ''),
                             ifelse(ctimes_check, 'newer input files \n', ''),
                             ifelse(rds_check, 'absent rds file \n', ''),
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

    # Adding entry in SR_coefs pointing to all px in master_index (to enable use of global constraints)
    sr_coefs = rbind(sr_coefs, rep(1,length(master_index)))
        
    # Sub-region-targets data.frame
    if (cfg$scenarios$`sub-region_scenarios`$`sub-region_flat_targets`){
      sr_targets = rowSums(sr_coefs * ub_vals)
    } else {
      sr_targets = calc_sparable_area(read.csv(paste0(rawdata_dir,
                                                      cfg$scenarios$`sub-region_scenarios`$`sub-region_folder`,
                                                      cfg$scenarios$`sub-region_scenarios`$`sub-region_targets`)))
    }
    

    sr_aux = list(sr_vals = sr_vals, sr_tbl = sr_tbl,
                  sr_coefs = sr_coefs, sr_targets = sr_targets)
    pigz_save(sr_aux, file = paste0(in_dir, 'sr_aux'))
    
  } else {
      if (verbose) {cat('Loading sub-regions data \n')}
      sr_aux = pigz_load(paste0(in_dir, 'sr_aux'))
      sr_vals = sr_aux$sr_vals
      sr_tbl = sr_aux$sr_tbl
      sr_coefs = sr_aux$sr_coefs
      sr_targets = sr_aux$sr_targets
  }
  
  sr_res = list(sr_vals = sr_vals, sr_tbl = sr_tbl, sr_coefs = sr_coefs,
                sr_targets = sr_targets,
                harmonize_log = file_log, update_flag = flag_log)
  
  pigz_save(sr_res, file = paste0(in_dir, 'harmonize_sr'))
  
  return(sr_res)
}