plangea_harmonize_lu = function(cfg, file_log, flag_log, verbose=T, force_comp=F){
  
  # Adding 'lu' control flag to flag_log
  flag_log$lu = F
  
  # Stores info on files to be used
  present_lu_info = file.info(dir(lu_dir, full.names = T)[dir(lu_dir) %in% cfg$landscape_features$land_use$classes_raster_names], extra_cols = F)
  
  if (is.null(file_log$lu)){file_log$lu = present_lu_info}
  
  # Update checks
  nfiles_check = (nrow(present_lu_info) != nrow(file_log$lu))         # number of files is not the same
  ctimes_check = (prod(present_lu_info$ctime > file_log$lu$ctime)==1) # creation times are not the same
  rdata_check = (!file.exists(paste0(in_dir, 'harmonize_lu.Rdata')))  # resulting processed data file not found
  
  # Adding / updating 'lu' data to file_log (must be done *after* checks)
  file_log$lu = present_lu_info  
  
  if (nfiles_check | ctimes_check | rdata_check | force_comp){
    # Modifies control structures to indicate lu_res will be computed
    flag_log$lu = T
    
    # Prints info on why lu_res is being computed
    if (verbose) {cat(paste0('Computing land-use results. Reason(s): \n',
                               ifelse(nfiles_check, 'different number of input files \n', ''),
                               ifelse(ctimes_check, 'newer input files \n', ''),
                               ifelse(rdata_check, 'absent Rdata file \n', ''),
                               ifelse(force_comp, 'because you said so! \n', '')
                               ))}
    
    # Load land-use rasters names
    lu_ras_names = dir(lu_dir)[dir(lu_dir) %in% cfg$landscape_features$land_use$classes_raster_names]
    
    # Re-ordering to ensure the object created follows order in config file
    lu_ras_names = lu_ras_names[match(cfg$landscape_features$land_use$classes_raster_names, lu_ras_names)]
    
    # Creating raster stack with rasters corresponding to LU names
    #lu_ras = stack(paste0(lu_dir, lu_ras_names))
    lu_ras = lapply(paste0(lu_dir, lu_ras_names), function(x){load_raster(x)})
    names(lu_ras) = cfg$landscape_features$land_use$class_names
    
    # Load land-use class types
    # Possible types of LU: "N"atural, "A"nthropic, "I"gnore
    lu_class_types = cfg$landscape_features$land_use$class_types  
    
    # Building raster with terrestrial areas
    terrestrial_areas = Reduce('+', lu_ras[lu_class_types %in% c("N", "A")])
    
    # Builds index with terrestrial pixels
    terrestrial_index = which(values(terrestrial_areas > 0))
    
    # Create auxiliary results list
    lu_aux = list(terrestrial_index = terrestrial_index,
                  lu_class_types = lu_class_types)
    
    save(lu_ras, file = paste0(in_dir, 'lu_ras.Rdata'))
    save(lu_aux, file = paste0(in_dir, 'lu_aux.Rdata'))
    
    # Updating / loading master_index
    source('plangea_harmonize_master_index.R')
    
    master_index = plangea_harmonize_master_index(cfg, file_log, flag_log,
                                                  lu_ras, lu_aux, force_comp)
    
    file_log = master_index[[2]]
    lag_log = master_index[[3]]
    master_index = master_index[[1]]
    
    # Computing lu_vals list (percent of the pixel in the master_index covered by each LU)
    lu_vals = lapply(lu_ras, function(x){x[master_index]})
    save(lu_vals, file = paste0(in_dir, 'lu_vals.Rdata'))
    
  } else { # else related to updated land-use rasters
    load(paste0(in_dir, 'lu_aux.Rdata'))
    terrestrial_index = lu_aux$terrestrial_index
    lu_class_types = lu_aux$lu_class_types
    
    source('plangea_harmonize_master_index.R')
    
    master_index = plangea_harmonize_master_index(cfg, file_log, flag_log,
                                                  lu_ras, lu_aux, force_comp)
    
    file_log = master_index[[2]]
    flag_log = master_index[[3]]
    master_index = master_index[[1]]
    
    if (flag_log$master == T | (!file.exists(paste0(in_dir, 'lu_vals.Rdata'))) ){
      # Loads land-use rasters (even if they didn't change, the master_index did)
      load(paste0(in_dir, 'lu_ras.Rdata'))
      
      # Updates lu_vals list (percent of the pixel in the master_index covered by each LU)
      lu_vals = lapply(lu_ras, function(x){x[master_index]})
      save(lu_vals, file = paste0(in_dir, 'lu_vals.Rdata'))
    } else {
      load(paste0(in_dir, 'lu_vals.Rdata'))
      } # end else related to updated master_index or unavailable lu_vals.Rdata
  } # end else related to updated land-use rasters

  lu_res = list(master_index=master_index, lu_vals=lu_vals,
                terrestrial_index = terrestrial_index,
                lu_class_types = lu_class_types,
                harmonize_log = file_log, update_flag = flag_log)
  
  save(lu_res, file = paste0(in_dir, 'harmonize_lu.Rdata'))
  return(lu_res)
}