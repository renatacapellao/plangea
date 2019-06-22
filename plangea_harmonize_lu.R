plangea_harmonize_lu = function(cfg, file_log, flag_log, verbose=T, force_comp=F){
  
  # Adding 'lu' control flag to flag_log
  flag_log$lu = F
  
  # Stores info on files to be used
  present_lu_info = file.info(dir(lu_dir, full.names = T)[dir(lu_dir) %in% cfg$landscape_features$land_use$classes_raster_names], extra_cols = F)
  
  if (is.null(file_log$lu)){ file_log$lu = present_lu_info }
  
  # Update checks
  nfiles_check = (nrow(present_lu_info) != nrow(file_log$lu))         # number of files is not the same
  ctimes_check = (prod(present_lu_info$ctime > file_log$lu$ctime)==1) # creation times are not the same
  rds_check = (!file.exists(paste0(in_dir, 'harmonize_lu')))  # resulting processed data file not found
  
  # Adding / updating 'lu' data to file_log (must be done *after* checks)
  file_log$lu = present_lu_info  
  
  if (nfiles_check | ctimes_check | rds_check | force_comp){
    # Modifies control structures to indicate lu_res will be computed
    flag_log$lu = T
    
    # Prints info on why lu_res is being computed
    if (verbose) {cat(paste0('Computing land-use results. Reason(s): \n',
                               ifelse(nfiles_check, 'different number of input files \n', ''),
                               ifelse(ctimes_check, 'newer input files \n', ''),
                               ifelse(rds_check, 'absent rds file \n', ''),
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
    
    # Computing lu_terr list (percent of the pixel in the terrestrial_index covered by each LU)
    lu_terr = lapply(lu_ras, function(x){x[terrestrial_index]})

    # Create auxiliary results list
    lu_aux = list(lu_terr = lu_terr, terrestrial_index = terrestrial_index,
                  lu_class_types = lu_class_types)
    
    pigz_save(lu_ras, file = paste0(in_dir, 'lu_ras'))
    pigz_save(lu_aux, file = paste0(in_dir, 'lu_aux'))
    
    # Updating / loading master_index
    source('plangea_harmonize_master_index.R')
    
    mi_res = plangea_harmonize_master_index(cfg=cfg, file_log=file_log,
                                            flag_log=flag_log, lu_ras=lu_ras,
                                            lu_aux=lu_aux, verbose=verbose,
                                            force_comp=force_comp)
    
    master_index = mi_res$master_index
    ub_vals = mi_res$ub_vals
    overall_area = mi_res$overall_area
    px_area = mi_res$px_area
    file_log = mi_res$file_log
    flag_log = mi_res$flag_log
    rm(mi_res)
    
    # Computing lu_vals list (percent of the pixel in the master_index covered by each LU)
    lu_vals = lapply(lu_ras, function(x){x[master_index]})
    pigz_save(lu_vals, file = paste0(in_dir, 'lu_vals'))
    
  } else { # else related to updated land-use rasters
    if (verbose) { cat('Loading auxiliary land-use data \n') }
    lu_aux = pigz_load(paste0(in_dir, 'lu_aux'))
    lu_terr = lu_aux$lu_terr
    terrestrial_index = lu_aux$terrestrial_index
    lu_class_types = lu_aux$lu_class_types
    # Loads land-use rasters (even if they didn't change, the master_index did)
    if (verbose) { cat('Loading land-use-raster data \n') }
    lu_ras = pigz_load(paste0(in_dir, 'lu_ras'))
    
    source('plangea_harmonize_master_index.R')
    
    mi_res = plangea_harmonize_master_index(cfg=cfg, file_log=file_log,
                                            flag_log=flag_log, lu_ras=lu_ras,
                                            lu_aux=lu_aux, verbose=verbose,
                                            force_comp=force_comp)
    
    master_index = mi_res$master_index
    ub_vals = mi_res$ub_vals
    overall_area = mi_res$overall_area
    px_area = mi_res$px_area
    file_log = mi_res$file_log
    flag_log = mi_res$flag_log
    rm(mi_res)
    
    if (flag_log$master == T | (!file.exists(paste0(in_dir, 'lu_vals'))) ){
      # Updates lu_vals list (percent of the pixel in the master_index covered by each LU)
      lu_vals = lapply(lu_ras, function(x){x[master_index]})
      pigz_save(lu_vals, file = paste0(in_dir, 'lu_vals'))
    } else {
      if (verbose) { cat('Loading land-use-values data \n') }
      lu_vals = pigz_load(paste0(in_dir, 'lu_vals'))
      } # end else related to updated master_index or unavailable lu_vals
  } # end else related to updated land-use rasters

  lu_res = list(lu_vals=lu_vals, lu_terr=lu_terr,
                master_index=master_index, 
                terrestrial_index = terrestrial_index,
                lu_class_types = lu_class_types,
                ub_vals = ub_vals, overall_area = overall_area,
                px_area = px_area,
                harmonize_log = file_log, update_flag = flag_log)
  
  pigz_save(lu_res, file = paste0(in_dir, 'harmonize_lu'))
  return(lu_res)
}