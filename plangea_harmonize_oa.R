plangea_harmonize_oa =  function(cfg, file_log, flag_log, c_lu_maps, lu_types,
                                 tolerance=1.e-7, verbose=T, force_comp=F){
  # c_lu_maps: current land-use list of rasters or indexed vectors
  # lu_types: for each lu in the maps above, a single letter:
  #  "N" for natural, "A" for anthropic, "I" for ignore
  
  # Adding 'oa' control flag to flag_log
  flag_log$oa = F
  
  # Past land-use --------------------------------------------------------------
  # Stores info on files to be used
  current_past_lu_info = file.info(dir(past_lu_dir, full.names = T)[dir(past_lu_dir) %in% cfg$landscape_features$original_areas$past_raster_names], extra_cols = F)
  
  if (is.null(file_log$past_lu)){file_log$past_lu = current_past_lu_info}
  
  # Update checks
  nfiles_check = (nrow(current_past_lu_info) != nrow(file_log$past_lu))         # number of files is not the same
  ctimes_check = (prod(current_past_lu_info$ctime > file_log$past_lu$ctime)==1) # creation times are not the same
  rdata_check = (!file.exists(paste0(in_dir, 'past_lu.Rdata')))                 # resulting processed data file not found
  config_check = cfg$landscape_features$original_areas$include_past             # Json asks to include past LU
  
  # Adding / updating 'past lu' data to file_log (must be done *after* checks)
  file_log$past_lu = current_past_lu_info  
  
  if ((nfiles_check | ctimes_check | rdata_check | force_comp) & (config_check)){
    # Modifies control structures to indicate lu_res will be computed
    flag_log$oa = T
    
    # Prints info on why lu_res is being computed
    if (verbose) {cat(paste0('Computing past land-use results. Reason(s): \n',
                             ifelse(nfiles_check, 'different number of input files \n', ''),
                             ifelse(ctimes_check, 'newer input files \n', ''),
                             ifelse(rdata_check, 'absent Rdata file \n', ''),
                             ifelse(force_comp, 'because you said so! \n', '')
    ))}
  
  # Reading past land-use maps, if provided. If past LU is not available, assume
  # past distribution is the same as current one
    past_names = cfg$landscape_features$original_areas$past_raster_names
    p_lu_maps = lapply(paste0(past_lu_dir, past_names), function(x){load_raster(x, master_index)})
    names(p_lu_maps) = cfg$landscape_features$original_areas$past_class_names
    save(p_lu_maps, file = paste0(in_dir, 'past_lu.Rdata'))
  } else {
    if (config_check){
      if (verbose) {cat('Loading past-land-use data \n')}
      load(paste0(in_dir, 'past_lu.Rdata'))}
    else {
      p_lu_maps = c_lu_maps[lu_types == "N"]}
    }
  
  # Making sure p_lu_maps are ordered the same way as c_lu_maps[lu_types=="N"]
  p_lu_maps = p_lu_maps[match(names(c_lu_maps)[lu_types=="N"], names(p_lu_maps))]
  
  
  # Ecoregions -----------------------------------------------------------------
  # Stores info on files to be used
  current_er_info = file.info(dir(er_dir, full.names = T)[dir(er_dir) %in% cfg$landscape_features$original_areas$ecoregions_raster_names], extra_cols = F)
  
  if (is.null(file_log$er)){file_log$er = current_er_info}
  
  # Update checks
  nfiles_check = (nrow(current_er_info) != nrow(file_log$er))         # number of files is not the same
  ctimes_check = (prod(current_er_info$ctime > file_log$er$ctime)==1) # creation times are not the same
  rdata_check = (!file.exists(paste0(in_dir, 'er.Rdata')))            # resulting processed data file not found
  config_check = cfg$landscape_features$original_areas$include_ecoregions       # Json asks to include ER
  
  # Adding / updating 'ER' data to file_log (must be done *after* checks)
  file_log$er = current_er_info  
  
  if ((nfiles_check | ctimes_check | rdata_check | force_comp) & (config_check)){
    # Modifies control structures to indicate lu_res will be computed
    flag_log$oa = T
    
    # Prints info on why lu_res is being computed
    if (verbose) {cat(paste0('Computing ecoregions results. Reason(s): \n',
                             ifelse(nfiles_check, 'different number of input files \n', ''),
                             ifelse(ctimes_check, 'newer input files \n', ''),
                             ifelse(rdata_check, 'absent Rdata file \n', ''),
                             ifelse(force_comp, 'because you said so! \n', '')
    ))}
    
    # Reading ecoregions maps, if provided. If Ecoregions is not available, assume
    # past distribution was flat among natural types
    er_ras_names = dir(er_dir)[dir(er_dir) %in% cfg$landscape_features$original_areas$ecoregions_raster_names]
    
    # Re-ordering to ensure the object created follows order in config file
    er_ras_names = er_ras_names[match(cfg$landscape_features$original_areas$ecoregions_raster_names,
                                      er_ras_names)]
    
    # Ecoregion maps required to deal with areas without natural-area remnants
    er_maps = lapply(paste0(er_dir, er_ras_names), function(x){load_raster(x, master_index)})
    names(er_maps) = cfg$landscape_features$original_areas$past_class_names   
    save(er_maps, file = paste0(in_dir, 'er.Rdata'))
  } else {
    if (config_check){
      if (verbose) {cat('Loading ecoregions data \n')}
      load(file = paste0(in_dir, 'er.Rdata'))}
    else {
      er_maps = lapply(c_lu_maps[lu_types == "N"],
                       function(x){Reduce('+', c_lu_maps[lu_types == "N"]) / length(which(lu_types == "N"))})
    }
  }

  # Original areas -------------------------------------------------------------
  
  if ((flag_log$oa == T) | (!file.exists(paste0(in_dir, 'oa.Rdata'))) ){
    # Total current anthropic area in each pixel
    c_anth_map = Reduce('+', c_lu_maps[lu_types == "A"])
    
    # Condition for applying ecoregion classes: no past natural or current anthropic above 95%
    corr_cond = ( (Reduce('+', p_lu_maps)==0) | (c_anth_map>0.95) )
    
    # Proportion of past natural area in each pixel
    p_nat_maps = lapply(p_lu_maps, function(x){x / Reduce('+', p_lu_maps)})
    
    # Correcting proportions of past natural areas in pixels where corr_cond is verified
    for (i in 1:length(p_nat_maps)){p_nat_maps[[i]][corr_cond] = er_maps[[i]][corr_cond]}
    
    # Proportion of current anthropic area converted on each natural type
    c_conv_map = lapply(p_nat_maps, function(x){x * c_anth_map})
    
    # Computing original areas
    oa_maps = mapply('+', c_lu_maps[lu_types == "N"], c_conv_map, SIMPLIFY=F)
    
    # Checking that original areas sum to 1 within given tolerance  
    check_oa = Reduce('+', c(oa_maps, list(Reduce('+', c_lu_maps[lu_types=='I']))))
    n_problems = length(which((check_oa -1) > tolerance))
    if(n_problems > 0){warning(paste0('OA was computed with ', n_problems,' inconsistencies'))}
    
    # Checking that original areas have no NA
    na_problem = lapply(oa_maps, function(x){length(which(is.na(x)))} != 0)
    
    save(oa_maps, file = paste0(in_dir, 'oa.Rdata'))
  } else {
    if (verbose) {cat('Loading original area data \n')}
    load (paste0(in_dir, 'oa.Rdata'))}

  oa_res = list(oa_vals = oa_maps, er_vals = er_maps, p_lu_vals = p_lu_maps,
                harmonize_log = file_log, update_flag = flag_log)
  
  save(oa_res, file = paste0(in_dir, 'harmonize_oa.Rdata'))
  return(oa_res)
}