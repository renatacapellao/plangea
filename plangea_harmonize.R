
plangea_harmonize = function(cfg, config_json_filename){
  
  # Structures for controling changes ------------------------------------
  
  # Creating / Loading log file
  if (file.exists(paste0(in_dir, 'harmonize_log.Rdata'))){
    load(paste0(in_dir, 'harmonize_log.Rdata'))
  } else {
      harmonize_log = list(json = present_json_info)
      save(harmonize_log, file = (paste0(in_dir, 'harmonize_log.Rdata')))
    }
  
  # Creating control-flags data.frame for updated input files
  update_flag = data.frame(lu=F)
  
  # Reading backup config file
  if(file.exists(paste0(in_dir, 'cfg_bk.Rdata'))){
    load(paste0(in_dir, 'cfg_bk.Rdata'))
    } else {cfg_bk = cfg}
  
  
  # Land-use / Terrestrial Index -----------------------------------------
  present_lu_info = file.info(dir(lu_dir, full.names = T)[dir(lu_dir) %in% cfg$landscape_features$land_use$classes_raster_names], extra_cols = F)
  
  if(is.null(harmonize_log$lu)){harmonize_log$lu = present_lu_info}   # in case the script is running for the 1st time
  
  if ((nrow(present_lu_info) != nrow(harmonize_log$lu)) |             # number of files is not the same or
      (prod(present_lu_info$ctime != harmonize_log$lu$ctime)==1) |    # creation times are not the same or
      (!file.exists(paste0(in_dir, 'harmonize_lu.Rdata')))            # resulting processed data file not found
      ){
    source('plangea_harmonize_lu.R')
    lu_res = plangea_harmonize_lu(cfg)
    harmonize_log$lu = lu_res$lu_log
    update_flag$lu = T
  } else {
    load(paste0(in_dir, 'harmonize_lu.Rdata'))
    update_flag$lu = F
    }
  
  
  # Master Index ---------------------------------------------------------
  if ((cfg$scenarios$problem_type != cfg_bk$scenarios$problem_type) | # problem type changed or
      (update_flag$lu)){                                              # land-use files were updated
    source('plangea_harmonize_master_index.R')
    master_index = plangea_harmonize_master_index(cfg, lu_res)
    update_flag$master = T
  } else {
    load(paste0(in_dir, 'master_index.Rdata'))
    update_flag$master = F
  }
  
  # Creating list with raster values corresponding to the master_index
  lu_vals = lapply(lu_res$lu_ras, function(x){x[master_index]})
  
  
  # All-variables list -------------------------------------------------------
  # Loading ready variables
  var_ras_names = cfg$variables$variable_raster_names
  
  var_vals = lapply(paste0(var_dir, var_ras_names[cfg$variables$ready_variables]),
                    function(x){load_raster(x, master_index)})
  names(var_vals) = cfg$variables$variable_names[cfg$variables$ready_variables]
  
  # Build list of all variables
  allvar_list = as.list(cfg$variables$variable_names)
  names(allvar_list) = allvar_list
  
  # Include in allvar list the variables already loaded
  allvar_list[names(allvar_list) %in% names(var_vals)] = var_vals
  
  
  # Opportunity cost ---------------------------------------------------------
  # Computing opportunity costs, if: (a) oc is not labeled as ready in
  # $ready_variables, and if (b) the required rasters are in the right folder
  if (!cfg$variables$ready_variables[cfg$variables$variable_names %in%
                                     (cfg$variables$calc_oc$oc_variable_name)] & # condition (a)
      (length(which(dir(var_dir) %in% cfg$variables$calc_oc$oc_files)) ==
      length(cfg$variables$calc_oc$oc_names))){ # condition (b)
    
    source('plangea_calc_oc.R')
    
    # Including opportunity cost into allvar_list
    allvar_list[names(allvar_list) %in% cfg$variables$calc_oc$oc_variable_name]=
      list(plangea_calc_oc(cfg, lu_val_list=lu_vals, master_index=master_index))
    
  } # end of calc_oc if statement

  
  # Ecoregions maps ------------------------------------------------------------
  # Load land-use rasters names
  er_ras_names = dir(er_dir)[dir(er_dir) %in% cfg$landscape_features$original_areas$ecoregions_raster_names]
  
  # Re-ordering to ensure the object created follows order in config file
  er_ras_names = er_ras_names[match(cfg$landscape_features$original_areas$ecoregions_raster_names, er_ras_names)]
  
  # Ecoregion maps required to deal with areas without natural-area remnants
  er_maps = lapply(paste0(er_dir, er_ras_names), function(x){load_raster(x, master_index)})
  names(er_maps) = cfg$landscape_features$original_areas$past_class_names
  
  
  # Original areas (OA) ---------------------------------------------------------
  past_lu_vals = NULL
  
  if (cfg$landscape_features$original_areas$include_past){
    past_names = cfg$landscape_features$original_areas$past_raster_names
    past_lu_vals = lapply(paste0(past_lu_dir, past_names), function(x){load_raster(x, master_index)})
    names(past_lu_vals) = cfg$landscape_features$original_areas$past_class_names
  }
  
  source('plangea_calc_oa.R')
    
  oa_vals = plangea_calc_oa(c_lu_maps = lu_vals, er_maps = er_maps,
                            p_lu_maps = past_lu_vals, lu_type = lu_class_types,
                            tolerance=1.e-7)
    

  # Biodiversity ---------------------------------------------------------
  
  source('plangea_calc_bd.R')
  
  bd = plangea_calc_bd(cfg = cfg, lu_vals = lu_vals,
                       master_index = master_index, oa_vals = oa_vals)
  
  # Including biodiversity benefits into allvar_list
  allvar_list[names(allvar_list) %in% cfg$variables$calc_bd$bd_variable_name] = list(bd)
  
  # Saving Rdatas
  env_list = ls()
  for (i in )

  # Updating backup config data  
  save(cfg, paste0(in_dir, 'cfg_bk.Rdata'))
  
  return(allvar_list)
    
#=======
  # List _Rdata files saved in dir
  #obj_list = dir(dir, full_names=T, pattern='_Rdata', ignore_case=T)
  # Loads all objects with names in obj_list
#>>>>>>> 82f05d12db99c7fed4f34e3018ec5959b097fe1f
  
} # end of plangea_harmonize function
