
plangea_harmonize = function(cfg, config_json_filename){
  
  # Initiating structures for controling changes -------------------------------
  
  # Creating / Loading harmonize_log: stores info on files loaded by the script
  if (file.exists(paste0(in_dir, 'harmonize_log.Rdata'))){
    load(paste0(in_dir, 'harmonize_log.Rdata'))
  } else {
      present_json_info = cfg
      harmonize_log = list(start = present_json_info)
      save(harmonize_log, file = (paste0(in_dir, 'harmonize_log.Rdata')))
    }
  
  # Creating update_flag: stores info on which sub-modules actively computed their result
  update_flag = data.frame(start=T)
  
  # Reading backup config file
  #if(file.exists(paste0(in_dir, 'cfg_bk.Rdata'))){
  #  load(paste0(in_dir, 'cfg_bk.Rdata'))
  #  } else {cfg_bk = rapply(cfg, function(x){''}, how = "replace")}
  
  
  # Sub-module: Land-use / Terrestrial Index -----------------------------------
  source('plangea_harmonize_lu.R')
  
  lu_res = plangea_harmonize_lu(cfg, file_log=harmonize_log, flag_log=update_flag, force_comp=T)
  
  lu_vals = lu_res$lu_vals
  master_index = lu_res$master_index
  terrestrial_index = lu_res$terrestrial_index
  lu_class_types = lu_res$lu_class_types
  harmonize_log = lu_res$harmonize_log
  update_flag = lu_res$update_flag
  

  # Sub-module: include ready variables into allvar_list -----------------------
  source('plangea_harmonize_add_ready.R')

  ready_res = plangea_harmonize_add_ready(cfg, file_log = harmonize_log,
                                         flag_log = update_flag, 
                                         master_index = master_index, force_comp=T)

  allvar_list = ready_res$allvar_list
  harmonize_log = ready_res$harmonize_log
  update_flag = ready_res$update_flag



  
  # Opportunity cost ---------------------------------------------------------
  # Computing opportunity costs, if: (a) oc is not labeled as ready in
  # $ready_variables, and if (b) the required rasters for computing the oc are
  # in the right folder
  update_flag$oc = F
  harmonize_log$calc_oc = ''
  if (!cfg$variables$ready_variables[cfg$variables$variable_names %in%
                                     (cfg$variables$calc_oc$oc_variable_name)] & # condition (a)
      (length(which(dir(var_dir) %in% cfg$variables$calc_oc$oc_files)) ==
      length(cfg$variables$calc_oc$oc_names))){ # condition (b)
    
    if (update_flag$lu){
      source('plangea_calc_oc.R')
      oc_ras_names = cfg$variables$calc_oc$oc_files
      
      cost_list = lapply(paste0(var_dir, oc_ras_names),
                         function(x){load_raster(x, master_index)})
      
      names(cost_list) = cfg$variables$calc_oc$oc_names
      
      # Saving in harmonize_log information about rasters used to build cost_list
      harmonize_log$calc_oc = file.info(dir(var_dir, full.names = T)[dir(var_dir) %in% oc_ras_names], extra_cols = F)
      
      # computing opportunity cost
      oc = plangea_calc_oc(cfg, cost_list = cost_list, lu_val_list=lu_vals, master_index=master_index)
      
      update_flag$oc = T
    } else {
      load(paste0(in_dir, 'oc.Rdata'))
    }

    # Including opportunity cost into allvar_list
    allvar_list[names(allvar_list) %in% cfg$variables$calc_oc$oc_variable_name] = list(oc)
    
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
