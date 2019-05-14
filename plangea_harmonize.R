
plangea_harmonize = function(cfg, config_json_filename, verbose=F){
  
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
  
  lu_res = plangea_harmonize_lu(cfg, file_log=harmonize_log, flag_log=update_flag,
                                verbose = verbose)
  
  lu_vals = lu_res$lu_vals
  master_index = lu_res$master_index
  terrestrial_index = lu_res$terrestrial_index
  lu_class_types = lu_res$lu_class_types
  harmonize_log = lu_res$harmonize_log
  update_flag = lu_res$update_flag
  rm(lu_res)
  

  # Sub-module: include ready variables into allvar_list -----------------------
  source('plangea_harmonize_add_ready.R')

  ready_res = plangea_harmonize_add_ready(cfg, file_log = harmonize_log,
                                         flag_log = update_flag, 
                                         master_index = master_index,
                                         verbose = verbose)

  allvar_list = ready_res$allvar_list
  harmonize_log = ready_res$harmonize_log
  update_flag = ready_res$update_flag
  rm(ready_res)



  
  # Sub-module: opportunity cost -----------------------------------------------
  source('plangea_harmonize_oc.R')
    
  oc_res = plangea_harmonize_oc(cfg, file_log = harmonize_log,
                                flag_log = update_flag,
                                master_index = master_index,
                                lu_val_list = lu_vals,
                                verbose = verbose)
  
  oc = oc_res$oc
  harmonize_log = oc_res$harmonize_log
  update_flag = oc_res$update_flag
  rm(oc_res)

  allvar_list[names(allvar_list) %in% cfg$variables$calc_oc$oc_variable_name] = list(oc)
    

  # Sub-module: original areas (OA) --------------------------------------------
  source('plangea_harmonize_oa.R')
    
  oa_res = plangea_harmonize_oa(cfg, file_log = harmonize_log,
                                flag_log = update_flag, c_lu_maps = lu_vals,
                                lu_types = lu_class_types, tolerance=1.e-7,
                                verbose = verbose)
  
  oa_vals = oa_res$oa_vals
  harmonize_log = oa_res$harmonize_log
  update_flag = oa_res$update_flag
  rm(oa_res)
    

  # Sub-module: biodiversity ---------------------------------------------------
  source('plangea_harmonize_bd.R')
  
  bd_res = plangea_harmonize_bd(cfg, file_log = harmonize_log,
                                flag_log = update_flag, lu_vals = lu_vals,
                                master_index = master_index, oa_vals = oa_vals,
                                verbose = verbose)

  bd = bd_res$bd
  usphab_index = bd_res$usphab_index 
  species_index_list_proc = bd_res$species_index_list_proc
  hab_now_areas = bd_res$hab_now_areas
  hab_pot_areas = bd_res$hab_pot_areas
  harmonize_log = bd_res$harmonize_log
  update_flag = bd_res$update_flag
  rm(bd_res)

  allvar_list[names(allvar_list) %in% cfg$variables$calc_bd$bd_variable_name] = list(bd)
  
  
  # Saving Rdatas and returning ------------------------------------------------
  harmonize_res = mget(objects())
  save(harmonize_res, file='harmonize_full_envir.RData')
  return(harmonize_res)
}
