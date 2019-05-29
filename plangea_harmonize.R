
plangea_harmonize = function(cfg, config_json_filename, verbose=F, force_comp = F){
  
  # Initiating structures for controling changes -------------------------------
  
  # Creating / Loading harmonize_log: stores info on files loaded by the script
  if (file.exists(paste0(in_dir, 'harmonize_log'))){
    harmonize_log = pigz_load(paste0(in_dir, 'harmonize_log'))
  } else {
      harmonize_log = list(start = cfg)
      pigz_save(harmonize_log, file = (paste0(in_dir, 'harmonize_log')))
    }
  
  # Creating update_flag: stores info on which sub-modules actively computed their result
  update_flag = data.frame(start=T)
  
  # Reading backup config file
  #if(file.exists(paste0(in_dir, 'cfg_bk'))){
  #  cfg_bk = pigz_load(paste0(in_dir, 'cfg_bk'))
  #  } else {cfg_bk = rapply(cfg, function(x){''}, how = "replace")}
  
  
  # Sub-module: Land-use / Terrestrial Index -----------------------------------
  source('plangea_harmonize_lu.R')
  
  lu_res = plangea_harmonize_lu(cfg, file_log=harmonize_log, flag_log=update_flag,
                                verbose = verbose,
                                force_comp = force_comp)
  
  lu_vals = lu_res$lu_vals
  lu_terr = lu_res$lu_terr
  master_index = lu_res$master_index
  terrestrial_index = lu_res$terrestrial_index
  ub_vals = lu_res$ub_vals
  overall_area = lu_res$overall_area
  lu_class_types = lu_res$lu_class_types
  px_area = lu_res$px_area
  harmonize_log = lu_res$harmonize_log
  update_flag = lu_res$update_flag
  rm(lu_res)
  

  # Sub-module: include ready variables into allvar_list -----------------------
  source('plangea_harmonize_add_ready.R')

  ready_res = plangea_harmonize_add_ready(cfg, file_log = harmonize_log,
                                         flag_log = update_flag, 
                                         master_index = master_index,
                                         verbose = verbose,
                                         force_comp = force_comp)

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
                                verbose = verbose,
                                force_comp = force_comp)
  
  oc = oc_res$oc
  harmonize_log = oc_res$harmonize_log
  update_flag = oc_res$update_flag
  rm(oc_res)

  allvar_list[names(allvar_list) %in% cfg$variables$calc_oc$oc_variable_name] = list(oc)
    

  # Sub-module: original areas (OA) --------------------------------------------
  source('plangea_harmonize_oa.R')
    
  oa_res = plangea_harmonize_oa(cfg, file_log = harmonize_log,
                                master_index = master_index,
                                flag_log = update_flag, c_lu_maps = lu_vals,
                                lu_types = lu_class_types, tolerance=1.e-7,
                                verbose = verbose,
                                force_comp = force_comp)
  
  oa_vals = oa_res$oa_vals
  harmonize_log = oa_res$harmonize_log
  update_flag = oa_res$update_flag
  rm(oa_res)
    
  
  # Sub-module: sub-regions ----------------------------------------------------
  if (cfg$scenarios$`sub-region_scenarios`$include_subregion_scenarios){
    source('plangea_harmonize_subregions.R')    
    
    sr_res = plangea_harmonize_subregions(cfg, file_log = harmonize_log,
                                          flag_log = update_flag,
                                          master_index = master_index,
                                          ub_vals = ub_vals,
                                          verbose = verbose, 
                                          force_comp = force_comp)
    
    sr_vals = sr_res$sr_vals
    sr_tbl = sr_res$sr_tbl
    sr_coefs = sr_res$sr_coefs
    sr_targets = sr_res$sr_targets
    harmonize_log = sr_res$harmonize_log
    update_flag = sr_res$update_flag
    rm(sr_res)    
  }
  
  
  # Sub-module: include refreshable variables into allvar_list -----------------
  # (plangea_refresh_vars script is sourced at the wrapper level)
  
  refresh_res = plangea_refresh_vars(cfg, upper_env = mget(objects()),
                                     verbose = verbose)
  
  
  # Sub-module: biodiversity ---------------------------------------------------
  source('plangea_harmonize_bd.R')
  
  bd_res = plangea_harmonize_bd(cfg, file_log = harmonize_log,
                                flag_log = update_flag, lu_terr = lu_terr,
                                master_index = master_index,
                                terrestrial_index = terrestrial_index,
                                oa_vals = oa_vals,
                                verbose = verbose,
                                force_comp = force_comp)

  bd = bd_res$bd
  usphab_index = bd_res$usphab_index 
  species_index_list_proc = bd_res$species_index_list_proc
  hab_now_areas = bd_res$hab_now_areas
  hab_pot_areas = bd_res$hab_pot_areas
  harmonize_log = bd_res$harmonize_log
  update_flag = bd_res$update_flag
  rm(bd_res)

  allvar_list[names(allvar_list) %in% cfg$variables$calc_bd$bd_variable_name] = list(bd)

  # Updating harmonize log with cfg used in this run
  harmonize_log$start = cfg
  pigz_save(harmonize_log, file=paste0(in_dir, 'harmonize_log'))
  
  # Saving rdss and returning ------------------------------------------------
  harmonize_res = mget(objects())
  pigz_save(harmonize_res, file=paste0(in_dir, 'harmonize_full_envir'))
  return(harmonize_res)
}
