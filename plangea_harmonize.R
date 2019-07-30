
plangea_harmonize = function(cfg, config_json_filename, verbose=F, force_comp = F){
  
  # Initiating structures for controling changes -------------------------------
  
  # Creating / Loading harmonize_log: stores info on files loaded by the script
  if (file.exists(paste0(in_dir, 'harmonize_log'))) {
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
  
  # Unpacking lu_res variables
  for (i in 1:length(lu_res)) {assign(names(lu_res)[i], lu_res[[i]])}
  
  # Creating base/background raster (mask)
  lu_aux = pigz_load(paste0(in_dir, 'lu_aux'))
  base_ras = lu_aux$terrestrial_areas
  base_ras[(base_ras>0)] = 0
  rm(lu_aux)

  # Sub-module: include ready variables into allvar_list -----------------------
  source('plangea_harmonize_add_ready.R')

  ready_res = plangea_harmonize_add_ready(cfg, file_log = harmonize_log,
                                         flag_log = update_flag, 
                                         master_index = master_index,
                                         verbose = verbose,
                                         force_comp = force_comp)

  # Unpacking ready_res variables
  for (i in 1:length(ready_res)) {assign(names(ready_res)[i], ready_res[[i]])}

  
  # Sub-module: opportunity cost -----------------------------------------------
  source('plangea_harmonize_oc.R')
    
  oc_res = plangea_harmonize_oc(cfg, file_log = harmonize_log,
                                flag_log = update_flag,
                                master_index = master_index,
                                lu_val_list = lu_vals,
                                verbose = verbose,
                                force_comp = force_comp)
  
  # Unpacking oc_res variables
  for (i in 1:length(oc_res)) {assign(names(oc_res)[i], oc_res[[i]])}

  allvar_list[names(allvar_list) %in% cfg$variables$calc_oc$oc_variable_name] = list(oc)
    

  # Sub-module: original areas (OA) --------------------------------------------
  source('plangea_harmonize_oa.R')
    
  oa_res = plangea_harmonize_oa(cfg, file_log = harmonize_log,
                                master_index = master_index,
                                flag_log = update_flag, c_lu_maps = lu_vals,
                                lu_types = lu_class_types, tolerance=1.e-7,
                                verbose = verbose,
                                force_comp = force_comp)
  
  # Unpacking oa_res variables
  for (i in 1:length(oa_res)) {assign(names(oa_res)[i], oa_res[[i]])}
    
  
  # Sub-module: sub-regions ----------------------------------------------------
  if (cfg$scenarios$`sub-region_scenarios`$include_subregion_scenarios) {
    source('plangea_harmonize_subregions.R')    
    
    sr_res = plangea_harmonize_subregions(cfg, file_log = harmonize_log,
                                          flag_log = update_flag,
                                          master_index = master_index,
                                          ub_vals = ub_vals,
                                          px_area = px_area,
                                          verbose = verbose, 
                                          force_comp = force_comp)
    
    # Unpacking sr_res variables
    for (i in 1:length(sr_res)) {assign(names(sr_res)[i], sr_res[[i]])}   
  }
  
  
  # Sub-module: include refreshable variables into allvar_list -----------------
  # (plangea_refresh_vars script is sourced at the wrapper level)
  
  allvar_list = plangea_refresh_vars(cfg, upper_env = mget(objects()),
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

  # Unpacking bd_res variables
  for (i in 1:length(bd_res)) {assign(names(bd_res)[i], bd_res[[i]])}

  allvar_list[names(allvar_list) %in% cfg$variables$calc_bd$bd_variable_name] = list(bd)

  # Updating harmonize log with cfg used in this run
  harmonize_log$start = cfg
  pigz_save(harmonize_log, file=paste0(in_dir, 'harmonize_log'))
  
  # Saving rdss and returning ------------------------------------------------
  harmonize_res = mget(objects())
  pigz_save(harmonize_res, file=paste0(in_dir, 'harmonize_full_envir'))
  return(harmonize_res)
}
