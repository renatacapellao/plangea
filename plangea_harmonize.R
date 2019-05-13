
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
  
  lu_res = plangea_harmonize_lu(cfg, file_log=harmonize_log, flag_log=update_flag)
  
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
                                         master_index = master_index)

  allvar_list = ready_res$allvar_list
  harmonize_log = ready_res$harmonize_log
  update_flag = ready_res$update_flag



  
  # Opportunity cost ---------------------------------------------------------
  source('plangea_harmonize_oc.R')
    
  oc_res = plangea_harmonize_oc(cfg, file_log = harmonize_log,
                                flag_log = update_flag,
                                master_index = master_index,
                                lu_val_list = lu_vals)
  
  oc = oc_res$oc
  harmonize_log = oc_res$harmonize_log
  update_flag = oc_res$update_flag

  allvar_list[names(allvar_list) %in% cfg$variables$calc_oc$oc_variable_name] = list(oc)
    

  # Original areas (OA) ---------------------------------------------------------
  source('plangea_harmonize_oa.R')
    
  oa_res = plangea_harmonize_oa(cfg, file_log = harmonize_log,
                                flag_log = update_flag, c_lu_maps = lu_vals,
                                lu_types = lu_class_types, tolerance=1.e-7,
                                force_comp = T)
  
  oa = oa_res$oa_vals
  harmonize_log = oa_res$harmonize_log
  update_flag = oa_res$update_flag
    

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
