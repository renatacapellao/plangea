plangea_calc_oc = function(cfg, lu_val_list, master_index){
  oc_ras_names = cfg$variables$calc_oc$oc_files
  
  cost_list = lapply(paste0(var_dir, oc_ras_names),
                   function(x){load_raster(x, master_index)})
  
  names(cost_list) = cfg$variables$calc_oc$oc_names
  
  map_list = lu_val_list[names(lu_val_list) %in% cfg$variables$calc_oc$corresponding_lu_classes]
  
  oc = Reduce('+', mapply('*', cost_list, map_list, SIMPLIFY=F))
}