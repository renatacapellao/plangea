plangea_calc_oc = function(cfg, cost_list, lu_val_list, master_index){
  
  map_list = lu_val_list[names(lu_val_list) %in% cfg$variables$calc_oc$corresponding_lu_classes]
  
  oc = Reduce('+', mapply('*', cost_list, map_list, SIMPLIFY=F))
  
  save(oc, file = paste0(in_dir, 'oc.Rdata'))
  return(oc)
}