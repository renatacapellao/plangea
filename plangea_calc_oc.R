plangea_calc_oc = function(cfg, lu.val.list, master_index){
  oc.ras.names = cfg$variables$calc_oc$oc_files
  
  cost.list = lapply(paste0(var.dir, oc.ras.names),
                   function(x){load_raster(x, master_index)})
  
  names(cost.list) = cfg$variables$calc_oc$oc_names
  
  map.list = lu.val.list[names(lu.val.list) %in% cfg$variables$calc_oc$corresponding_lu_classes]
  
  oc = Reduce('+', mapply('*', cost.list, map.list, SIMPLIFY=F))
}