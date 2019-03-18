plangea_calc_bd = function(cfg, lu.vals, master_index, OA=NULL){
  
  # Number of rasters in each subfolder defined in the config json
  n.rasters = lapply(cfg$variables$calc_bd$bd_subfolders,
                     function(x){length(dir(paste0(spp.dir,x)))})
  
  # List of raster names, divided by BD-classes / subfolders 
  raster.names = lapply(cfg$variables$calc_bd$bd_subfolders,
                       function(x){dir(paste0(spp.dir,x))})
  names(raster.names) = cfg$variables$calc_bd$bd_classes
  
  # List of species-raster values
  bd.vals = c()
  for (sf in cfg$variables$calc_bd$bd_subfolders){
    bd.vals = c(bd.vals, lapply(dir(paste0(spp.dir,sf), full.names=T),
                                function(x){#print(paste0('loading raster ', x));
                                  load_raster(x, master_index=master_index)})) }
  names(bd.vals) = raster.names
  
  # Loading list of suitable land-uses for each species
  bd.table = read.csv(paste0(spp.dir, cfg$variables$calc_bd$bd_table_file))
  
  # Compute present habitats by intersecting bd.vals with corresponding lu.vals
  hab.vals = list()
  for (spid in unique(bd.table[,names(bd.table) %in% cfg$variables$calc_bd$bd_spp_names_column])){
    bd.vals[names(bd.vals) == spid][[1]] *              # species range for spid
      Reduce('+', lu.vals[names(lu.vals) %in% iter.lu]) # sum of suitable land-uses for spid
  }
  
  
  bd.table[spid,names(bd.table) %in% cfg$variables$calc_bd$bd_lu_names_column]
}