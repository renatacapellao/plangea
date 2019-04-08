plangea_harmonize_lu = function(cfg){
  
  # Load land-use rasters names
  lu_ras_names = dir(lu_dir)[dir(lu_dir) %in% cfg$landscape_features$land_use$classes_raster_names]
  
  # Re-ordering to ensure the object created follows order in config file
  lu_ras_names = lu_ras_names[match(cfg$landscape_features$land_use$classes_raster_names, lu_ras_names)]
  
  # Creating raster stack with rasters corresponding to LU names
  #lu_ras = stack(paste0(lu_dir, lu_ras_names))
  lu_ras = lapply(paste0(lu_dir, lu_ras_names), function(x){load_raster(x)})
  names(lu_ras) = cfg$landscape_features$land_use$class_names
  
  # Rasters were loaded - saving update info
  lu_log = file.info(dir(lu_dir, full.names = T)[dir(lu_dir) %in% cfg$landscape_features$land_use$classes_raster_names])
  
  # Load land-use class types
  # Possible types of LU: "N"atural, "A"nthropic, "I"gnore
  lu_class_types = cfg$landscape_features$land_use$class_types  
  
  # Building raster with terrestrial areas
  terrestrial_areas = Reduce('+', lu_ras[lu_class_types %in% c("N", "A")])
  
  # Builds index with terrestrial pixels
  terrestrial_index = which(values(terrestrial_areas > 0))
  
  # Create Results list, Save & Return
  lu_res = list(terrestrial_index = terrestrial_index,
                lu_ras = lu_ras,
                lu_class_types = lu_class_types,
                lu_log = lu_log)
  
  save(lu_res, file = paste0(in_dir, 'harmonize_lu.Rdata'))
  
  return(lu_res)
}