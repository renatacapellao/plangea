
plangea_harmonize = function(cfg){
  # set the directory names from which to load all the data for the optimisation
  base.dir = cfg$io$base_path
  rawdata.dir = paste0(base.dir, cfg$io$rawdata_path)
  lu.dir = paste0(rawdata.dir, cfg$io$lu_path)
  in.dir = paste0(base.dir, cfg$io$preprocessed_path)
  out.dir = paste0(base.dir, cfg$io$output_path)
  
  # Load land-use rasters names
  lu.ras.names = dir(lu.dir)[dir(lu.dir) %in% cfg$landscape_features$land_use$classes_raster_names]
  
  # Re-ordering to ensure the object created follows order in config file
  lu.ras.names = lu.ras.names[match(cfg$landscape_features$land_use$classes_raster_names, lu.ras.names)]
  
  # Load land-use class types
  lu.class.types = cfg$landscape_features$land_use$class_types
  
  # Creating raster stack with rasters corresponding to LU names
  #lu.ras = stack(paste0(lu.dir, lu.ras.names))
  lu.ras = lapply(paste0(lu.dir, lu.ras.names), function(x){raster(x)})
  names(lu.ras) = cfg$landscape_features$land_use$class_names
  
  # Loads type of optimisation problem
  # Possible types of optimisation: "C"onservation, "R"estoration
  optim.type = cfg$scenarios$problem_type
  
  # Building list relating type of LU to type of optimisation
  # Possible types of LU: "N"atural, "A"nthropic, "I"gnore
  lu.to.optim = lu.class.types
  lu.to.optim[lu.to.optim == 'N'] = 'C'
  lu.to.optim[lu.to.optim == 'A'] = 'R'
  
  # Builds raster of interest areas
  interest.areas = Reduce('+', lu.ras[lu.to.optim %in% optim.type])
  
  # Building master_index of pixels of interest
  master_index = which(values(interest.areas > 0))
  
  # Creating list with raster values corresponding to the master_index
  lu.vals = lapply(lu.ras, function(x){x[master_index]})
  
  
  # List .Rdata files saved in dir
  #obj.list = dir(dir, full.names=T, pattern='.Rdata', ignore.case=T)
  # Loads all objects with names in obj.list
}
