
plangea_harmonize = function(cfg){
  # set the directory names from which to load all the data for the optimisation
  base.dir = cfg$io$base_path
  rawdata.dir = paste0(base.dir, cfg$io$rawdata_path)
  lu.dir = paste0(rawdata.dir, cfg$io$lu_path)
  var.dir = paste0(rawdata.dir, cfg$io$variables_path)
  in.dir = paste0(base.dir, cfg$io$preprocessed_path)
  out.dir = paste0(base.dir, cfg$io$output_path)
  
  
  # Load land-use rasters names
  lu.ras.names = dir(lu.dir)[dir(lu.dir) %in% cfg$landscape_features$land_use$classes_raster_names]
  
  # Re-ordering to ensure the object created follows order in config file
  lu.ras.names = lu.ras.names[match(cfg$landscape_features$land_use$classes_raster_names, lu.ras.names)]
  
  # Creating raster stack with rasters corresponding to LU names
  #lu.ras = stack(paste0(lu.dir, lu.ras.names))
  lu.ras = lapply(paste0(lu.dir, lu.ras.names), function(x){load_raster(x)})
  names(lu.ras) = cfg$landscape_features$land_use$class_names

  
  # Load land-use class types
  # Possible types of LU: "N"atural, "A"nthropic, "I"gnore
  lu.class.types = cfg$landscape_features$land_use$class_types  

  # Building raster with terrestrial areas
  terrestrial.areas = Reduce('+', lu.ras[lu.class.types %in% c("N", "A")])
  
  # Builds index with terrestrial pixels
  terrestrial_index = which(values(terrestrial.areas > 0))
  
  
  # Loading type of optimisation problem
  # Possible types of optimisation: "C"onservation, "R"estoration
  optim.type = cfg$scenarios$problem_type
  
  # Building list relating type of LU to type of optimisation
  lu.to.optim = lu.class.types
  lu.to.optim[lu.to.optim == 'N'] = 'C'
  lu.to.optim[lu.to.optim == 'A'] = 'R'
  
  # Builds raster of interest areas
  interest.areas = Reduce('+', lu.ras[lu.to.optim %in% optim.type])
  
  # Building master_index of pixels of interest
  master_index = which(values(interest.areas > 0))
  
  
  # Creating list with raster values corresponding to the master_index
  lu.vals = lapply(lu.ras, function(x){x[master_index]})
  
  # Loading ready variables
  var.ras.names = cfg$variables$variable_raster_names
  
<<<<<<< HEAD
  var.vals = lapply(paste0(var.dir, var.ras.names[cfg$variables$ready_variables]),
                    function(x){load_raster(x, master_index)})
  names(var.vals) = cfg$variables$variable_names[cfg$variables$ready_variables]
  
  # Build list of all variables
  allvar.list = as.list(cfg$variables$variable_names)
  names(allvar.list) = allvar.list
  
  # Include in allvar list the variables already loaded
  allvar.list[names(allvar.list) %in% names(var.vals)] = var.vals
  

  # Computing opportunity costs, if: (a) oc is not labeled as ready in
  # $ready_variables, and if (b) the required rasters are in the right folder
  if (!cfg$variables$ready_variables[cfg$variables$variable_names %in%
                                     (cfg$variables$calc_oc$oc_variable_name)] & # condition (a)
      (length(which(dir(var.dir) %in% cfg$variables$calc_oc$oc_files)) ==
      length(cfg$variables$calc_oc$oc_names))){ # condition (b)
    oc.ras.names = cfg$variables$calc_oc$oc_files
    oc.vals = lapply(paste0(var.dir, oc.ras.names),
                     function(x){load_raster(x, master_index)})
    names(oc.vals) = cfg$variables$calc_oc$oc_names
    
    # Including oc computed using calc_oc in corresponding entry of allvar list
    allvar.list[names(allvar.list) %in% cfg$variables$calc_oc$oc_variable_name] = 
      list(calc_oc(occ = oc.vals$occ, ocg = oc.vals$ocg,
              crp.map = lu.vals$AGR, grs.map = lu.vals$CGR))
  }
  
  # calc_bd placeholder
  
=======
  # List .Rdata files saved in dir
  #obj.list = dir(dir, full.names=T, pattern='.Rdata', ignore.case=T)
  # Loads all objects with names in obj.list
>>>>>>> 82f05d12db99c7fed4f34e3018ec5959b097fe1f
}
