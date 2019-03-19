
plangea_harmonize = function(cfg){
  # Directory names ---------------------------------------------------------
  # set the directory names from which to load all the data for the optimisation
  base.dir = cfg$io$base_path
  rawdata.dir = paste0(base.dir, cfg$io$rawdata_path)
  lu.dir = paste0(rawdata.dir, cfg$io$lu_path)
  past.lu.dir = paste0(rawdata.dir, cfg$io$lu_path, cfg$io$past_lu_path)
  var.dir = paste0(rawdata.dir, cfg$io$variables_path)
  spp.dir = paste0(rawdata.dir, cfg$io$species_path)
  in.dir = paste0(base.dir, cfg$io$preprocessed_path)
  out.dir = paste0(base.dir, cfg$io$output_path)



  
  # Land-use ---------------------------------------------------------
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
  
  # Master Index ---------------------------------------------------------
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
  
#<<<<<<< HEAD
  var.vals = lapply(paste0(var.dir, var.ras.names[cfg$variables$ready_variables]),
                    function(x){load_raster(x, master_index)})
  names(var.vals) = cfg$variables$variable_names[cfg$variables$ready_variables]
  
  # Build list of all variables
  allvar.list = as.list(cfg$variables$variable_names)
  names(allvar.list) = allvar.list
  
  # Include in allvar list the variables already loaded
  allvar.list[names(allvar.list) %in% names(var.vals)] = var.vals
  
  
  # Opportunity cost ---------------------------------------------------------
  # Computing opportunity costs, if: (a) oc is not labeled as ready in
  # $ready_variables, and if (b) the required rasters are in the right folder
  if (!cfg$variables$ready_variables[cfg$variables$variable_names %in%
                                     (cfg$variables$calc_oc$oc_variable_name)] & # condition (a)
      (length(which(dir(var.dir) %in% cfg$variables$calc_oc$oc_files)) ==
      length(cfg$variables$calc_oc$oc_names))){ # condition (b)
    
    source('plangea_calc_oc.R')
  
    allvar.list[names(allvar.list) %in% cfg$variables$calc_oc$oc_variable_name]=
      list(plangea_calc_oc(cfg, lu.val.list=lu.vals, master_index=master_index))
    
  } # end of calc_oc if statement
  
  
  # Original areas (OA) ---------------------------------------------------------
  past.lu.vals = NULL
  
  if (cfg$landscape_features$original_areas$include_past){
    past.names = cfg$landscape_features$original_areas$past_raster_names
    past.lu.vals = lapply(paste0(past.lu.dir, past.names), function(x){load_raster(x, master_index)})
    names(past.lu.vals) = cfg$landscape_features$original_areas$past_class_names
  }
    
  source('plangea_calc_oa.R')
    
  oa.vals = plangea_calc_oa(lu.vals, past.lu.vals, lu.class.types)
    

  # Biodiversity ---------------------------------------------------------
  # calc_bd placeholder

    
#=======
  # List .Rdata files saved in dir
  #obj.list = dir(dir, full.names=T, pattern='.Rdata', ignore.case=T)
  # Loads all objects with names in obj.list
#>>>>>>> 82f05d12db99c7fed4f34e3018ec5959b097fe1f
  
} # end of plangea_harmonize function
