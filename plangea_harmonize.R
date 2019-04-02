
plangea_harmonize = function(cfg){
  # Land-use ---------------------------------------------------------
  # Load land-use rasters names
  lu_ras_names = dir(lu_dir)[dir(lu_dir) %in% cfg$landscape_features$land_use$classes_raster_names]
  
  # Re-ordering to ensure the object created follows order in config file
  lu_ras_names = lu_ras_names[match(cfg$landscape_features$land_use$classes_raster_names, lu_ras_names)]
  
  # Creating raster stack with rasters corresponding to LU names
  #lu_ras = stack(paste0(lu_dir, lu_ras_names))
  lu_ras = lapply(paste0(lu_dir, lu_ras_names), function(x){load_raster(x)})
  names(lu_ras) = cfg$landscape_features$land_use$class_names

  # Load land-use class types
  # Possible types of LU: "N"atural, "A"nthropic, "I"gnore
  lu_class_types = cfg$landscape_features$land_use$class_types  

  # Building raster with terrestrial areas
  terrestrial_areas = Reduce('+', lu_ras[lu_class_types %in% c("N", "A")])
  
  # Builds index with terrestrial pixels
  terrestrial_index = which(values(terrestrial_areas > 0))
  
  # Master Index ---------------------------------------------------------
  # Loading type of optimisation problem
  # Possible types of optimisation: "C"onservation, "R"estoration
  optim_type = cfg$scenarios$problem_type
  
  # Building list relating type of LU to type of optimisation
  lu_to_optim = lu_class_types
  lu_to_optim[lu_to_optim == 'N'] = 'C'
  lu_to_optim[lu_to_optim == 'A'] = 'R'
  
  # Builds raster of interest areas
  interest_areas = Reduce('+', lu_ras[lu_to_optim %in% optim_type])
  
  # Building master_index of pixels of interest
  master_index = which(values(interest_areas > 0))
  
  # Creating list with raster values corresponding to the master_index
  lu_vals = lapply(lu_ras, function(x){x[master_index]})
  
  # Loading ready variables
  var_ras_names = cfg$variables$variable_raster_names
  
#<<<<<<< HEAD
  var_vals = lapply(paste0(var_dir, var_ras_names[cfg$variables$ready_variables]),
                    function(x){load_raster(x, master_index)})
  names(var_vals) = cfg$variables$variable_names[cfg$variables$ready_variables]
  
  
  # All-variables list -------------------------------------------------------
  # Build list of all variables
  allvar_list = as_list(cfg$variables$variable_names)
  names(allvar_list) = allvar_list
  
  # Include in allvar list the variables already loaded
  allvar_list[names(allvar_list) %in% names(var_vals)] = var_vals
  
  
  # Opportunity cost ---------------------------------------------------------
  # Computing opportunity costs, if: (a) oc is not labeled as ready in
  # $ready_variables, and if (b) the required rasters are in the right folder
  if (!cfg$variables$ready_variables[cfg$variables$variable_names %in%
                                     (cfg$variables$calc_oc$oc_variable_name)] & # condition (a)
      (length(which(dir(var_dir) %in% cfg$variables$calc_oc$oc_files)) ==
      length(cfg$variables$calc_oc$oc_names))){ # condition (b)
    
    source('plangea_calc_oc.R')
    
    # Including opportunity cost into allvar_list
    allvar_list[names(allvar_list) %in% cfg$variables$calc_oc$oc_variable_name]=
      list(plangea_calc_oc(cfg, lu_val_list=lu_vals, master_index=master_index))
    
  } # end of calc_oc if statement

  
  # Ecoregions maps ------------------------------------------------------------
  # Load land-use rasters names
  er_ras_names = dir(er_dir)[dir(er_dir) %in% cfg$landscape_features$original_areas$ecoregions_raster_names]
  
  # Re-ordering to ensure the object created follows order in config file
  er_ras_names = er_ras_names[match(cfg$landscape_features$original_areas$ecoregions_raster_names, er_ras_names)]
  
  # Ecoregion maps required to deal with areas without natural-area remnants
  er_maps = lapply(paste0(er_dir, er_ras_names), function(x){load_raster(x, master_index)})
  names(er_maps) = cfg$landscape_features$original_areas$past_class_names
  
  
  # Original areas (OA) ---------------------------------------------------------
  past_lu_vals = NULL
  
  if (cfg$landscape_features$original_areas$include_past){
    past_names = cfg$landscape_features$original_areas$past_raster_names
    past_lu_vals = lapply(paste0(past_lu_dir, past_names), function(x){load_raster(x, master_index)})
    names(past_lu_vals) = cfg$landscape_features$original_areas$past_class_names
  }
  
  source('plangea_calc_oa.R')
    
  oa_vals = plangea_calc_oa(c_lu_maps = lu_vals, er_maps = er_maps,
                            p_lu_maps = past_lu_vals, lu_type = lu_class_types,
                            tolerance=1.e-7)
    

  # Biodiversity ---------------------------------------------------------
  
  source('plangea_calc_bd.R')
  
  bd = plangea_calc_bd(cfg = cfg, lu_vals = lu_vals,
                       master_index = master_index, oa_vals = oa_vals)
  
  # Including biodiversity benefits into allvar_list
  allvar_list[names(allvar_list) %in% cfg$variables$calc_bd$bd_variable_name] = list(bd)

  return(allvar_list)
    
#=======
  # List _Rdata files saved in dir
  #obj_list = dir(dir, full_names=T, pattern='_Rdata', ignore_case=T)
  # Loads all objects with names in obj_list
#>>>>>>> 82f05d12db99c7fed4f34e3018ec5959b097fe1f
  
} # end of plangea_harmonize function
