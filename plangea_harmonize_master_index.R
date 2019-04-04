plangea_harmonize_master_index = function(cfg, lu_res){
  # Unpacking needed quantities from lu_res
  lu_class_types = lu_res$lu_class_types
  lu_ras = lu_res$lu_ras
  
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
  
  save(master_index, file = paste0(in_dir, 'master_index.Rdata'))
  return(master_index)
}