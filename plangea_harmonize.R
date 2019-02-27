
plangea_preprocess = function(config){
  # set the input directory from which to load all the data for the optimisation
  dir <- config$io$preprocessed_data_location_name
  
  # List .Rdata files saved in dir
  obj.list = dir(dir, full.names=T, pattern='.Rdata', ignore.case=T)
  
  # Loads all objects with names in obj.list
  for (i in obj.list) { load(i) }
  
}
