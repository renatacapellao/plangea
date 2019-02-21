# WRAPPER FOR RUNNING PLANGEA BASED ON AN INPUT JSON CONFIG FILE

plangea_wrapper = function(config_json_location='./plangea_config.json'){
  source('plangea_functions.R')
  
  cfg = plangea_read_config(config_json_location)
}