# FUNCTION FOR PREPARING CONFIGURATIONS FROM AN INPUT JSON FILE

plangea_prepare_config = function(config_json_location){
  library(jsonlite)
  config=fromJSON(config_json_location)
}