# PLANGEA FUNCTIONS -------------------------------------------------------


library(raster)
library(Rsymphony)
library(Matrix)
library(jsonlite)
library(assertthat)

calc_sparable_area = function(projected_areas){
  # Computing overall sparable land vs demanded land
  dem.lnd = sum(projected_areas$total[projected_areas$total>0])
  spa.lnd = sum(projected_areas$total[projected_areas$total<=0])
  
  # Correcting limits to restoration to account for compensation of demanded area
  projected_areas$total[projected_areas$total>0] = 0
  projected_areas$total = projected_areas$total * ((dem.lnd+spa.lnd)/spa.lnd)
  projected_areas$total = -projected_areas$total
  
  return(projected_areas)
}

calc_objective_function = function(var.list, type.list){
  b = Reduce('+', var.list[type.list=="B"])
  c = Reduce('+', var.list[type.list=="C"])
  return(b/c)
}