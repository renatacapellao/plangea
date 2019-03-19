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

load_raster = function(raster.path, master_index=NULL){
  res = raster(raster.path)
  res[res<0] = 0
  res[is.na(res)] = 0
  if(!is.null(master_index)){res = res[master_index]}
  return(res)
}

# Mandrake troglodita
gen.usphab = function(n){
  res.list = list(c(1, rep(0, n-1)))
  for (i in 2:n){
    res.list = c(res.list, lapply(res.list, function(x){x[i]=x[i]+1; return(x)}) )
  }
  back.list = list(c(rep(0, n)))
  for (i in n:2){
    back.list = c(back.list, lapply(back.list, function(x){x[i]=x[i]+1; return(x)}) )
  }
  res.list = unique(c(res.list, back.list))
  matrix(unlist(res.list[!sapply(res.list, function(x){sum(x)}) == 0]), ncol=n)
}