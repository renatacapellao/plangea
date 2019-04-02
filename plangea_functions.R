# PLANGEA FUNCTIONS -------------------------------------------------------


library(raster)
library(Rsymphony)
library(Matrix)
library(jsonlite)
library(assertthat)

calc_sparable_area = function(projected_areas){
  # Computing overall sparable land vs demanded land
  dem_lnd = sum(projected_areas$total[projected_areas$total>0])
  spa_lnd = sum(projected_areas$total[projected_areas$total<=0])
  
  # Correcting limits to restoration to account for compensation of demanded area
  projected_areas$total[projected_areas$total>0] = 0
  projected_areas$total = projected_areas$total * ((dem_lnd+spa_lnd)/spa_lnd)
  projected_areas$total = -projected_areas$total
  
  return(projected_areas)
}

calc_objective_function = function(var_list, type_list){
  b = Reduce('+', var_list[type_list=="B"])
  c = Reduce('+', var_list[type_list=="C"])
  return(b/c)
}

load_raster = function(raster_path, master_index=NULL){
  res = raster(raster_path)
  res[res<0] = 0
  res[is.na(res)] = 0
  if(!is.null(master_index)){res = res[master_index]}
  return(res)
}

gen_usphab = function(n){
  res = as.matrix(expand.grid(replicate(n, 0:1, simplify = F)), ncol=n)
  res = res[rowSums(res == rep(0,n))!=5,] # removing solution with all zeroes
}

# Legacy calc_bd ------------------------------------------------------------
calc_bd <- function(slp, np, prop_restore, usphab_proc, usphab_index,
                    species_index_list_proc, g_scalar_bd){
  bd <- rep(0, np)
  for (i in 1:dim(usphab_proc)[1]){
    hab_values <- prop_restore %*% usphab_proc[i,]
    if(length(usphab_index[[i]])>0){
      for (j in 1:length(usphab_index[[i]])){
        if (!is.null(species_index_list_proc[[usphab_index[[i]][j]]])){
          recs <- species_index_list_proc[[usphab_index[[i]][j]]]
          bd[recs] <- bd[recs] + slp[usphab_index[[i]][j]] * hab_values[recs]
        }
      }      
    }
  }
  bd <- bd * g_scalar_bd
  return(bd)
}

# Legacy computation of extinction risk --------------------------------------
# function to calculate extinction risk
# A: current area
# Amax: maximum potential area
extinction_risk <- function(A, Amax, z=0.25){
  r <- 1 - (A/Amax)^z
  recs <- which(r < 0)
  if (length(recs) > 0) r[recs] <- 0
  return(r)
}

# Legacy computation of extinction risk slope --------------------------------
calc_extinction_slope <- function(A, Amax, z=0.25){
  er1 <- extinction_risk(A, Amax, z=z)
  er2 <- extinction_risk(A+1E-6, Amax, z=z)
  res <- (er2 - er1) / 1E-6	
  return(abs(res))
}