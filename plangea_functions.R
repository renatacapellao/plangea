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
  
  # Including overall projected area
  projected_areas = rbind(projected_areas, c(nrow(projected_areas)+1, 0, 0) )
  projected_areas$total[nrow(projected_areas)] = sum(projected_areas$total)
  
  return(projected_areas)
}

calc_objective_function = function(var_list, type_list){
  b = Reduce('+', var_list[type_list=="B"])
  c = Reduce('+', var_list[type_list=="C"])
  # b = Reduce('+', lapply(type_list=="B", function(x){var_list[x] * wgt_list[x]}))
  # c = Reduce('+', lapply(type_list=="C", function(x){var_list[x] * wgt_list[x]}))
  if (is.null(b)){b = c * (-c + abs(min(-c)) + 1)}
  if (is.null(c)){c=1}
  return(b/c)
}

load_raster = function(raster_path, master_index=NULL){
  res = raster(raster_path)
  res[res<0] = 0
  res[is.na(res)] = 0
  if(!is.null(master_index)){res = res[master_index]}
  return(res)
}

pigz_save <- function(object, file, threads=parallel::detectCores()) {
  con <- pipe(paste0("pigz -1 -p", threads, " > ", file), "wb")
  saveRDS(object, file = con)
  close(con)
}

pigz_load <-function(file, threads=parallel::detectCores()) {
  con <- pipe(paste0("pigz -d -c -p", threads, " ", file))
  object <- readRDS(file = con)
  close(con)
  return(object)
}

gen_usphab = function(n){
  res = as.matrix(expand.grid(replicate(n, 0:1, simplify = F)), ncol=n)
  res = res[rowSums(res == rep(0,n))!=5,] # removing solution with all zeroes
}

gen_wgt_list = function(in_wgts){
  n_vars = length(in_wgts)
  n_iter = Reduce('*', lapply(in_wgts, length))

}

plot_vals = function(x_vals, base_ras, master_index){
  #base_ras[!is.na(base_ras)] = 0
  base_ras[master_index] = x_vals
  plot(base_ras)
}

spplot_vals = function(x_vals, base_ras, master_index){
  #base_ras[!is.na(base_ras)] = 0
  res = stack(sapply(x_vals, function(x){y=base_ras; y[master_index] = x; return(y)}))
  spplot(res)
}


# Legacy update the habitat proportions ----------------------------------------
# be careful with the matrix multiplications here... easy to mess this up
update_hab = function(hab_now_areas, restored_area, prop_restore, usphab_proc,
                      usphab_index, species_index_list_proc){

  # species IDs in usphab_index
  id = unlist(usphab_index)
  
  # allocate that restoration to habitat types
  delta_vegtype_pu <- prop_restore * restored_area
  
  # convert the change in habitat per pu to change in habitat per species
  delta_hab_spp <- rep(0, length(id))
  for (i in 1:length(id)){
    id_lu = usphab_proc[sapply(usphab_index, function(x){as.logical(sum(x==id[i]))}),]
    delta_hab_spp[i] = delta_hab_spp[i] + (delta_vegtype_pu %*% id_lu)
  }
  
  return(hab_now_areas + delta_hab_spp)
}

# Legacy calc_bd ------------------------------------------------------------
calc_bd <- function(slp, prop_restore, usphab_proc, usphab_index,
                    species_index_list_proc, restored_area = NULL){
  bd <- rep(0, nrow(prop_restore))
  if(!is.null(restored_area)){prop_restore = prop_restore - (restored_area/ncol(prop_restore))}
  for (i in 1:dim(usphab_proc)[1]){
    hab_values <- prop_restore %*% usphab_proc[i,]
    if(length(usphab_index[[i]])>0){
      for (j in 1:length(usphab_index[[i]])){
        #print(paste(i, j, length(bd), length(bd[recs]+slp[usphab_index[[i]][j]]*hab_values[recs])))
        if (!is.null(species_index_list_proc[[usphab_index[[i]][j]]])){
          recs <- species_index_list_proc[[usphab_index[[i]][j]]]
          bd[recs] = bd[recs] + slp[usphab_index[[i]][j]] * hab_values[recs]
        }
      }      
    }
  }
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