plangea_calc_oa =  function(c_lu_maps, er_maps=NULL, p_lu_maps=NULL, lu_types, tolerance=1.e-7){
  # c_lu_maps: current land-use list of rasters or indexed vectors
  # p_lu_maps: past land-use list of rasters or indexed vectors of "N" types only
  # lu_types: for each lu in the maps above, a single letter:
  #  "N" for natural, "A" for anthropic, "I" for ignore
  
  # If past LU is not available, assume past distribution is the same as current one
  if (is.null(p_lu_maps)) {p_lu_maps = c_lu_maps[lu_types == "N"]}
  
  # If Ecoregions is not available, assume past distribution was flat among natural types
  if (is.null(er_maps)) {
    er_maps = lapply(c_lu_maps[lu_types == "N"],
                     function(x){Reduce('+', c_lu_maps[lu_types == "N"]) / length(which(lu_types == "N"))})
  
  # Making sure p_lu_maps are ordered the same way as c_lu_maps[lu_types=="N"]
  p_lu_maps = p_lu_maps[match(names(c_lu_maps)[lu_types=="N"], names(p_lu_maps))]
  
  # Total current anthropic area in each pixel
  c_anth_map = Reduce('+', c_lu_maps[lu_types == "A"])
  
  # Condition for applying ecoregion classes: no past natural or current anthropic above 95%
  corr_cond = ( (Reduce('+', p_lu_maps)==0) | (c_anth_map>0.95) )
  
  # Proportion of past natural area in each pixel
  p_nat_maps = lapply(p_lu_maps, function(x){x / Reduce('+', p_lu_maps)})

  # Correcting proportions of past natural areas in pixels where corr_cond is verified
  for (i in 1:length(p_nat_maps)) {p_nat_maps[[i]][corr_cond] = er_maps[[i]][corr_cond]}
    
  # Proportion of current anthropic area converted on each natural type
  c_conv_map = lapply(p_nat_maps, function(x){x * c_anth_map})
  
  # Computing original areas
  oa_maps = mapply('+', c_lu_maps[lu_types == "N"], c_conv_map, SIMPLIFY=F)

  # Checking that original areas sum to 1 within given tolerance  
  check_oa = Reduce('+', c(oa_maps, list(Reduce('+', c_lu_maps[lu_types=='I']))))
  n_problems = length(which((check_oa -1) > tolerance))
  if (n_problems > 0) {warning(paste0('OA was computed with ', n_problems,' inconsistencies'))}
  
  # Checking that original areas have no NA
  na_problem = lapply(oa_maps, function(x){length(which(is.na(x)))} != 0)
  if (na_problem) {stop('Original Areas (OA) have NA.')}
  
  return(oa_maps)
}