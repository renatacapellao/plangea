plangea_calc_oa =  function(c.lu.maps, er.maps, p.lu.maps=NULL, lu.types, tolerance=1.e-7){
  # c.lu.maps: current land-use list of rasters or indexed vectors
  # p.lu.maps: past land-use list of rasters or indexed vectors of "N" types only
  # lu.types: for each lu in the maps above, a single letter:
  #  "N" for natural, "A" for anthropic, "I" for ignore
  if (is.null(p.lu.maps)){p.lu.maps = c.lu.maps[lu.types == "N"]}
  
  # Making sure p.lu.maps are ordered the same way as c.lu.maps[lu.types=="N"]
  p.lu.maps = p.lu.maps[match(names(c.lu.maps)[lu.types=="N"], names(p.lu.maps))]
  
  # Total current anthropic area in each pixel
  c.anth.map = Reduce('+', c.lu.maps[lu.types == "A"])
  
  # Proportion of past natural area in each pixel
  p.nat.maps = lapply(p.lu.maps, function(x){x / Reduce('+', p.lu.maps)})

  # Correcting proportions of past natural areas in pixels where corr.cond is verified
  for (i in 1:length(p.nat.maps)){p.nat.maps[[i]][corr.cond] = er.maps[[i]][corr.cond]}
    
  # Proportion of current anthropic area converted on each natural type
  c.conv.map = lapply(p.nat.maps, function(x){x * c.anth.map})
  
  # Computing original areas
  oa.maps = mapply('+', c.lu.maps[lu.types == "N"], c.conv.map, SIMPLIFY=F)

  # Condition for applying ecoregion classes: no past natural or current anthropic above 95%
  corr.cond = ( (Reduce('+', p.lu.maps)==0) | (c.anth.map>0.95) )
  
  # Checking that original areas sum to 1 within given tolerance  
  check.oa = Reduce('+', c(oa.maps, list(Reduce('+', c.lu.maps[lu.types=='I']))))
  n.problems = length(which((check.oa -1) > tolerance))
  if(n.problems > 0){warning(paste0('OA was computed with ', n.problems,' inconsistencies'))}
  
  # Checking that original areas have no NA
  na.problem = lapply(oa.maps, function(x){length(which(is.na(x)))} != 0)
  
  #if (na.problem){print('a')}
  
  return(oa.maps)
}