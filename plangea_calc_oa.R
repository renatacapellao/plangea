plangea_calc_oa =  function(c.lu.maps, p.lu.maps=NULL, lu.types, tolerance=1.e-6){
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
  p.nat.map = lapply(p.lu.maps, function(x){x / Reduce('+', p.lu.maps)})
  
  # Proportion of current anthropic area converted on each natural type
  c.conv.map = lapply(p.nat.map, function(x){x * c.anth.map})
  
  oa.maps = mapply('+', c.lu.maps[lu.types == "N"], c.conv.map, SIMPLIFY=F)
  
  check.oa = Reduce('+', c(oa.maps, list(Reduce('+', c.lu.maps[lu.types=='I']))))
  
  n.problems = length(which((check.oa -1) > tolerance))
  
  if(n.problems > 0){warning(paste0('OA was computed with ',n.problems,' inconsistencies'))}
  
  return(oa.maps)
}