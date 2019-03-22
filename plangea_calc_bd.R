# para extincoes globais, areas de ocorrencia da especie fora do recorte da
# paisagem sendo otimizada precisam ser computadas e consideradas no calculo do
# risco de extincao, i.e., o usuario precisa entrar com mapas de ocorrencia das
# especies recortados para a paisagem, mas tambem valores agregados de habitat
# atual e potencial totais, o que inclui possiveis areas de ocorrencia fora da
# paisagem. Se tais areas de habitat n forem informadas, o script calculara
# exticoes locais (baseado apenas nos habitats atual e potencial dentro da
# paisagem)

plangea_calc_bd = function(cfg, lu.vals, master_index, oa.vals=NULL){
  
  # Reading scaling factor for bd computation
  g_scalar_bd = cfg$variables$variable_scaling_factor[cfg$variables$variable_names == cfg$variables$calc_bd$bd_variable_name]
  
  # Pointer for native-vegetation classes
  nat.ptr = (cfg$landscape_features$land_use$class_types == "N")
  
  # List of native-vegetation classes
  nat.cls = cfg$landscape_features$land_use$class_names[nat.ptr]
  
  # Generating matrix with proportion to restore for each natural land-use in each pixel
  prop.restore = matrix(unlist(oa.vals), ncol=length(oa.vals))
  
  # Computing all possible combinations of suitable land-use classes
  usphab_proc = gen_usphab(length(which(nat.ptr)))
  
  # Number of rasters in each subfolder defined in the config json
  n.rasters = lapply(cfg$variables$calc_bd$bd_subfolders,
                     function(x){length(dir(paste0(spp.dir,x)))})
  
  # List of raster names, divided by BD-classes / subfolders 
  raster.names = lapply(cfg$variables$calc_bd$bd_subfolders,
                       function(x){dir(paste0(spp.dir,x))})
  names(raster.names) = cfg$variables$calc_bd$bd_classes
  
  # List of species-raster values
  spp.vals = c()
  for (sf in cfg$variables$calc_bd$bd_subfolders){
    spp.vals = c(spp.vals, lapply(dir(paste0(spp.dir,sf), full.names=T),
                                function(x){print(paste0('loading raster ', x));
                                  load_raster(x, master_index=master_index)})) }
  names(spp.vals) = sub(x=unlist(raster.names), pattern='.tif', replacement='')
  
  # Loading list of suitable land-uses for each species
  spp.table = read.csv(paste0(spp.dir, cfg$variables$calc_bd$spp_table$spp_filename))
  
  # Creating list of species ID that have entries in both spp.vals and spp.table
  spid.list = names(spp.vals)[names(spp.vals) %in%
                                unique(spp.table[,names(spp.table) %in%
                                                   cfg$variables$calc_bd$spp_table$spp_names_column])]
  
  # Start present and potential habitats by intersecting bd.vals with corresponding lu.vals
  hab.now.vals = list()
  hab.pot.vals = list()
  
  # Start list of spids corresponding to each unique combination of suitable LUs in usphab_proc
  usphab_index = lapply(1:nrow(usphab_proc), function(x){c()})
  
  # spid loop ----------------------------------------------------------
  for (spid in spid.list){
    # Pointer in the rows of spp.table to select the iteration's spid
    spid.ptr = (spp.table[,names(spp.table) %in% cfg$variables$calc_bd$spp_table$spp_names_column] == spid)
    
    # Pointer in the columns of spp.table to selec the suitable land-uses column
    lu.col.ptr = (names(spp.table) %in% cfg$variables$calc_bd$spp_table$lu_names_column)
    
    # Suitable land-uses for iteration's spid
    spid.lu = unique(spp.table[spid.ptr, lu.col.ptr])
    
    # Keeping in spid.lu only entries listed as native classes in nat.cls
    spid.lu = spid.lu[spid.lu %in% nat.cls]
    
    # Current habitat for spid -----------------------------------------------
    hab.now.vals = c(hab.now.vals, list(spp.vals[names(spp.vals) == spid][[1]] *  # species range for spid
                     Reduce('+', lu.vals[names(lu.vals) %in% spid.lu])))          # sum of suitable lu for spid
    
    # Potential habitat for spid ---------------------------------------------
    hab.pot.vals = c(hab.pot.vals, list(spp.vals[names(spp.vals) == spid][[1]] *  # species range for spid
                     Reduce('+', oa.vals[names(oa.vals) %in% spid.lu])))          # sum of suitable lu for spid
    
    # usphab_index computation ---------------------------------------------    
    # Corresponding entry on usphab_proc rows for spid
    spid.proc = as.numeric(nat.cls %in% spid.lu)
    
    # Identifying which row in usphab_proc corresponds to spid.proc
    spid.proc.row = which(rowSums(t(sapply(1:nrow(usphab_proc),
                                           function(x){usphab_proc[x,]==spid.proc}))) == 5)

    # Adding the spid to the corresponding entry in usphab_index
    usphab_index[spid.proc.row] = list(c(unlist(usphab_index[spid.proc.row]), spid))
  }
  names(hab.now.vals) = spid.list
  names(hab.pot.vals) = spid.list

  # species_index_list_proc computation ----------------------------------
  species_index_list_proc = lapply(spp.vals, function(x){which(x==1)})
  
  #
  np = length(master_index)
  hab.now.areas = sapply(hab.now.vals, sum)
  hab.pot.areas = sapply(hab.pot.vals, sum)
  
  # Points to spids in spid.list that have valid habitat areas
  valid_spid_ptr = (hab.now.areas > 0) & (hab.pot.areas > 0)
  
  # Subsetting output species variables for only species with valid habitats
  usphab_index = lapply(usphab_index, function(x){x[x %in% spid.list[valid_spid_ptr]]})
  species_index_list_proc = species_index_list_proc[valid_spid_ptr]
  hab.now.areas = hab.now.areas[valid_spid_ptr]
  hab.pot.areas = hab.pot.areas[valid_spid_ptr]
  
  bd = calc.bd(calc.extinction.slope(hab.now.areas, hab.pot.areas))
  
  return(list(bd = bd, species_vars=list(usphab_proc=usphab_proc,
                                         usphab_index=usphab_index,
                                         species_index_list_proc=species_index_list_proc)))
}