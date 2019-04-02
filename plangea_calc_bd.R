# para extincoes globais, areas de ocorrencia da especie fora do recorte da
# paisagem sendo otimizada precisam ser computadas e consideradas no calculo do
# risco de extincao, i_e_, o usuario precisa entrar com mapas de ocorrencia das
# especies recortados para a paisagem, mas tambem valores agregados de habitat
# atual e potencial totais, o que inclui possiveis areas de ocorrencia fora da
# paisagem_ Se tais areas de habitat n forem informadas, o script calculara
# exticoes locais (baseado apenas nos habitats atual e potencial dentro da
# paisagem)

plangea_calc_bd = function(cfg, lu_vals, master_index, oa_vals=NULL){
  
  # Reading scaling factor for bd computation
  g_scalar_bd = cfg$variables$variable_scaling_factor[cfg$variables$variable_names == cfg$variables$calc_bd$bd_variable_name]
  
  # Pointer for native-vegetation classes
  nat_ptr = (cfg$landscape_features$land_use$class_types == "N")
  
  # List of native-vegetation classes
  nat_cls = cfg$landscape_features$land_use$class_names[nat_ptr]
  
  # Generating matrix with proportion to restore for each natural land-use in each pixel
  prop_restore = matrix(unlist(oa_vals), ncol=length(oa_vals))
  
  # Computing all possible combinations of suitable land-use classes
  usphab_proc = gen_usphab(length(which(nat_ptr)))
  
  # Number of rasters in each subfolder defined in the config json
  n_rasters = lapply(cfg$variables$calc_bd$bd_subfolders,
                     function(x){length(dir(paste0(spp_dir,x)))})
  
  # List of raster names, divided by BD-classes / subfolders 
  raster_names = lapply(cfg$variables$calc_bd$bd_subfolders,
                       function(x){dir(paste0(spp_dir,x))})
  names(raster_names) = cfg$variables$calc_bd$bd_classes
  
  # List of species-raster values
  spp_vals = c()
  for (sf in cfg$variables$calc_bd$bd_subfolders){
    spp_vals = c(spp_vals, lapply(dir(paste0(spp_dir,sf), full.names=T),
                                function(x){print(paste0('loading raster ', x));
                                  load_raster(x, master_index=master_index)})) }
  names(spp_vals) = sub(x=unlist(raster_names), pattern='.tif', replacement='')
  
  # Loading list of suitable land-uses for each species
  spp_table = read.csv(paste0(spp_dir, cfg$variables$calc_bd$spp_table$spp_filename))
  
  # Creating list of species ID that have entries in both spp_vals and spp_table
  spid_list = names(spp_vals)[names(spp_vals) %in%
                                unique(spp_table[,names(spp_table) %in%
                                                   cfg$variables$calc_bd$spp_table$spp_names_column])]
  
  # Start present and potential habitats by intersecting bd_vals with corresponding lu_vals
  hab_now_vals = list()
  hab_pot_vals = list()
  
  # Start list of spids corresponding to each unique combination of suitable LUs in usphab_proc
  usphab_index = lapply(1:nrow(usphab_proc), function(x){c()})
  
  # spid loop ----------------------------------------------------------
  for (spid in spid_list){
    # Pointer in the rows of spp_table to select the iteration's spid
    spid_ptr = (spp_table[,names(spp_table) %in% cfg$variables$calc_bd$spp_table$spp_names_column] == spid)
    
    # Pointer in the columns of spp_table to selec the suitable land-uses column
    lu_col_ptr = (names(spp_table) %in% cfg$variables$calc_bd$spp_table$lu_names_column)
    
    # Suitable land-uses for iteration's spid
    spid_lu = unique(spp_table[spid_ptr, lu_col_ptr])
    
    # Keeping in spid_lu only entries listed as native classes in nat_cls
    spid_lu = spid_lu[spid_lu %in% nat_cls]
    
    # Current habitat for spid -----------------------------------------------
    hab_now_vals = c(hab_now_vals, list(spp_vals[names(spp_vals) == spid][[1]] *  # species range for spid
                     Reduce('+', lu_vals[names(lu_vals) %in% spid_lu])))          # sum of suitable lu for spid
    
    # Potential habitat for spid ---------------------------------------------
    hab_pot_vals = c(hab_pot_vals, list(spp_vals[names(spp_vals) == spid][[1]] *  # species range for spid
                     Reduce('+', oa_vals[names(oa_vals) %in% spid_lu])))          # sum of suitable lu for spid
    
    # usphab_index computation ---------------------------------------------    
    # Corresponding entry on usphab_proc rows for spid
    spid_proc = as.numeric(nat_cls %in% spid_lu)
    
    # Identifying which row in usphab_proc corresponds to spid_proc
    spid_proc_row = which(rowSums(t(sapply(1:nrow(usphab_proc),
                                           function(x){usphab_proc[x,]==spid_proc}))) == 5)

    # Adding the spid to the corresponding entry in usphab_index
    usphab_index[spid_proc_row] = list(c(unlist(usphab_index[spid_proc_row]), spid))
  }
  names(hab_now_vals) = spid_list
  names(hab_pot_vals) = spid_list

  # species_index_list_proc computation ----------------------------------
  species_index_list_proc = lapply(spp_vals, function(x){which(x==1)})
  
  np = length(master_index)
  hab_now_areas = sapply(hab_now_vals, sum)
  hab_pot_areas = sapply(hab_pot_vals, sum)
  
  # Points to spids in spid_list that have valid habitat areas
  valid_spid_ptr = (hab_now_areas > 0) & (hab_pot_areas > 0)
  
  # Subsetting output species variables for only species with valid habitats
  usphab_index = lapply(usphab_index, function(x){x[x %in% spid_list[valid_spid_ptr]]})
  species_index_list_proc = species_index_list_proc[valid_spid_ptr]
  hab_now_areas = hab_now_areas[valid_spid_ptr]
  hab_pot_areas = hab_pot_areas[valid_spid_ptr]
  
  bd = calc_bd(slp = calc_extinction_slope(hab_now_areas, hab_pot_areas),
               np, prop_restore, usphab_proc, usphab_index,
               species_index_list_proc, g_scalar_bd)
  
  return(bd)
}