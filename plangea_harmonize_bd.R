# para extincoes globais, areas de ocorrencia da especie fora do recorte da
# paisagem sendo otimizada precisam ser computadas e consideradas no calculo do
# risco de extincao, i_e_, o usuario precisa entrar com mapas de ocorrencia das
# especies recortados para a paisagem, mas tambem valores agregados de habitat
# atual e potencial totais, o que inclui possiveis areas de ocorrencia fora da
# paisagem_ Se tais areas de habitat n forem informadas, o script calculara
# exticoes locais (baseado apenas nos habitats atual e potencial dentro da
# paisagem)

plangea_harmonize_bd = function(cfg, file_log, flag_log, lu_terr,
                                master_index, terrestrial_index, oa_vals=NULL,
                                verbose=T, force_comp=F){
  
  # Adding 'bd' control flag to flag_log
  flag_log$bd = F
  
  # Stores info on files to be used
  present_bd_info =  lapply(cfg$variables$calc_bd$bd_subfolders,
                            function(x){file.info(dir(paste0(spp_dir,x), full.names = T))})
  
  if (is.null(file_log$bd)) {file_log$bd = present_bd_info}
  
  # Update checks
  # number of files is not the same
  nfiles_check = (sum(unlist(lapply(present_bd_info, nrow), use.names = F) != unlist(lapply(file_log$bd, nrow), nrow,use.names = F)) > 0)
  # creation time of at least one spp raster are not the same
  ctimes_check = (sum(unlist(lapply(present_bd_info, function(x){x$ctime}), use.names = F) >
                        unlist(lapply(file_log$bd, function(x){x$ctime}), nrow,use.names = F)) > 0) 
  # resulting processed data file not found
  rds_check = (!file.exists(paste0(in_dir, 'harmonize_bd')))
  dependencies = flag_log$master
  
  # Adding / updating 'bd' data to file_log (must be done *after* checks)
  file_log$bd = present_bd_info  
  
  if (nfiles_check | ctimes_check | rds_check | dependencies | force_comp) {
    # Modifies control structures to indicate lu_res will be computed
    flag_log$bd = T
    
    # Prints info on why lu_res is being computed
    if (verbose) {cat(paste0('Computing biodiversity layer. Reason(s): \n',
                             ifelse(nfiles_check, 'different number of input files \n', ''),
                             ifelse(ctimes_check, 'newer input files \n', ''),
                             ifelse(rds_check, 'absent rds file \n', ''),
                             ifelse(dependencies, 'dependencies changed \n', ''),
                             ifelse(force_comp, 'because you said so! \n', '')
    ))}
    
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
    spp_terr = c()
    for (sf in cfg$variables$calc_bd$bd_subfolders) {
      spp_terr = c(spp_terr, lapply(dir(paste0(spp_dir,sf), full.names=T),
                                    function(x){print(paste0('Loading raster ', x));
                                      load_raster(x, master_index=terrestrial_index)})) }
    names(spp_terr) = sub(x=unlist(raster_names), pattern='.tif', replacement='')
    
    # Loading list of suitable land-uses for each species
    spp_table = read.csv(paste0(spp_dir, cfg$variables$calc_bd$spp_table$spp_filename))
    
    # Creating list of species ID that have entries in both spp_terr and spp_table
    spid_list = names(spp_terr)[names(spp_terr) %in%
                                  unique(spp_table[,names(spp_table) %in%
                                                     cfg$variables$calc_bd$spp_table$spp_names_column])]
    
    # Start present and potential habitats by intersecting bd_vals with corresponding lu_terr
    hab_now_terr = list()
    hab_pot_terr = list()
    
    # Start list of spids corresponding to each unique combination of suitable LUs in usphab_proc
    usphab_index = lapply(1:nrow(usphab_proc), function(x){c()})
    
    # spid loop ----------------------------------------------------------
    for (spid in spid_list) {
      # Pointer in the rows of spp_table to select the iteration's spid
      spid_ptr = (spp_table[,names(spp_table) %in% cfg$variables$calc_bd$spp_table$spp_names_column] == spid)
      
      # Pointer in the columns of spp_table to select the suitable land-uses column
      lu_col_ptr = (names(spp_table) %in% cfg$variables$calc_bd$spp_table$lu_names_column)
      
      # Suitable land-uses for iteration's spid
      spid_lu = unique(spp_table[spid_ptr, lu_col_ptr])
      
      # Keeping in spid_lu only entries listed as native classes in nat_cls
      spid_lu = spid_lu[spid_lu %in% nat_cls]
      
      # Current habitat for spid -----------------------------------------------
      hab_now_terr = c(hab_now_terr, list(spp_terr[names(spp_terr) == spid][[1]] *  # species range for spid
                                            Reduce('+', lu_terr[names(lu_terr) %in% spid_lu])))          # sum of suitable lu for spid
      
      # Potential habitat for spid ---------------------------------------------
      oa_terr = lu_terr[names(lu_terr) %in% names(oa_vals)] # building original areas to terrestrial_index
      for (nat_lu in names(oa_terr)) { # adding proportion of oa_vals to each native LU in oa_terr
        oa_terr[[nat_lu]][terrestrial_index %in% master_index] = oa_vals[[nat_lu]]
      }

      hab_pot_terr = c(hab_pot_terr,
                       list(spp_terr[names(spp_terr) == spid][[1]] *  # species range for spid
                                            Reduce('+', oa_terr[names(oa_terr) %in% spid_lu]))) # sum of suitable lu for spid
      
      # usphab_index computation -----------------------------------------------    
      # Corresponding entry on usphab_proc rows for spid
      spid_proc = as.numeric(nat_cls %in% spid_lu)
      
      # Identifying which row in usphab_proc corresponds to spid_proc
      spid_proc_row = which(rowSums(t(sapply(1:nrow(usphab_proc),
                                             function(x){usphab_proc[x,]==spid_proc}))) == 5)
      
      # Adding the spid to the corresponding entry in usphab_index
      usphab_index[spid_proc_row] = list(c(unlist(usphab_index[spid_proc_row]), spid))
    }
    names(hab_now_terr) = spid_list
    names(hab_pot_terr) = spid_list
    
    # List of indices in which each species occur, subsetted to master_index ---
    #species_index_list_proc = lapply(spp_terr, function(x){which((x==1) & (terrestrial_index %in% master_index))})
    species_index_list_proc = lapply(spp_terr, function(x){x=x[terrestrial_index %in% master_index]; which(x==1)})
    
    hab_now_areas = sapply(hab_now_terr, sum)
    hab_pot_areas = sapply(hab_pot_terr, sum)
    
    # Points to spids in spid_list that have valid habitat areas
    valid_spid_ptr = (hab_now_areas > 0) & (hab_pot_areas > 0)
    
    # Subsetting output species variables for only species with valid habitats
    usphab_index = lapply(usphab_index, function(x){x[x %in% spid_list[valid_spid_ptr]]})
    species_index_list_proc = species_index_list_proc[valid_spid_ptr]
    hab_now_areas = hab_now_areas[valid_spid_ptr]
    hab_pot_areas = hab_pot_areas[valid_spid_ptr]
    
    bd = calc_bd(slp = calc_extinction_slope(hab_now_areas, hab_pot_areas),
                 prop_restore, usphab_proc, usphab_index,
                 species_index_list_proc)

    bd_aux = list(bd = bd, usphab_index = usphab_index, 
                  species_index_list_proc = species_index_list_proc,
                  hab_now_areas = hab_now_areas, hab_pot_areas = hab_pot_areas,
                  prop_restore = prop_restore)
    
    pigz_save(bd_aux, file = paste0(in_dir, 'bd_aux'))
        
  } else {
    if (verbose) {cat('Loading biodiversity data \n')}
    bd_aux = pigz_load(paste0(in_dir, 'bd_aux'))
    bd = bd_aux$bd
    usphab_index = bd_aux$usphab_index 
    usphab_proc = bd_aux$usphab_proc
    species_index_list_proc = bd_aux$species_index_list_proc
    hab_now_areas = bd_aux$hab_now_areas
    hab_pot_areas = bd_aux$hab_pot_areas
    prop_restore = bd_aux$prop_restore
    }

  bd_res = list(bd = bd, usphab_index = usphab_index, usphab_proc = usphab_proc,
                species_index_list_proc = species_index_list_proc,
                hab_now_areas = hab_now_areas, hab_pot_areas = hab_pot_areas,
                prop_restore = prop_restore,
                harmonize_log = file_log, update_flag = flag_log)
  
  pigz_save(bd_res, file = paste0(in_dir, 'harmonize_bd'))
    
  return(bd_res)
}