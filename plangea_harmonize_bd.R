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
    
    # Sources external script for computing extra constraints to habitats
    if (length(cfg$variables$calc_bd$calc_extra_constraints_script) > 1){
      # The input script must add to the envir a function named spid_constr
      # that receives spid as an input and returns a list of master_index size
      # with suitable areas of habitat for that spid in each px of master_index
      source(paste0(scripts_dir, cfg$variables$calc_bd$calc_extra_constraints_script))
    } else {spid_constr = function(spid){1}}
    
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
                       function(x){length(grep_raster_ext(dir(paste0(spp_dir,x))))})
    
    # List of raster names, divided by BD-classes / subfolders 
    raster_names = lapply(cfg$variables$calc_bd$bd_subfolders,
                          function(x){grep_raster_ext(dir(paste0(spp_dir,x)))})
    names(raster_names) = cfg$variables$calc_bd$bd_classes

    # Load rasters loop --------------------------------------------------------
    last_loaded_folder = 1
    spp_terr_range = c()            # occurrence indices w.r.t. (TERR)estrial_index
    spp_main_range = c()            # occurrence indices w.r.t. (MA)ster_(IN)dex
    
    if (file.exists(paste0(in_dir, 'load_spp'))){
      pigz_load(file = paste0(in_dir, 'load_spp'))
      if ((sum(unlist(lapply(present_bd_info, nrow), use.names = F) != unlist(lapply(load_spp$present_bd_info, nrow), nrow,use.names = F)) == 0) &
          (sum(unlist(lapply(present_bd_info, function(x){x$ctime}), use.names = F) >
               unlist(lapply(load_spp$present_bd_info, function(x){x$ctime}), nrow,use.names = F)) == 0) ){ #checks consistency with file info from load_spp
        for (i in 1:length(load_spp)) {assign(names(load_spp)[i], load_spp[[i]])}       # assigns objects in load_spp to the function's env
        }
    }
    
    for (sf in cfg$variables$calc_bd$bd_subfolders[last_loaded_folder:length(cfg$variables$calc_bd$bd_subfolders)]) {
      sf_names = grep_raster_ext(dir(paste0(spp_dir, sf), full.names=T))
      last_loaded_spp = 1
      for (ras_name in sf_names[last_loaded_spp:length(sf_names)]){
        print(paste0('Loading raster ', ras_name,' [', length(spp_terr_range)+1, ' of ',
                     Reduce('+', n_rasters), ' | stored total: ',
                     format(utils::object.size(spp_terr_range) +
                              utils::object.size(spp_main_range),
                            units='auto', standard='SI') , ']'))
        ras_terr = load_raster(ras_name, master_index=terrestrial_index)
        spp_terr_range[[length(spp_terr_range)+1]] = which(ras_terr > 0)
        ras_main = ras_terr[terrestrial_index %in% master_index]
        spp_main_range[[length(spp_main_range)+1]] = which(ras_main > 0)
        #species_index_list_proc[[length(species_index_list_proc)+1]] = ras_terr[terrestrial_index %in% master_index]
        # Save every 100 rasters loaded a log of rasters loaded to allow for resume in case of a crash
        if(last_loaded_spp %% 100 == 0){
          if (verbose){print(paste0('--- Saving intermediate results ---'))}
          load_spp = list(present_bd_info = present_bd_info,
                          last_loaded_folder = last_loaded_folder,
                          last_loaded_spp = last_loaded_spp,
                          spp_terr_range = spp_terr_range,
                          spp_main_range = spp_main_range)
          pigz_save(load_spp_log, file = paste0(in_dir, 'load_spp'))
        }
        last_loaded_spp = last_loaded_spp + 1
      }
      last_loaded_folder = last_loaded_folder + 1
    }
    names(spp_terr_range) = unlist(lapply(raster_names, tools::file_path_sans_ext))
    names(spp_main_range) = unlist(lapply(raster_names, tools::file_path_sans_ext))
    
    # Remove entries from defect rasters
    defect.ptr = (lapply(spp_main_range, length) == 0)
    spp_terr_range = spp_terr_range[!defect.ptr]
    spp_main_range = spp_main_range[!defect.ptr]
    
    # Loading list of suitable land-uses for each species
    spp_table = read.csv(paste0(spp_dir, cfg$variables$calc_bd$spp_table$spp_filename))
    
    # Creating list of species ID that have entries in both spp_terr_range and spp_table
    spid_list = names(spp_terr_range)[names(spp_terr_range) %in%
                                  unique(spp_table[,names(spp_table) %in%
                                                     cfg$variables$calc_bd$spp_table$spp_names_column])]
    
    # Start present and potential habitats by intersecting bd_vals with corresponding lu_terr
    hab_now_terr = list()
    hab_pot_terr = list()
    
    # Start list of spids corresponding to each unique combination of suitable LUs in usphab_proc
    usphab_index = lapply(1:nrow(usphab_proc), function(x){c()})
    
    
    # spid loop ----------------------------------------------------------------
    spid_count = 1
    
    if (file.exists(paste0(in_dir, 'hab_comp'))){
      pigz_load(file = paste0(in_dir, 'hab_comp'))
      if ((sum(unlist(lapply(present_bd_info, nrow), use.names = F) != unlist(lapply(hab_comp$present_bd_info, nrow), nrow,use.names = F)) == 0) &
          (sum(unlist(lapply(present_bd_info, function(x){x$ctime}), use.names = F) >
               unlist(lapply(hab_comp$present_bd_info, function(x){x$ctime}), nrow,use.names = F)) == 0) ){ #checks consistency with file info from hab_comp
        for (i in 1:length(hab_comp)) {assign(names(hab_comp)[i], hab_comp[[i]])}       # assigns objects in hab_comp to the function's env
      }
    }

    for (spid in spid_list[spid_count:length(spid_list)]) {
      if (verbose){
      print(paste0('Computing habitats for species ID ', spid, ' [', spid_count, ' of ', length(spid_list),']'))
      }
      
      # Pointer in the rows of spp_table to select the iteration's spid
      spid_ptr = (spp_table[,names(spp_table) %in% cfg$variables$calc_bd$spp_table$spp_names_column] == spid)
      
      # Pointer in the columns of spp_table to select the suitable land-uses column
      lu_col_ptr = (names(spp_table) %in% cfg$variables$calc_bd$spp_table$lu_names_column)
      
      # Suitable land-uses for iteration's spid
      spid_lu = unique(spp_table[spid_ptr, lu_col_ptr])
      
      # Keeping in spid_lu only entries listed as native classes in nat_cls
      spid_lu = spid_lu[spid_lu %in% nat_cls]
      
      # Reconstruct species terrestrial vector
      spid_terr = rep(0, length(terrestrial_index))
      spid_terr_range = spp_terr_range[names(spp_terr_range) == spid][[1]]
      spid_terr[spid_terr_range] = 1
      
      # Current habitat for spid -----------------------------------------------
      hab_now_terr = c(hab_now_terr, list(spid_terr *                                           # species range for spid
                                            Reduce('+', lu_terr[names(lu_terr) %in% spid_lu]) * # sum of suitable lu for spid
                                            spid_constr(spid)                                   # extra constraints
                                          )) 
      
      
      # Potential habitat for spid ---------------------------------------------
      oa_terr = lu_terr[names(lu_terr) %in% names(oa_vals)] # building original areas to terrestrial_index
      for (nat_lu in names(oa_terr)) { # adding proportion of oa_vals to each native LU in oa_terr
        oa_terr[[nat_lu]][terrestrial_index %in% master_index] = oa_vals[[nat_lu]]
      }

      hab_pot_terr = c(hab_pot_terr,
                       list(spid_terr *                                                   # species range for spid
                              Reduce('+', oa_terr[names(oa_terr) %in% spid_lu]) *         # sum of suitable lu for spid
                              spid_constr(spid)                                           # extra constraints
                            ))
      
      # usphab_index computation -----------------------------------------------    
      # Corresponding entry on usphab_proc rows for spid
      spid_proc = as.numeric(nat_cls %in% spid_lu)
      
      # Identifying which row in usphab_proc corresponds to spid_proc
      spid_proc_row = which(rowSums(t(sapply(1:nrow(usphab_proc),
                                             function(x){usphab_proc[x,]==spid_proc}))) == 5)
      
      # Adding the spid to the corresponding entry in usphab_index
      usphab_index[spid_proc_row] = list(c(unlist(usphab_index[spid_proc_row]), spid))
      
      # Save every 100 habitats computed intermediate results to allow for resume in case of a crash
      if(spid_count %% 100 == 0){
        if (verbose){print(paste0('--- Saving intermediate results ---'))}
        hab_comp = list(present_bd_info = present_bd_info,
                        spid_count = spid_count,
                        hab_now_terr = hab_now_terr,
                        hab_pot_terr = hab_pot_terr,
                        usphab_index = usphab_index)
        pigz_save(load_spp_log, file = paste0(in_dir, 'load_spp'))
      }
      spid_count = spid_count + 1
    } # end of spid_list loop
    names(hab_now_terr) = spid_list
    names(hab_pot_terr) = spid_list
    
    # List of indices in which each species occur, subsetted to master_index
    # (outdated: moved this part to the loop where spp_terr_range is also computed)
    #species_index_list_proc = lapply(spp_terr, function(x){which((x==1) & (terrestrial_index %in% master_index))})
    #species_index_list_proc = lapply(spp_terr_range,
    #                                 function(x){
    #                                   spp_range = rep(0, length(terrestrial_index))
    #                                   spp_range[x] = 1
    #                                   spp_range = spp_range[terrestrial_index %in% master_index]
    #                                   return(which(spp_range==1))
    #                                   }
    #                                 )
    
    hab_now_areas = sapply(hab_now_terr, sum)
    hab_pot_areas = sapply(hab_pot_terr, sum)
    
    # Points to spids in spid_list that have valid habitat areas
    valid_spid_ptr = (hab_now_areas > 0) & (hab_pot_areas > 0)
    
    # Subsetting output species variables for only species with valid habitats
    usphab_index = lapply(usphab_index, function(x){x[x %in% spid_list[valid_spid_ptr]]})
    #species_index_list_proc = species_index_list_proc[valid_spid_ptr]
    spp_main_range = spp_main_range[valid_spid_ptr]
    hab_now_areas = hab_now_areas[valid_spid_ptr]
    hab_pot_areas = hab_pot_areas[valid_spid_ptr]
    
    if (verbose){print('Computing layer for t0 benefits for biodiversity (bd)')}
    bd = calc_bd(slp = calc_extinction_slope(hab_now_areas, hab_pot_areas),
                 prop_restore, usphab_proc, usphab_index,
                 spp_main_range)

    bd_aux = list(bd = bd, usphab_index = usphab_index, 
                  spp_terr_range = spp_terr_range,
                  spp_main_range = spp_main_range,
                  hab_now_areas = hab_now_areas, hab_pot_areas = hab_pot_areas,
                  prop_restore = prop_restore)
    
    pigz_save(bd_aux, file = paste0(in_dir, 'bd_aux'))
        
  } else {
    if (verbose) {cat('Loading biodiversity data \n')}
    bd_aux = pigz_load(paste0(in_dir, 'bd_aux'))
    bd = bd_aux$bd
    usphab_index = bd_aux$usphab_index 
    usphab_proc = bd_aux$usphab_proc
    #species_index_list_proc = bd_aux$species_index_list_proc
    spp_terr_range = bd_aux$spp_terr_range
    spp_main_range = bd_aux$spp_main_range
    hab_now_areas = bd_aux$hab_now_areas
    hab_pot_areas = bd_aux$hab_pot_areas
    prop_restore = bd_aux$prop_restore
    }

  bd_res = list(bd = bd, usphab_index = usphab_index, usphab_proc = usphab_proc,
                spp_terr_range = spp_terr_range, spp_main_range = spp_main_range,
                hab_now_areas = hab_now_areas, hab_pot_areas = hab_pot_areas,
                prop_restore = prop_restore,
                harmonize_log = file_log, update_flag = flag_log)
  
  pigz_save(bd_res, file = paste0(in_dir, 'harmonize_bd'))
    
  return(bd_res)
}