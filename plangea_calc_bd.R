# para extincoes globais, areas de ocorrencia da especie fora do recorte da
# paisagem sendo otimizada precisam ser computadas e consideradas no calculo do
# risco de extincao, i.e., o usuario precisa entrar com mapas de ocorrencia das
# especies recortados para a paisagem, mas tambem valores agregados de habitat
# atual e potencial totais, o que inclui possiveis areas de ocorrencia fora da
# paisagem. Se tais areas de habitat n forem informadas, o script calculara
# exticoes locais (baseado apenas nos habitats atual e potencial dentro da
# paisagem)

plangea_calc_bd = function(cfg, lu.vals, master_index, oa.vals=NULL){
  
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
  spid.list = names(spp.vals)[names(spp.vals) %in% unique(spp.table[,names(spp.table) %in% cfg$variables$calc_bd$spp_table$spp_names_column])]
  
  # Compute present and potential habitats by intersecting bd.vals with corresponding lu.vals
  hab.now.vals = list()
  hab.pot.vals = list()
  for (spid in spid.list){
    # Pointer in the rows of spp.table to select the iteration's spid
    spid.ptr = (spp.table[,names(spp.table) %in% cfg$variables$calc_bd$spp_table$spp_names_column] == spid)
    
    # Pointer in the columns of spp.table to selec the suitable land-uses column
    lu.ptr = (names(spp.table) %in% cfg$variables$calc_bd$spp_table$lu_names_column)
    
    # Suitable land-uses for iteration's spid
    spid.lu = unique(spp.table[spid.ptr, lu.ptr])
    
    # Current habitat for spid
    hab.now.vals = c(hab.now.vals, list(spp.vals[names(spp.vals) == spid][[1]] *  # species range for spid
                     Reduce('+', lu.vals[names(lu.vals) %in% spid.lu])))          # sum of suitable lu for spid
    
    # Potential habitat for spid
    hab.pot.vals = c(hab.pot.vals, list(spp.vals[names(spp.vals) == spid][[1]] *  # species range for spid
                     Reduce('+', oa.vals[names(oa.vals) %in% spid.lu])))          # sum of suitable lu for spid
    
  }
  names(hab.now.vals) = spid.list
  names(hab.pot.vals) = spid.list
  
  # Generating matrix with proportion to restore for each natural land-use in each pixel
  prop.restore = matrix(unlist(oa.vals), ncol=5)

  # Computing all possible combinations of suitable land-use classes
  usphab_proc = gen.usphab(length(which(cfg$landscape_features$land_use$class_types == "N")))
  
  # compute usphab_index next
}