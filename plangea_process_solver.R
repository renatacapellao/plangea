plangea_process_solver = function(obj, mat, rhs, ub, iter_filename){
  lb = rep(0, length(ub))
  ub[ub < lb] = lb[ub < lb]
  bounds = list(lower = list(ind=1:length(ub), val=lb),
                upper = list(ind=1:length(ub), val=ub))
  
  result = Rsymphony_solve_LP(obj = obj, mat = mat, dir = rep('<=', nrow(mat)),
                              rhs = rhs, bounds = bounds, max=T)
  names(result)[1] = 'x'
  if (result$status == 0){result$status = "OPTIMAL"}
  
  #pigz_save(result, file = paste0(run_dir, iter_filename))
  
  return(result$x)
}