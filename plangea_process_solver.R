plangea_process_solver = function(obj, mat, rhs, bounds, iter_filename){

  result = Rsymphony_solve_LP(obj = obj, mat = mat, dir = rep('<=', nrow(mat)),
                              rhs = rhs, bounds = bounds, max=T)
  names(result)[1] = 'x'
  if (result$status == 0){result$status = "OPTIMAL"}
  
  pigz_save(result, file = paste0(run_dir, iter_filename))
  
  return(result$x)
}