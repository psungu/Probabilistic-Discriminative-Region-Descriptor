generate_features <- function(train_data, shapelets, location){
  
  list_shapelets <- Map(function(x, y){c(x, rep(NA, y))}, shapelets, max(lengths(shapelets)) - lengths(shapelets))
  unlist_shapelets <- as.data.frame(t(matrix(unlist(list_shapelets), ncol = length(list_shapelets), nrow = max(lengths(lapply(list_shapelets, unlist))))))
  unlist_shapelets[is.na(unlist_shapelets)] <- -999
  
  shapelets_input <- as.matrix(unlist_shapelets)
  
  len <- rapply(shapelets, length, how="list")
  shapelet_length = as.numeric(len)
  
  distance_matrix <-compute_best_matching_distance(train_data,shapelets_input,shapelet_length, location)
  
  return(distance_matrix)

}





