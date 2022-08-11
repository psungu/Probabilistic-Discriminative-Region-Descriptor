compute_best_matching_distance<-function(dataset,shapelets,shapelet_length,location=TRUE){
	similarity=shapeletSimilarity(dataset,shapelets,as.integer(shapelet_length))
	if(location){
		return(cbind(similarity$similarity,similarity$location))
	}else{
		return(similarity$similarity)
	}
}
