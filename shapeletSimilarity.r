shapeletSimilarity <- function(series, shapelets, lenshapelet, location=TRUE, ...) 
{
	nseries=nrow(series)
	lenseries=ncol(series)
	nshapelet=nrow(shapelets)
	R.Version()$arch
	if(Sys.info()["sysname"] =="Windows"){
		if(R.Version()$arch == "x86_64"){
			adress ="shapeletSimilarity64.dll"
		}else{
			adress ="shapeletSimilarity32.dll"
		}
	}else{
		if(R.Version()$arch == "x86_64"){
			adress ="shapeletSimilarity64.so"
		}else{
			adress ="shapeletSimilarity32.so"
		}
	}
	dyn.load(adress)
	similarity <- .C("compute_shapelet_distance", 
		as.double(t(series)),
		as.double(t(shapelets)), 
		as.integer(nseries), 
		as.integer(lenseries),
		as.integer(nshapelet),
		as.integer(lenshapelet),
		as.integer(max(lenshapelet)),
		result = double(nseries*nshapelet),
		location=integer(nseries*nshapelet))
		out <- list(similarity=matrix(similarity$result,nrow=nseries),
					location=matrix(similarity$location,nrow=nseries))
		return(out)
}
