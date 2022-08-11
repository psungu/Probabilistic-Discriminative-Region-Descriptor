prepare_data <- function(dataset,label){
  lenseries=ncol(dataset)
  nofdata=nrow(dataset)
  nofclass=length(unique(label))
  
  times=rep(c(1:lenseries),nofdata)
  cls=matrix(as.double(label)-1,nrow=nofdata,ncol=lenseries)
  tid=sort(rep(c(1:nofdata),lenseries))
  dataobs=data.table(tid=tid,times=times,obs=c(t(dataset)),cls=c(t(cls)))
  classes=as.double(label)-1
  
  return(list("data" = dataobs,"class"=classes))
  
}
