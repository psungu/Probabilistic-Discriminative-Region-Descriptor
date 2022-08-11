generate_shapelets <- function(observations, classes){
  
  #sec<-unlist(lapply(classes,function(x){ind=which(classes==x)}))
  #long_series <- as.data.frame(observations[tid %in% sec,c(2:5),with=F])
  long_series = observations
  shapelets <- list()
  long_series$cls <- as.numeric(long_series$cls)
  
  for(time_knot in c(4,8,16)){
    #tensor = c("cls", "~ti(obs, times, lag, bs = c('cr', 'cc', 'cr'),k=c(5,time_knot,5))")
    #tensor = c("cls", "~ti(obs, times, bs = c('cr', 'cc'),k=c(5,time_knot))")
    tensor = c("cls", "~ti(obs, times, bs = c('tp', 'cc'),k=c(time_knot))")
    #tensor = c("cls", "~ti(obs, times, bs = c('tp', 'tp'),k=c(time_knot))")
    
    #tensor = c("cls", "~ti(obs, times, bs = c('tp', 'cc'),k=c(time_knot)) + ti(diff, times, bs = c('tp', 'cc'),k=c(time_knot))")
    
    default <- list(as.formula(paste(tensor[1], paste(tensor[2]))))
    
    if(length(unique(classes))>2){
      
      x <- replicate(length(unique(classes))-2, as.formula(paste(tensor[2])))
      
      default <- c(default, x)
      
    }
    
    #fmla = 'cls~ti(obs, times, bs = c("tp", "cc"),k=c(time_knot))'
    
    #fit = bam(as.formula(fmla),data=long_series,method="fREML",discrete=T, family=binomial())
    
    #fit <- gam(default,data=long_series,family=multinom(length(unique(classes))-1))
    fit <- gam(default,data=long_series, method = "fREML", family=multinom(length(unique(classes))-1))
    train_predict <- data.frame(predict(fit, long_series, type='response'))
    
    for(threshold in c(0.70,0.80,0.90)){
      
      for(i in names(train_predict)){
        observations[,i] <- train_predict[i]
        observations <- data.frame(observations)
        probability <- observations[which(train_predict[i] >= quantile(as.matrix(train_predict[i]), probs = c(threshold))),]
        for(j in unique(probability$tid)){
          shap <- subset(probability,tid==j)
          len = max(shap$times) - min(shap$times)
          
          list_NA <- rep(NA, len+1)
          
          list_NA[shap$times] <- shap$obs
          
          shapelet<-list_NA[c(min(shap$times):max(shap$times))]
          
          #serie_len <- length(long_series$times)/length(classes)
          
          #if((length(shapelet) < serie_len * 0.5) & (length(which(is.na(shapelet))) < serie_len * 0.3)){
          shapelets <-append(shapelets, list(shapelet))
          #}
          
        }
        rm(probability)
      }
    }
  }
  return(unique(shapelets))
}
