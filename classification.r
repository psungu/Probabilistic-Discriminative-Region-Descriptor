classification <- function(train_features, train_classes, test_features, test_classes){
  
 # model=randomForest(train_features,train_classes,ntree=200, do.trace=20)
  model=randomForest(train_features,train_classes,ntree=200)
  varImpPlot(model,type=2,n.var=20)

 # model = ranger(x=train_features, y=train_classes,classification=T,verbose=T)
  
  predictions=predict(model, test_features)
  predictions_train=predict(model, train_features)
  
  
  train_accuracy=sum(as.character(predictions_train)==as.character(train_classes))/length(train_classes)
  
  oob=mean(model$err.rate[,1])
 # oob=prediction.error
 
  test_accuracy=sum(as.character(predictions)==as.character(test_classes))/length(test_classes)

  print(paste0("train_accuracy: ", round(train_accuracy, digits = 3)))
  print(paste0("test_accuracy: ", round(test_accuracy, digits = 3)))
  print(paste0("OOB: ", round(oob, digits=3)))
  
  return(c(round(train_accuracy, digits = 3),round(test_accuracy, digits = 3), round(mean(model$err.rate[,1]), digits=3)))
}
