import_data <- function(dname,filedir = '',train_label=TRUE,test_label=TRUE){
	trainfile=sprintf("%s%s/%s_TRAIN.txt",filedir,dname,dname)
    testfile=sprintf("%s%s/%s_TEST.txt",filedir,dname,dname)
	traindata=as.matrix(fread(trainfile))
    testdata=as.matrix(fread(testfile))
	class_train=as.factor(traindata[,1])
    class_test=as.factor(testdata[,1])
	traindata=traindata[,-1]
	testdata=testdata[,-1]
	return(list("train_data" = traindata,"train_labels"= class_train,"test_data" = testdata,"test_labels"=class_test))
}