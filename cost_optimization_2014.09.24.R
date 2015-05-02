# Loop over svm to optimize cost (C) using a linear kernel. Default cost is 1. 
# Check training accuracy and testing accuracy
# This code is specific to the R session in /projects/julie/Masters/data
# svm.model is a part of the package e1071, library libsvm, and is a tool for creating a support vector machine model. 
 
install.packages("e1071") # install package e1071 in R
library(libsvm) # load library libsvm

y<-matrix(, nrow=999, ncol=2) # initialize matrices
x<-matrix(, nrow=10000,ncol=2)

y[,1]<-seq(from=0.0001, to=0.0999, by=0.0001) # sequence from 0 to 0.0999 in increments of 0.0001 (C>0)
x[,1]<-seq(from=0.1, to=1000, by=0.1) # sequence from 0.1 to 1000 in increments of 0.1

for (i in 1:1000) {
	svm.model<-svm(dx~.,data=trainset,kernel="linear",cost=y[i,1],cross=10,scale=TRUE) # cross = number of rounds of cross-validation, scale the data
	y[i,2]<-svm.model$tot.accuracy # store training accuracy in y vector
	svm.pred<-predict(svm.model,testset[,-1]) # apply svm algorithm to testset using predict
	y[i,2]<-table(testset[,1]==svm.pred)["TRUE"]/nrow(testset) # store testing accuracy in y vector
}

for (i in 1:10000) {
	svm.model<-svm(dx~.,data=trainset,kernel="linear",cost=x[i,1],cross=10,scale=TRUE) # cross = number of rounds of cross-validation, scale the data
	x[i,2]<-svm.model$tot.accuracy # store training accuracy in x vector
        svm.pred<-predict(svm.model,testset[,-1]) # apply svm algorithm to testset using predict
	x[i,2]<-table(testset[,1]==svm.pred)["TRUE"]/nrow(testset) # store testing accuracy in x vector
}


for (i in 1:25) {
        svm.model<-svm(dx~.,data=trainset,kernel="linear",cost=z[i,1],cross=10,scale=TRUE) # cross = number of rounds of cross-validation, scale the data
        z[i,2]<-svm.model$tot.accuracy # store training accuracy in y vector
        svm.pred<-predict(svm.model,testset[,-1]) # apply svm algorithm to testset using predict
        z[i,2]<-table(testset[,1]==svm.pred)["TRUE"]/nrow(testset) # store testing accuracy in y vector
}

