#R loop to perform LOOCV on naive Bayes classifier

temp_result = NULL

for (i in 1:nrow(thickness_under_60)) {
	temp<-naiveBayes(thickness_under_60[-i,3:81926],thickness_under_60[-i,1])
	temp_result[i]<-predict(temp,thickness_under_60[i,1])
	temp_correct[i]<-thickness_under_60[i,1]==temp_result
}
avg_accuracy<-table(thickness_under_60[,1]==temp_result)["TRUE"]/nrow(thickness_under_60)
