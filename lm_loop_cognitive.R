#Initialize dataframe to store p-values
x<-data.frame(matrix(nrow=36,ncol=17))

#Set up names of dataframe 
names(x)[1]<-"Structure"
names(x)[2]<-"Cognitive_Measure"
names(x)[3]<-"cognitive.measure.p"
names(x)[4]<-"age"
names(x)[5]<-"sex"
names(x)[6]<-"TBV"
names(x)[7]<-"APOE"
names(x)[8]<-"Education"
names(x)[9]<-"cognitive.measure.corr"
names(x)[10]<-"age.corr"
names(x)[11]<-"sex.corr"
names(x)[12]<-"TBV.corr"
names(x)[13]<-"APOE.corr"
names(x)[14]<-"Education.corr"
names(x)[15]<-"R2"
names(x)[16]<-"R2.model.w.o.main.variable"
names(x)[17]<-"R2.main.variable"

#Grab structure names from control_data
y<-data.frame(matrix(nrow=12,ncol=1))
y[1,1]<-"Right.CA1"
y[2,1]<-"Right.Subiculum"
y[3,1]<-"Right.CA4.DG"
y[4,1]<-"Right.CA2.CA3"
y[5,1]<-"Right.SR.SL.SM"
y[6,1]<-"Right.Whole"
y[7,1]<-"Left.CA1"
y[8,1]<-"Left.Subiculum"
y[9,1]<-"Left.CA4.DG"
y[10,1]<-"Left.CA2.CA3"
y[11,1]<-"Left.SR.SL.SM"
y[12,1]<-"Left.Whole"

x[1:12,1]<-y
x[13:24,1]<-y
x[25:36,1]<-y

#Fill in Cognitive Measure column
x[1:12,2]<-"List Recall"
x[13:24,2]<-"Figure Recall"
x[25:36,2]<-"LNS"

#Run glm for all 12 stuctures
for (i in 4:15){
a1<-summary(lm(control_memory_data[,i]~control_memory_data$ListRecall_TotalScore+control_memory_data$age+control_memory_data$sex+control_memory_data$beast_TBV+control_memory_data$APOE4_status+control_memory_data$Education))

a2<-summary(lm(control_memory_data[,i]~control_memory_data$FigureRecall_Time_TotalScore+control_memory_data$age+control_memory_data$sex+control_memory_data$beast_TBV+control_memory_data$APOE4_status+control_memory_data$Education))

a3<-summary(lm(control_memory_data[,i]~control_memory_data$LNS+control_memory_data$age+control_memory_data$sex+control_memory_data$beast_TBV+control_memory_data$APOE4_status+control_memory_data$Education))

#Run glm w/o main variable
b<-summary(lm(control_memory_data[,i]~control_memory_data$age+control_memory_data$sex+control_memory_data$beast_TBV+control_memory_data$APOE4_status+control_memory_data$Education))

#Store p-values for structure i, all co-variates
x[i-3,3:8]<-a1$coefficients[2:7,4]
x[i+9,3:8]<-a2$coefficients[2:7,4]
x[i+21,3:8]<-a3$coefficients[2:7,4]

#Store R2s
x[i-3,15]<-a1$adj.r.squared
x[i+9,15]<-a2$adj.r.squared
x[i+21,15]<-a3$adj.r.squared
x[i-3,16]<-b$adj.r.squared
x[i+9,16]<-b$adj.r.squared
x[i+21,16]<-b$adj.r.squared

#Calculate independent R2 for main variable
x[i-3,17]<-x[i-3,15]-x[i-3,16]
x[i+9,17]<-x[i+9,15]-x[i+9,16]
x[i+21,17]<-x[i+21,15]-x[i+21,16]
}

#Correct for multiple comparisons using FDR
for (i in 9:14){
x[,i]<-p.adjust(x[,i-6],method="fdr")
}
