library("class") 
jd_train<-read.csv(file= "E://JData_Action_201602.csv",header=TRUE)
head(jd_train)
jd_train$type<-as.factor(jd_train$type)
jd_test<-read.csv(file= "E:\\JData_Action_201604.csv",header=TRUE)
jd_test$type<-as.factor(jd_test$type)
set.seed(12345678)
errRatio<-vector()   
for(i in 1:30){
  KnnFit<-knn(train=jd_train[,-5],test=jd_test[,-5],cl=jd_train[,5],k=i,prob=FALSE) 
  CT<-table(jd_test[,5],KnnFit) 
  errRatio<-c(errRatio,(1-sum(diag(CT))/sum(CT))*100)    
}

plot(errRatio,type="b",xlab="近邻个数K",ylab="错判率(%)",
     main="天猫成交顾客分类预测中的近邻数K与错判率")