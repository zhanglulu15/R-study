#重要变量判断
library("class")
set.seed(123456789)
errratio <- vector()
for(i in 1:30){
  KnnFit <- knn(train = kqzl_train[,-1,-2],test = kqzl_test[,-1,-2],cl = kqzl_train[,2],
                k=i,prob = FALSE)
  ct <- table(kqzl_test[,2],KnnFit)
  errratio <- c(errratio,(1-sum(diag(ct))/sum(ct))*100)
}
plot(errratio,type = "l",xlab = "近邻个数K",ylab = "错判率（%）",main = "近邻数K与错判率")
errdeltex <- errratio[5]
for(i in -3:-10){
  fit <- knn(train = kqzl_train[,c(-2,i)],test = kqzl_test[,c(-2,i)],cl = kqzl_train[,2],k=5)
  ct <- table(kqzl_test[,2],fit)
  errdeltex <- c(errdeltex,(1-sum(diag(ct))/sum(ct))*100)
}
#基于袋装技术的应用
ct1 <- rpart.control(minsplit = 20,maxcompete = 6,maxdepth = 30,method = "class",parms = list(split = "gini"))
set.seed(123456789)
treefit <- rpart(质量等级~.,data = zldj[,-1],method = "class",parms = list(split = "gini"))
cfit1 <- predict(treefit,质量等级,type = "class")
confm1 <- table(zldj$质量等级,cfit1)
e1 <- (sum(confm1)-sum(diag(confm1)))/sum(confm1)
