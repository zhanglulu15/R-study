library("class")
tamll_train <- read.table(file = "E://天猫_Train_1.txt",header = TRUE,sep = ",")
head(tmall_train)
tamll_train$BuyOrNot <- as.factor(tamll_train$BuyOrNot)
tamll_test <- read.table(file = "E://天猫_Test_1.txt",header = TRUE,sep = ",")
tamll_test$BuyOrNot <- as.factor(tamll_test$BuyOrNot)
set.seed(123456)
errratio <- vector()
for(i in 1:30){
  KnnFit <- knn(train = tamll_train[,-1],test = tamll_test[,-1],cl = tamll_train[,1],k = i,prob = FALSE)
  ct <- table(tamll_test[,1],KnnFit)
  errratio <- c(errratio,(1-sum(diag(ct))/sum(ct))*100)
}
errratio
plot(errratio,type = "b",xlab = "近领个数k",ylab = "错判率（%）",main = "天猫成交客户分类预测错判率")
#总结：得出当k=7时，错判率最下，为3.3%，此时knnfit的 值即为预测值，ct的值是错误判别的综合

#k_近邻的适用性及特征选择
#变量重要性判断应用：天猫成交客户预测中的重要变量
library("class")
par(mfrow = c(2,2))  #将整个图分为四部分
set.seed(123456)
errratio <- vector()
for(i in 1:30){
  KnnFit <- knn(train = tamll_train[,-1],test = tamll_test[,-1],cl = tamll_train[,1],k=i,prob = FALSE)
  ct <- table(tamll_test[,1],KnnFit)
  errratio <- c(errratio,(1-sum(diag(ct))/sum(ct))*100)
}
plot(errratio,type = "l",xlab = "近邻个数K",ylab = "错判率（%）",main = "近邻个数K与错判率")
errDelteX <- errratio[7]
for(i in -2:-5){
  fit <- knn(train = tamll_train[,c(-1,i)],test = tamll_test[,c(-1,i)],cl = tamll_train[,1],k=7)
  ct <- table(tamll_test[,1],fit)
  errDelteX <- c(errDelteX,(1-sum(diag(ct))/sum(ct))*100)
}
plot(errDelteX,type = "l",xlab = "剔除变量",ylab = "剔除错判率（%）",main = "剔除变量与错判率（k=7)",cex.main = 0.8)
xTitle = c("1:全体变量",2：消费活跃度")