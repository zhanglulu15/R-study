library("class")
kqzl_train <- read.csv("E://空气数据train.csv",header = TRUE)
head(kqzl_train)
kqzl_train$质量等级 < as.factor(kqzl_train$质量等级)
kqzl_test <- read.csv("E://空气数据test.csv",header = TRUE)
kqzl_test$质量等级 <- as.factor(kqzl_test$质量等级)
set.seed(123456789)
errratio <- vector()
for(i in 1:30){
  KnnFit <- knn(train = kqzl_train[,-1,-2],test = kqzl_test[,-1,-2],cl = kqzl_train[,2],k = i,prob = FALSE)
  ct <- table(kqzl_test[,2],KnnFit)
  errratio <- c(errratio,(1-sum(diag(ct))/sum(ct))*100)
}
errratio
plot(errratio,type = "b",xlab = "近邻个数K",ylab = "错判率（%）",main = "空气质量分类预测中的近邻个数K与错判率")

#svm对空气质量进行预测
kqzl_train <- read.csv("E://空气数据train.csv",header = TRUE)
kqzl_train$质量等级 <- as.factor(kqzl_train$质量等级)
library("e1071")
set.seed(123456789)
tobj <- tune.svm(质量等级~.,data = kqzl_train[,-1],type = "C-classification",kernel = "radial",gamma = 10^(-3:2))
plot(tobj,xlab = expression(gamma),ylab = "损失惩罚函数C",main = "不同参数组合下的预测错误率",
     nlevels = 10,color.palette = terrain.colors)
best.svm <- tobj$best.model
summary(best.svm)
kqzl_test <- read.csv("E://空气数据test.csv",header = TRUE)
kqzl_test$质量等级 <- as.factor(kqzl_test$质量等级)
ypred <- predict(best.svm,kqzl_test)
(confm <- table(ypred,kqzl_test$质量等级))
(err <- (sum(confm)-sum(diag(confm)))/sum(confm))


#分类回归树的应用，提炼不同天气等级的特征
library("rpart")
library("rpart.plot")
set.seed(123456789)
zldj <- read.csv("E://空气数据train.csv",header = TRUE)
treefit2 <- rpart(质量等级~.,data = zldj[,-1,],method = "class",parms = list(split = "gini"))
rpart.plot(treefit2,type=1,branch = 0,extra = 2)
printcp(treefit2)
plotcp(treefit2)

#分析数据
plot(质量等级~PM2.5,data = kqzl_train)