library("class")
kqzl_train <- read.csv("E://��������train.csv",header = TRUE)
head(kqzl_train)
kqzl_train$�����ȼ� < as.factor(kqzl_train$�����ȼ�)
kqzl_test <- read.csv("E://��������test.csv",header = TRUE)
kqzl_test$�����ȼ� <- as.factor(kqzl_test$�����ȼ�)
set.seed(123456789)
errratio <- vector()
for(i in 1:30){
  KnnFit <- knn(train = kqzl_train[,-1,-2],test = kqzl_test[,-1,-2],cl = kqzl_train[,2],k = i,prob = FALSE)
  ct <- table(kqzl_test[,2],KnnFit)
  errratio <- c(errratio,(1-sum(diag(ct))/sum(ct))*100)
}
errratio
plot(errratio,type = "b",xlab = "���ڸ���K",ylab = "�����ʣ�%��",main = "������������Ԥ���еĽ��ڸ���K�������")

#svm�Կ�����������Ԥ��
kqzl_train <- read.csv("E://��������train.csv",header = TRUE)
kqzl_train$�����ȼ� <- as.factor(kqzl_train$�����ȼ�)
library("e1071")
set.seed(123456789)
tobj <- tune.svm(�����ȼ�~.,data = kqzl_train[,-1],type = "C-classification",kernel = "radial",gamma = 10^(-3:2))
plot(tobj,xlab = expression(gamma),ylab = "��ʧ�ͷ�����C",main = "��ͬ��������µ�Ԥ�������",
     nlevels = 10,color.palette = terrain.colors)
best.svm <- tobj$best.model
summary(best.svm)
kqzl_test <- read.csv("E://��������test.csv",header = TRUE)
kqzl_test$�����ȼ� <- as.factor(kqzl_test$�����ȼ�)
ypred <- predict(best.svm,kqzl_test)
(confm <- table(ypred,kqzl_test$�����ȼ�))
(err <- (sum(confm)-sum(diag(confm)))/sum(confm))


#����ع�����Ӧ�ã�������ͬ�����ȼ�������
library("rpart")
library("rpart.plot")
set.seed(123456789)
zldj <- read.csv("E://��������train.csv",header = TRUE)
treefit2 <- rpart(�����ȼ�~.,data = zldj[,-1,],method = "class",parms = list(split = "gini"))
rpart.plot(treefit2,type=1,branch = 0,extra = 2)
printcp(treefit2)
plotcp(treefit2)

#��������
plot(�����ȼ�~PM2.5,data = kqzl_train)