#��Ҫ�����ж�
library("class")
set.seed(123456789)
errratio <- vector()
for(i in 1:30){
  KnnFit <- knn(train = kqzl_train[,-1,-2],test = kqzl_test[,-1,-2],cl = kqzl_train[,2],
                k=i,prob = FALSE)
  ct <- table(kqzl_test[,2],KnnFit)
  errratio <- c(errratio,(1-sum(diag(ct))/sum(ct))*100)
}
plot(errratio,type = "l",xlab = "���ڸ���K",ylab = "�����ʣ�%��",main = "������K�������")
errdeltex <- errratio[5]
for(i in -3:-10){
  fit <- knn(train = kqzl_train[,c(-2,i)],test = kqzl_test[,c(-2,i)],cl = kqzl_train[,2],k=5)
  ct <- table(kqzl_test[,2],fit)
  errdeltex <- c(errdeltex,(1-sum(diag(ct))/sum(ct))*100)
}
#���ڴ�װ������Ӧ��
ct1 <- rpart.control(minsplit = 20,maxcompete = 6,maxdepth = 30,method = "class",parms = list(split = "gini"))
set.seed(123456789)
treefit <- rpart(�����ȼ�~.,data = zldj[,-1],method = "class",parms = list(split = "gini"))
cfit1 <- predict(treefit,�����ȼ�,type = "class")
confm1 <- table(zldj$�����ȼ�,cfit1)
e1 <- (sum(confm1)-sum(diag(confm1)))/sum(confm1)