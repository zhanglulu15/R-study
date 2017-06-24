library("class")
tamll_train <- read.table(file = "E://��è_Train_1.txt",header = TRUE,sep = ",")
head(tmall_train)
tamll_train$BuyOrNot <- as.factor(tamll_train$BuyOrNot)
tamll_test <- read.table(file = "E://��è_Test_1.txt",header = TRUE,sep = ",")
tamll_test$BuyOrNot <- as.factor(tamll_test$BuyOrNot)
set.seed(123456)
errratio <- vector()
for(i in 1:30){
  KnnFit <- knn(train = tamll_train[,-1],test = tamll_test[,-1],cl = tamll_train[,1],k = i,prob = FALSE)
  ct <- table(tamll_test[,1],KnnFit)
  errratio <- c(errratio,(1-sum(diag(ct))/sum(ct))*100)
}
errratio
plot(errratio,type = "b",xlab = "�������k",ylab = "�����ʣ�%��",main = "��è�ɽ��ͻ�����Ԥ�������")
#�ܽ᣺�ó���k=7ʱ�����������£�Ϊ3.3%����ʱknnfit�� ֵ��ΪԤ��ֵ��ct��ֵ�Ǵ����б���ۺ�

#k_���ڵ������Լ�����ѡ��
#������Ҫ���ж�Ӧ�ã���è�ɽ��ͻ�Ԥ���е���Ҫ����
library("class")
par(mfrow = c(2,2))  #������ͼ��Ϊ�Ĳ���
set.seed(123456)
errratio <- vector()
for(i in 1:30){
  KnnFit <- knn(train = tamll_train[,-1],test = tamll_test[,-1],cl = tamll_train[,1],k=i,prob = FALSE)
  ct <- table(tamll_test[,1],KnnFit)
  errratio <- c(errratio,(1-sum(diag(ct))/sum(ct))*100)
}
plot(errratio,type = "l",xlab = "���ڸ���K",ylab = "�����ʣ�%��",main = "���ڸ���K�������")
errDelteX <- errratio[7]
for(i in -2:-5){
  fit <- knn(train = tamll_train[,c(-1,i)],test = tamll_test[,c(-1,i)],cl = tamll_train[,1],k=7)
  ct <- table(tamll_test[,1],fit)
  errDelteX <- c(errDelteX,(1-sum(diag(ct))/sum(ct))*100)
}
plot(errDelteX,type = "l",xlab = "�޳�����",ylab = "�޳������ʣ�%��",main = "�޳�����������ʣ�k=7)",cex.main = 0.8)
xTitle = c("1:ȫ�����",2�����ѻ�Ծ��")