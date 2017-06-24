#ARIMAʱ������Ԥ��
kqzl_train <- read.csv("E://��������train.csv",header = TRUE)
kqzl_test <- read.csv("E://��������test.csv",header = TRUE)
plot(kqzl_test$AQIָ��)
plot(kqzl_train$AQIָ��)
library("forecast")
kqzl <- ts(kqzl_train[,3],start=c(2014,1),frequency = 365) 
plot(kqzl)


#��װ��
install.packages("zoo")
install.packages("forecast")
install.packages("tseries")
install.packages("e1071")
# �����
library("e1071")
library("tseries")
library("zoo")
library("forecast")

kqzl_train <- read.csv("E://��������train.csv",header = TRUE)
kqzl_test <- read.csv("E://��������test.csv",header = TRUE)


airts <- ts(kqzl_train[3])
plot.ts(airts)# ת��Ϊʱ�����У���2011��2��Ϊ��㣬12��������




airdiff <- diff(diff(airts, differences=1))
plot.ts(airdiff) #������ص㣬����ĵ�һ��diff��һ�ײ�֣������ݸ�ƽ�ȣ��ڶ���diff�Ƕ�����ȥ��������
adf.test(airdiff, alternative="stationary", k=0)  #��������ƽ���� �ù��˼���ſ���ȥ����acf and pcaf  ��Ȼ�����ù���

#adf��pacf�ļ��飬���ڶ���
acf(airdiff, lag.max=50)
acf(airdiff, lag.max=50,plot=FALSE)
pacf(airdiff, lag.max=50)
pacf(airdiff, lag.max=50,plot=FALSE)

auto.arima(airdiff,trace=T)#�Զ�����

#�����Զ����׵Ľ����ѡ�����ŵ����
airarima1 <- arima(kqzl_train[3],order=c(5,1,0),method="ML")

airarima1

airarima2 <- arima(kqzl_train[3],order=c(5,1,0),seasonal=list(order=c(1,1,0),period=12),method="ML")

airarima2


airforecast <- forecast.Arima(airarima3,h=16,level=c(99.5))

airforecast

plot.forecast(airforecast)   #��Ԥ��ͼ
ycforecast <- forecast.Arima(airarima1,h=16)  #Ԥ�����16��


sub1=dt[56:68,2]-tq[1] #�в�
sub1=as.matrix(sub1)  #ת���ɾ���


#4������ѭ��
xindt=matrix(NA,4,length(sub1)-6)
for (i in 1: 10){
  
  xindt[,i]=c(sub1[i],sub1[i+1],sub1[i+2],sub1[i+3])
  
}



inputData<-cbind(t(xindt[,1:9]),sub1[5:13])

set.seed(100) # for reproducing results
rowIndices <- 1 : nrow(inputData) # prepare row indices
sampleSize <- 0.8 * length(rowIndices) # training sample size
trainingRows <- sample (rowIndices, sampleSize) # random sampling
trainingData <- inputData[trainingRows, ] # training data
testData <- inputData[-trainingRows, ] # test data


----------
  
  #��������£�����̫���ˣ�Ī������Ѱ�ţ������ײ⣬�������ˣ��Ϳ���Ѱ�����ŵ�Cost and gamma
  #tuned <- tune.svm(V5 ~., data = trainingData,  type="eps-regression" , gamma = 10^(-4,-1), cost = 10^(1:2)) # tuneѰ��
  #summary (tuned) # to select best gamma and cost  
  
  
  ----------
kqzl_train$AQIָ�� <- as.factor(kqzl_train$AQIָ��)  
set.seed(123456789)  
svmfit <- tune.svm(AQIָ��~.,data = kqzl_train[,-1],type = "C-classification",kernel = "radial",gamma = 10^(-3:2))
best.svm <- svmfit$best.model
summary(best.svm)
kqzl_test <- read.csv("E://��������test.csv",header = TRUE)
kqzl_test$AQIָ�� <- as.factor(kqzl_test$AQIָ��)
ypred <- predict(best.svm,kqzl_test)
compareTable <- cbind(testData[,5], predict(svmfit, testData[,1:4]))  # comparison table
View(compareTable)


yc1=predict(svmfit, t(xindt))
write.csv(yc1,file="C://Users//yelang//Desktop//matlabyewu//��ƱԤ��//yc2.csv")#д������


#Ԥ������1��
h1=sub1[10:13,1]
h1=as.matrix(h1)
row.names(h1)<-c("V1","V2","V3","V4")
th1=predict(svmfit, t(h1))

#Ԥ�����ڶ���
h2=c(sub1[11:13,1],th1)
h2=as.matrix(h2)
row.names(h2)<-c("V1","V2","V3","V4")
th2=predict(svmfit, t(h2))


#Ԥ������������
h3=c(sub1[12:13,1],th1,th2)
h3=as.matrix(h3)
row.names(h3)<-c("V1","V2","V3","V4")
th3=predict(svmfit, t(h3))