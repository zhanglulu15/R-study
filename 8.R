#ARIMA时间序列预测
kqzl_train <- read.csv("E://空气数据train.csv",header = TRUE)
kqzl_test <- read.csv("E://空气数据test.csv",header = TRUE)
plot(kqzl_test$AQI指数)
plot(kqzl_train$AQI指数)
library("forecast")
kqzl <- ts(kqzl_train[,3],start=c(2014,1),frequency = 365) 
plot(kqzl)


#安装包
install.packages("zoo")
install.packages("forecast")
install.packages("tseries")
install.packages("e1071")
# 载入包
library("e1071")
library("tseries")
library("zoo")
library("forecast")

kqzl_train <- read.csv("E://空气数据train.csv",header = TRUE)
kqzl_test <- read.csv("E://空气数据test.csv",header = TRUE)


airts <- ts(kqzl_train[3])
plot.ts(airts)# 转化为时间序列，以2011年2月为起点，12个月周期




airdiff <- diff(diff(airts, differences=1))
plot.ts(airdiff) #这个是重点，这里的第一个diff是一阶差分，让数据更平稳；第二个diff是对数据去除周期性
adf.test(airdiff, alternative="stationary", k=0)  #检验序列平稳性 用过了检验才可以去测试acf and pcaf  不然。无用功。

#adf与pacf的检验，初期定阶
acf(airdiff, lag.max=50)
acf(airdiff, lag.max=50,plot=FALSE)
pacf(airdiff, lag.max=50)
pacf(airdiff, lag.max=50,plot=FALSE)

auto.arima(airdiff,trace=T)#自动定阶

#根据自动定阶的结果，选择最优的组合
airarima1 <- arima(kqzl_train[3],order=c(5,1,0),method="ML")

airarima1

airarima2 <- arima(kqzl_train[3],order=c(5,1,0),seasonal=list(order=c(1,1,0),period=12),method="ML")

airarima2


airforecast <- forecast.Arima(airarima3,h=16,level=c(99.5))

airforecast

plot.forecast(airforecast)   #画预测图
ycforecast <- forecast.Arima(airarima1,h=16)  #预测后面16个


sub1=dt[56:68,2]-tq[1] #残差
sub1=as.matrix(sub1)  #转换成矩阵


#4个数据循环
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
  
  #这里解释下，样本太少了，莫法交叉寻优，本人亲测，样本多了，就可以寻找最优的Cost and gamma
  #tuned <- tune.svm(V5 ~., data = trainingData,  type="eps-regression" , gamma = 10^(-4,-1), cost = 10^(1:2)) # tune寻优
  #summary (tuned) # to select best gamma and cost  
  
  
  ----------
kqzl_train$AQI指数 <- as.factor(kqzl_train$AQI指数)  
set.seed(123456789)  
svmfit <- tune.svm(AQI指数~.,data = kqzl_train[,-1],type = "C-classification",kernel = "radial",gamma = 10^(-3:2))
best.svm <- svmfit$best.model
summary(best.svm)
kqzl_test <- read.csv("E://空气数据test.csv",header = TRUE)
kqzl_test$AQI指数 <- as.factor(kqzl_test$AQI指数)
ypred <- predict(best.svm,kqzl_test)
compareTable <- cbind(testData[,5], predict(svmfit, testData[,1:4]))  # comparison table
View(compareTable)


yc1=predict(svmfit, t(xindt))
write.csv(yc1,file="C://Users//yelang//Desktop//matlabyewu//股票预测//yc2.csv")#写出数据


#预测后面第1个
h1=sub1[10:13,1]
h1=as.matrix(h1)
row.names(h1)<-c("V1","V2","V3","V4")
th1=predict(svmfit, t(h1))

#预测后面第二个
h2=c(sub1[11:13,1],th1)
h2=as.matrix(h2)
row.names(h2)<-c("V1","V2","V3","V4")
th2=predict(svmfit, t(h2))


#预测后面第三个数
h3=c(sub1[12:13,1],th1,th2)
h3=as.matrix(h3)
row.names(h3)<-c("V1","V2","V3","V4")
th3=predict(svmfit, t(h3))