library("zoo")
library("forecast")
dt <- read.csv("E:\\2015bp.csv",header = TRUE)


airline2 <- dt[1:438,2]

airts <- ts(airline2) 
plot.ts(airts)# 转化为时间序列，以2011年2月为起点，12个月周期 

airdiff <- diff(diff(airts, differences=1))
plot.ts(airdiff) #这个是重点，这里的第一个diff是一阶差分，让数据更平稳；第二个diff是对数据去除周期性

adf.test(airdiff, alternative="stationary", k=0)  #检验序列平稳性 用过了检验才可以去测试acf and pcaf  不然。无用功。
#p=0.01<0.05

#adf与pacf的检验，初期定阶
acf(airdiff, lag.max=30)
acf(airdiff, lag.max=30,plot=FALSE)
pacf(airdiff, lag.max=30)
pacf(airdiff, lag.max=30,plot=FALSE)

auto.arima(airdiff,trace=T)#自动定阶

airarima1 <- arima(airline2,order=c(5,1,0),method="ML")

airarima1

airarima2 <- arima(airline2,order=c(5,1,0),seasonal=list(order=c(1,1,0),period=365),method="ML")

airarima2

airarima3 <- arima(airline2,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=365),method="ML")

airarima3
airarima1 <- arima(airline2,order=c(5,1,0)
airforecast <- forecast.Arima(airarima1,h=16,level=c(99.5))

airforecast

plot.forecast(airforecast)   #画预测图
ycforecast <- forecast.Arima(airarima1,h=20)  #预测后面16个
plot.forecast(ycforecast)