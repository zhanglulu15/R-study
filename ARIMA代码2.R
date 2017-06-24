library("zoo")
library("forecast")
kzl <- read.csv("E:\\2015bp.csv",header = TRUE)
kzl2 <- kzl[2]
data=read.table('clipboard',T)

kzlts <- ts(kzl2,start=c(2015,01,01),frequency=365) 
#2. 识别模型
#2.1. 查看趋势图
plot.ts(kzlts)
kzldiff <- diff(diff(airts, differences=1)) 
plot.ts(airdiff) #这个是重点，这里的第一个diff是一阶差分，让数据更平稳；第二个diff是对数据去除周期性


#由图可见，该序列还不平稳，先做一次Log平滑，再做一次差分:
kzllog <- log(kzlts)
kzldiff <- diff(kzllog, differences=1)
plot.ts(kzldiff)

#这次看上去就比较平稳了，现在看看ACF和PACF的结果

acf(kzldiff, lag.max=60)
acf(kzldiff, lag.max=60,plot=FALSE)
pacf(airdff, lag.max=30)
pacf(airdff, lag.max=30,plot=FALSE)
