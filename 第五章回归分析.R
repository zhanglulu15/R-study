#第五章：线性回归模型

library(ggplot2)
ages <- read.csv("E:\\ML_for_Hackers-master\\05-Regression\\data\\longevity.csv")
ggplot(ages,aes(x = AgeAtDeath,fill = factor(Smokes))) + geom_density()+facet_grid(Smokes~.)
                                                      
#均方误差（MSE)的计算,使用73岁作为在不知道如何条件下的最好预测年龄
guess <- 73
with(ages,mean((AgeAtDeath - guess)^2))

#使用其他年龄范围作为预测，查看均方误差
guess.accuracy <- data.frame()
for(guess in seq(63,83,by = 1))
{
  prediction.error <- with(ages,
                           mean((AgeAtDeath - guess)^2))
  guess.accuracy <- rbind(guess.accuracy,
                          data.frame(Guess = guess,
                                     Error = prediction.error))
}

#得到各个年龄对应的误差值，在73岁时最小
ggplot(guess.accuracy,aes(x = Guess,y = Error)) + geom_point() + geom_line()

#使用虚拟变量的回归模型
#R语言计算均方根误差(RMSE)

constant.guess <- with(ages,mean(AgeAtDeath))
with(ages,sqrt(mean((AgeAtDeath - constant.guess)^2)))  #不包含吸烟信息的预测误差（RMSE)

smokers.guess <- with(subset(ages,Smokes == 1),
                      mean(AgeAtDeath))
non.smokers.guess <- with(subset(ages,Smokes == 0),
                          mean(AgeAtDeath))
ages <- transform(ages,
                  NewPrediction = ifelse(Smokes == 0,
                                         non.smokers.guess,
                                         smokers.guess))
with(ages,sqrt(mean((AgeAtDeath - NewPrediction)^2)))  #包含吸烟信息的预测误差(RMSE)

#体重相对身高的散点图,增加了回归线后体重相对于身高的散点图
library("ggplot2")
heights.weights <- read.csv("E:\\ML_for_Hackers-master\\05-Regression\\data\\01_heights_weights_genders.csv",
                            header = TRUE,
                            sep = ",")
ggplot(heights.weights,aes(x = Height,y = Weight)) +
  geom_point() +
  geom_smooth(method = "lm")  #在调用geom_smoth()时指明要用lm方法即可

#线性回归模型
fitted.regression <- lm(Weight ~ Height,
                        data = heights.weights)
#一旦运行了对lm函数的调用，就可以通过调用coef函数来得到回归直线的截距，，coef函数返回将输入和输出结果联系
#在一起的线性模型的系数

coef(fitted.regression)  #得到回归直线的截距
intercept <- coef(fitted.regression)[1]
slope <- coef(fitted.regression)[2]

predict(fitted.regression)

true.values <- with(heights.weights,Weight)
errors <- true.values - predict(fitted.regression)  #预测误差(残差)

#可使用residuals函数替换predict函数来直接获得残差
residuals(fitted.regression)

#为了发现使用线性回归时产生的明显错误，可以把残差和真实数据对赢你画在一幅图中，这里指定which=1
#让R语言只画出了第一个回归诊断点图
plot(fitted.regression,which = 1)

#机器学习使用RMSE评估机器学习算法的效果
x <- 1:10
y <- x^2
fitted.regression <- lm(y ~ x)
errors <- residuals(fitted.regression)


#预测网页流量
top.10000.sites <- read.csv("E:\\ML_for_Hackers-master\\05-Regression\\data\\top_1000_sites.csv",
                            stringsAsFactors = FALSE)

#用ggplot函数获得散点图，几乎所有的数据值都在x轴的附件挤成一束，而只有非常少的数字跳出那一堆数
#主要原因是因为是因为使用了非标准分布数据，数值跨度太大
ggplot(top.10000.sites,aes(x = PageViews,y = UniqueVisitors)) + geom_point()

#观察PageViews本身的分布
ggplot(top.10000.sites,aes(x = PageViews)) +geom_density()  #该密度图同样不可理解

#尝试取log函数
ggplot(top.10000.sites,aes(x = log( PageViews))) + geom_density()  #得到的密度图比较合理

#使用Log变换后的PageViews和UniqueVisitors
ggplot(top.10000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  #加入了一跳回归线

#调用Lm函数来找到定义这条直线斜率和截距的数值
lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors),data = top.10000.sites)

#summary 告我我们的第一件事：对Lm所做的调用，第二件事：残差的分位数,等价于quantile(residuls(lm.fit))
summary(lm.fit)

lm.fit <- lm(log(PageViews) ~ HasAdvertising + log(UniqueVisitors) + InEnglish,
                              data = top.10000.sites)
summary(lm.fit)

#在实践中，当输入容易获得时，值得将所有的输入都包含进一个预测模型，但是当HasAdvert是难以通过程序获得
#时，那么可以将其去掉
lm.fit <- lm(log(PageViewisings) ~ HasAdvert,data = top.10000.sites)
summary(lm.fit)$r.squared  #解释了1%的方差  

lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors),
             data = top.10000.sites)
summary(lm.fit)$r.squared  #解释了46%的方差

lm.fit <- lm(log(PageViews) ~  InEnglish,
             data = top.10000.sites)
summary(lm.fit)$r.squared  #解释了3%的方差

#定义相关性
x <- 1:10
y <- x^2
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
#用cor函数来估计点和线的线性关系
cor(x,y)