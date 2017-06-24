#读取数据
kqzl_train <- read.csv("E://空气数据train.csv",header = TRUE)
kqzl_test <- read.csv("E://空气数据test.csv",header = TRUE)
#画出核密度图
ggplot(kqzl_train,aes(x = AQI指数)) + geom_density()  
#画出直方图
ggplot(kqzl_train,aes(x = AQI指数)) + geom_histogram(binwidth = 30)

summary(kqzl_train)
#主成分分析,注：cor = T的意思是用相关系数进行主成分分析。
kqzl.pr <- princomp(kqzl_train[-1], cor = T)

#观察主成分分析的详细情况
summary(kqzl.pr, loadings = T)
#说明：结果中的Comp.1、Comp.2、Comp.3和Comp.4等是计算出来的主成分，Standard deviation代
#表每个主成分的标准差，Proportion of Variance代表每个主成分的贡献率，Cumulative 
#Proportion代表各个主成分的累积贡献率。每个主成分都不属于X1、X2、X3和X4中的任何一个
#第一主成分、第二主成分、第三主成分和第四主成分都是X1、X2、X3和X4的线性组合，也就是说最原始
#数据的成分经过线性变换得到了各个主成分。然而并不是每个主成分的作用都非常关键，因此，我们只选
#择作用比较关键的几个，一般地，选择累积贡献率达到八成的前几个主成分即可（这个实例中我们选择前
#两个，毕竟第二主成分的贡献率也比较大）。
#接下来，在得到主成分的基础上进行回归也好进行聚类也好，就不再使用原始的X1、X2、X3和X4
#了，而是使用主成分的数据。但现在还没有各个样本的主成分的数据，所以，最后一步就是得到各个样本主成分的数据。


#计算得到各个样本主成分的数据
pca_data <- predict(kqzl.pr)


#一般相关分析,相关关系是指两个变量的数值变化存在的不完全确定的依存关系
#他们之间的数值不能用方程表示出来，但是可以用某种相关性度量来刻画。进行相关分析的代码如下：
y1 <- kqzl_train$AQI指数
x1 <- kqzl_train$PM2.5
x2 <- kqzl_train$PM10
x3 <- kqzl_train$No2
x4 <- kqzl_train$So2
x5 <- kqzl_train$Co
x7 <- kqzl_train$O3
y2 <- kqzl_train$质量等级

test<-data.frame(x1,x2,x3,x4,x5,y1,y2)
pairs(test)
cor.test(x1,y1)
cor.test(x2,y1)
cor.test(x3,y1)
cor.test(x4,y1)
cor.test(x5,y1)

cor.test(x1,y2)
cor.test(x2,y2)
cor.test(x3,y2)
cor.test(x4,y2)
cor.test(x5,y2)
