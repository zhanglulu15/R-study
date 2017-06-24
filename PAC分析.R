#PCA:构建股票市场指数

#主成分分析(PCA)

prices <- read.csv("E:\\ML_for_Hackers-master\\08-PCA\\data\\stock_prices.csv")
prices[1,]

#数据预处理：利用lubridate包将数据集中的时间戳转换为正确编码的日期变量，用lubridate包里面的ymd函数
library("lubridate")
prices <- transform(prices,Date = ymd(Date))


#使用cast函数，在波浪符左边指定数据源中哪些列作为输出矩阵的行，在波浪符右边指定哪些列作为输出矩阵
#的列，用value来指明输出矩阵中每一个元素的取值
library("reshape")
date.stock.matrix <- cast(prices,Date ~ Stock,value = "Close")

#回到最初的prices数据集，删除那些缺失元素的数据，然后在运行cast函数
prices <- subset(prices,Date!=ymd('2002-02-01'))
prices <- subset(prices,Stock!='DDR')
date.stock.matrix <- cast(prices,Date ~ Stock,value = "Close")


#删除了确实元素的数据之后，我们重新生成了想要的矩阵，使用cor函数来找到这个矩阵中所有数字列之间的相关性，
#然后把相关性矩转换成一个数值向量，并且画一个相关性密度图，以此来获得两个直观认识
#1）相关性的均值 2）低相关性出现的频率
library('ggplot2')
cor.matrix <- cor(date.stock.matrix[,2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)

#得到密度图，大部分相关性是正数，远程PCA适合这份数据,其中fill是填充颜色的
ggplot(data.frame(Correlation = correlations),
       aes(x = Correlation,fill = 1)) + geom_density() + theme(legend.position = 'none')
#PCA
pca <- princomp(date.stock.matrix[,2:ncol(date.stock.matrix)])
pca

#观察第一主成分的荷载量来更加细致的研究它，荷载值告诉我们，它给每一个主成分多大的权重，
#我们通过pca中的princomp对象的loading元素来获得这些信息，提取loading后，可以获得一个大矩阵，
#它告诉恩源数据的25列中每一列有多少信息包含在主成分中，我们只对主成分感兴趣，所以只把
#pca荷载的第一列提取出来

principal.component <- pca$loadings[,1]

 #完成这些之后，我们可以分析荷载的密度图，直观地了解第一主成分是如何形成的

loading <- as.numeric(principal.component)
ggplot(data.frame(loading),aes(x = loading,fill = 1)) + geom_density() +
                               theme(legend.position = 'none')

#目前为止我们已经获得了主成分，接下来可以把数据总结成一列，可以使用predict函数完成直观目标
market.index <- predict(pca)[,1]

#如何才能做到这些预测值的效果了？本列使用道琼斯指数（DJI)来代表
dji.prices <- read.csv("E:\\ML_for_Hackers-master\\08-PCA\\data\\DJI.csv")
dji.prices <- transform(dji.prices,Date = ymd(Date))

#因为使用整个DJI运行的时间比我们预想的要长很多，所以取一个它的子集
dji.prices <- subset(dji.prices,Date > ymd('2001-12-31'))
dji.prices <- subset(dji.prices,Date != ymd('2002-02-01'))

#然后提取DJI中我们感兴趣的部分，也就是每日收盘价格和我们记录过的那些日期，因为他们的顺序和我们先在的
#数据集相反，用rev函数反转即可
dji <- with(dji.prices,rev(Close))
dates <- with(dji.prices,rev(Date))