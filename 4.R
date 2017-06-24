library("arules")
library("arulesViz")
data <- read.table(file = "E://购物篮数据.txt",header = TRUE,sep = ",")
data <- as.matrix(data[,-1:-7])
mytrans <- as(data,"transactions")
summary(mytrans)

myrules <- apriori(data = mytrans,parameter = list(support = 0.1,
                                                   confidence = 0.5,target = "rules"))
plot(myrules,method = "graph",control = list(arrowSize = 2,main = "连滞商品可视化结果"))

#顾客选择倾向对比
Data<-read.table(file="E://购物篮数据.txt",header=TRUE,sep=",")
Data<-Data[,c(4,7,14)]
Data$beer<-factor(Data$beer)
Data[,2]<-sapply(Data[,2],FUN=function(x){
  if(x %in% 0:29) x<-1 else
    if(x %in% 30:49) x<-2 else
      if(x %in% 50:59) x<-3})
Data$age<-factor(Data$age)
MyTrans<-as(Data,"transactions")
MyRules<-apriori(data=MyTrans,parameter=list(support=0.01,confidence=0.2,minlen=2,target="rules"),
                 appearance=list(rhs=c("beer=1"),
                                 lhs=c("age=1","age=2","age=3","sex=M","sex=F"),
                                 default="none"))
supersetf <- is.subset(MyRules,MyRules)   #判断是否存在冗余规则
inspect(MyRules[-which(colSums(supersetf) > 1)])  #浏览冗余规则
MyRules <- subset(x = MyRules,subset = quality(MyRules)$lift > 1)
plot(MyRules,method = "graph",control = list(arrowSize = 2,main = "性别与年龄的啤酒选择倾向对比"))

#发现数据的时序关联性
library("arulesSequences")
mytrans<-read_baskets(con="E://事务序列原始数据.txt",sep=",",info=c("sequenceID","eventID"))
MyFsets <- cspade(data = mytrans,parameter = list(support = 0.5))  #SPADE算法
inspect(MyFsets)
MyRules<-ruleInduction(x=MyFsets,confidence=0.3)  
MyRules.DF<-as(MyRules,"data.frame")  
MyRules.DF[MyRules.DF$lift>=1,]

#序列关联分析的应用：发现网民浏览记录
MyTrans <- read_baskets(con = "E://网页浏览数据.txt",sep = ",",info = c("sequenceID","eventID"))
summary(MyTrans)
MyFsets <- cspade(data = MyTrans,parameter = list(support = 0.1))
inspect(MyTrans)
MyRules <- ruleInduction(x = MyFsets,confidence = 0.3) #生成序列推理规则
MyRules.DF <- as(MyRules,"data.frame")  #转成数据框方便后续书写
MyRules.DF[MyRules.DF$lift >= 1,]  #提取提升度大于等于1的序列规则











