library("arules")
library("arulesViz")
data <- read.table(file = "E://����������.txt",header = TRUE,sep = ",")
data <- as.matrix(data[,-1:-7])
mytrans <- as(data,"transactions")
summary(mytrans)

myrules <- apriori(data = mytrans,parameter = list(support = 0.1,
                                                   confidence = 0.5,target = "rules"))
plot(myrules,method = "graph",control = list(arrowSize = 2,main = "������Ʒ���ӻ����"))

#�˿�ѡ������Ա�
Data<-read.table(file="E://����������.txt",header=TRUE,sep=",")
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
supersetf <- is.subset(MyRules,MyRules)   #�ж��Ƿ�����������
inspect(MyRules[-which(colSums(supersetf) > 1)])  #����������
MyRules <- subset(x = MyRules,subset = quality(MyRules)$lift > 1)
plot(MyRules,method = "graph",control = list(arrowSize = 2,main = "�Ա��������ơ��ѡ������Ա�"))

#�������ݵ�ʱ�������
library("arulesSequences")
mytrans<-read_baskets(con="E://��������ԭʼ����.txt",sep=",",info=c("sequenceID","eventID"))
MyFsets <- cspade(data = mytrans,parameter = list(support = 0.5))  #SPADE�㷨
inspect(MyFsets)
MyRules<-ruleInduction(x=MyFsets,confidence=0.3)  
MyRules.DF<-as(MyRules,"data.frame")  
MyRules.DF[MyRules.DF$lift>=1,]

#���й���������Ӧ�ã��������������¼
MyTrans <- read_baskets(con = "E://��ҳ�������.txt",sep = ",",info = c("sequenceID","eventID"))
summary(MyTrans)
MyFsets <- cspade(data = MyTrans,parameter = list(support = 0.1))
inspect(MyTrans)
MyRules <- ruleInduction(x = MyFsets,confidence = 0.3) #����������������
MyRules.DF <- as(MyRules,"data.frame")  #ת�����ݿ򷽱������д
MyRules.DF[MyRules.DF$lift >= 1,]  #��ȡ�����ȴ��ڵ���1�����й���










