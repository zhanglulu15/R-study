#�򵥹�������Ӧ�ã���Ʒ�Ƽ�
data <- read.table(file = "E://��Ӱ��������.txt",header = TRUE,sep = ",")
data <- as.matrix(data[-1:-7])
MyTrans <- as(data,"transactions")
summary(MyTrans)
myrules <- apriori(data = MyTrans,parameter = list(support = 0.1,confidence = 0.5
                                                   ,target = "rules"))
                                                  
plot(myrules,method = "graph",control = list(arrowSize = 2,main = "����������Ʒ���ӻ����"))