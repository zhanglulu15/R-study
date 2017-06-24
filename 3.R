#简单关联分析应用，商品推荐
data <- read.table(file = "E://电影评分数据.txt",header = TRUE,sep = ",")
data <- as.matrix(data[-1:-7])
MyTrans <- as(data,"transactions")
summary(MyTrans)
myrules <- apriori(data = MyTrans,parameter = list(support = 0.1,confidence = 0.5
                                                   ,target = "rules"))
                                                  
plot(myrules,method = "graph",control = list(arrowSize = 2,main = "连滞销售商品可视化结果"))