library("arules")
library("arulesViz")
myfsets <- eclat(MyTrans,parameter = list(support = 0.5,target =
                                            "maximally frequent itemsets"))#搜索做大频繁k项集
inspect(myfsets)

myfsets <- eclat(data = MyTrans,parameter = list(support = 0.5,
                                                 target = "frequent itemsets"))#搜索频繁项集
plot(myfsets)  #可视化频繁项集

myrules <- ruleInduction(x = myfsets,transactions = MyTrans,confidence = 0.6)
inspect(sort(x = myrules,by = "lift"))
                         

