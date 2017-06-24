library("arules")
library("arulesViz")
myfsets <- eclat(MyTrans,parameter = list(support = 0.5,target =
                                            "maximally frequent itemsets"))#��������Ƶ��k�
inspect(myfsets)

myfsets <- eclat(data = MyTrans,parameter = list(support = 0.5,
                                                 target = "frequent itemsets"))#����Ƶ���
plot(myfsets)  #���ӻ�Ƶ���

myrules <- ruleInduction(x = myfsets,transactions = MyTrans,confidence = 0.6)
inspect(sort(x = myrules,by = "lift"))
                         
