#构建决策树
str(iris)  #获得数据结构
set.seed(1234)
ind <- sample(2,nrow(iris),replace = TRUE,prob = c(0.3,0.7))
traindata <- iris[ind == 1,]
testdata <- iris[ind == 2,]

library(party)
myformula   Secpies~Sepal.Length + Sepal.Width + Pe