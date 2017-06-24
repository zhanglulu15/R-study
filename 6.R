set.seed(12345)
y <- c(rep(-1,20),rep(1,20))
x <- matrix(rnorm(n = 40*2,mean = 0,sd = 1),ncol = 2,byrow = TRUE)
x[y==1] <- x[y==1,] + 1.5
data_train <- data.frame(fx1 = x[,1],fx2 = x[,2],fy = as.factor(y))  #生成训练样本集
x <- matrix(rnorm(n = 20,mean = 0,sd = 1),ncol = 2,byrow = TRUE)
y <- sample(x = c(-1,1),size = 10,replace = TRUE)
x[y==1,] <- x[y==1,] + 1.5
data_test <- data.frame(fx1 = x[,1],fx2 = x[,2],fy = as.factor(y))  #测试训练样本集
plot(data_train[,2:1],col = as.integer(as.vector(data_train[,3])) + 2,pch = 8,
     cex = 0.7,main = "训练样本集-1和+1类散点图")
library("e1071")
svmfit <- svm(fy~.,data = data_train,type = "C-classification",kernel = "linear",
              cost = 10,scale = FALSE)
summary(svmfit)
svmfit$index  #给出各支持向量的观测编号
plot(x = svmfit,data = data_train,formula = fx1~fx2,svSymbol = "#",dataSymbol = "*",
     grid = 100)
svmfit <- svm(fy~.,data = data_train,type = "C-classifition",kernel= "linear",cost = 0.1.scale = FALSE)
summary(svmfit )


#10折交叉验证选取损失惩罚参数c
set.seed(12345)
tobj <- tune.svm(fy~.,data = data_train,type = "C-classification",kernel = "linear",cost = c(0.001,0.01,0.1,1,
                                                                                             5,10,100,1000),scale = FALSE)
summary(tobj)
bestsvm <- tobj$best.model
summary(bestsvm)
ypred <- predict(bestsvm,data_test)
(confm <- table(ypred,data_test$fy))
err <- (sum(confm) - sum(diag(confm)))/sum(confm)

#利用R模拟线性不可分下的支持向量
set.seed(12345)
x <- matrix(rnorm(n = 400,mean = 0,sd = 1),ncol = 2,byrow = TRUE)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1,150),rep(2,50))
data <- data.frame(fx1 = x[,1],fx2 = x[,2],fy = as.factor(y))
flag <- sample(1:200,size = 100)
data_train <- data[flag,]
data_test <- data[-flag,]
plot(data_train[,2:1],col = as.integer(as.vector(data_train[,3])),pch = 8,
     cex = 0.7,main = "训练样本集散点图")  #是一个线性不可分的的问题，要采用径向核函数
library("e1071")
set.seed(12345)
tobj <- tune.svm(fy~.,data = data_train,type = "C-classification",kernel = "radial",
                 cost = c(0.001,0.01,0.1,1,5,10,100,1000),gamma = c(0.5,1,2,3,4),scale = FALSE)
plot(tobj,xlab = expression(gamma),ylab = "损失惩罚函数C",main = "不同参数组合下错误率",nlevels = 10,
     color.palette = terrain.colors)  #颜色越深表示误差越小
betsvm <- tobj$best.model
summary(betsvm)

#利用R模拟多分类的支持向量分类
set.seed(12345)
x <- matrix(rnorm(n = 400,mean = 0,sd = 1),ncol = 2,byrow = TRUE)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
x <- rbind(x,matrix(rnorm(n = 100,mean = 0,sd = 1),ncol = 2,byrow = TRUE))
y <- c(rep(1,150),rep(2,50))
y <- c(y,rep(0,50))
x[y==0,2] <- x[y==0,2] + 3
data <- data.frame(fx1=x[,1],fx2=[,2],fy=as.factor(y))
plot(data[,2:1],col = as.integer(as.vector(data[,3])) + 1,pch = 8,cex = 0.7,main = "训练样本散点图")
library("e1071")
set.seed(12345)
tobj <- tune.svm(fy~.,data = data,type = "C-classifition",kernel = "radial",cost = c(0.001,0.01,
                                0.1,1,5,10,100,1000),gamma = c(0.5,1,2,3,4),scale=FALSE)
bestsvm <- tobj$best.model
summary(bestsvm)
plot(x = bestsvm,data = data,formula = fx1~fx2,svSymbol = "#",dataSymbol = "*",grid = 100)
svmfit <- svm(fy~.,data = data,type = "C-classification",kernel = "radial",
              cost = 5,gamma = 1,scale = FALSE)
head(svmfit$decision.values)
ypred <- predict(svmfit,data)
(confm <- table(ypred,data$fy))
err <- (sum(confm) - sum(diag(confm)))/sum(confm)

#支持向量机分类应用：天猫成交顾客预测
tmall_train <- read.table(file = "E://天猫_Train_1.txt",header = TRUE,sep = ",")
tmall_train$BuyOrNot <- as.factor(tmall_train$BuyOrNot)
library("e1071")
set.seed(12345)
tobj <- tune.svm(BuyOrNot~.,data = tmall_train,type="C-classification",kernel="radial",gamma=10^(-6:-3),cost=10^(-3:2))
plot(tobj,xlab = expression(gamma),ylab = "损失惩罚函数C",main = "不同参数组合下的预测错误率",nlevels = 
       10,color.palette = terrain.colors)
bestsvm <- tobj$best.model
summary(bestsvm)
tmall_test <- read.table(file = "E://天猫_Test_1.txt",header = TRUE,sep = ",")
tmall_test$BuyOrNot <- as.factor(tmall_test$BuyOrNot)
ypred <- predict(bestsvm,tmall_test)
(confm <- table(ypred,tmall_test$BuyOrNot))
(err <- (sum(confm) - sum(diag(confm)))/sum(confm))