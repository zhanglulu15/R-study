#机器学习-练习2线性回归
x <- read.csv("E:\\ex2x.dat",header = F)
y <- read.csv("E:\\ex2y.dat",header = F)
colnames(x) <- c("age")
colnames(y) <- c("height")

#scatter plot
input <- data.frame(age =x$age,height = y$height )
library(ggplot2)
p <- ggplot(aes(x = age,y = height),data = input)
p + geom_point(size = 3,shape = 3) + theme_bw()  + xlab("Age in years") + ylab("Height in meters")

#梯度下降法实现
x <- cbind(c(rep(1,nrow(x))),x$age)
y <- as.matrix(y)
theta <- c(rep(0,ncol(x)))
alpha <- 0.07
max_ita <- 300
for(i in 1:max_ita){
  grad <- 1/nrow(x)*t(x)%*%(x%*%theta - y)
  theta <- theta - alpha*grad
}
print(theta)

#添加拟合线
p + geom_point(size = 3,shape = 4) + geom_abline(intercept = theta[1],slope = theta[2],size = 1)+
  geom_smooth(method = "lm",size = 1) + theme_bw() + xlab("Age in years") + ylab("Height in meters")

