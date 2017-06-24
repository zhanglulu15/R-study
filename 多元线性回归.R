#练习3-多元线性回归
x <- read.table("E:\\ex3x.dat",header = F)
y <- read.table("E:\\ex3y.dat",header = F)
colnames(x) <- c("area","room_num")
colnames(y) <- c("price")

#散点图
input <- data.frame(area = x$area,room_num=x$room_num,price = y$price)
plot(input)
#房价和所在地区的散点图
library(ggplot2)
p <- ggplot(aes(x = area,y = price),data = input)
p + geom_point(size = 3,shape = 3)+theme_bw()+xlab("area")+ylab("price")

#归一化
x <- cbind(rep(1,nrow(x)),x$area,x$room_num)
x[,2:3] <- scale(x[,2:3])  #对数据进行标准化
y <- as.matrix(y)  

#学习率的实验
library(ggplot2)
iter_num <- 50
alpha <- c(0.01,0.03,0.1,0.3,1,1.3)
plot_data <- NULL
plot_color <- c("back","green","blue","orange","purple")
for(j in 1:length(alpha){
  theta <- c(rep(0,ncol(x)))
  jtheta <- c(rep(0,iter_num))
  for(i in 1:iter_num){
    jtheta[i] <- (1/(2*nrow(x)))*t(x%*%theta - y)%*%(x%*%theta - y)
    grad <- 1/nrow(x)*t(x)%*%(x%*%theta - y)
    theta <- theta - alpha[j]*grad
  }
  if(is.null(plot_data)){
    plot_data <- data.frame(iter = c(1:iter_num),cost = jtheta,rate = alpha[j])
  }
  else{
    plot_data <- rbind(plot_data,data.frame(iter = c(1:iter_num),cost = jtheta,rate = alpha[j]))
  }
}
