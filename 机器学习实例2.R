#机器学习实例第二章：数据分析
height.weight <-  read.delim("E:\\ML_for_Hackers-master\\02-Exploration\\data\\01_heights_weights_genders.csv",
                             sep = ",",header = TRUE )
summary(height.weight)

#仅查看heights.weights中的height的各个值

heights <- with(height.weight, Height)
summary(heights)

#计算均值的函数：把向量中的值求和，在除以长度

my.mean <- function(){
  return(sum(x)/length(x))
}

 #计算中位数:首先，对向量排序，因为中位数本质上是有序向量中间那个数。如果列表长度是偶数，
#则在数据集中间有两个数，这两个数所在位置相当于列表长度为奇数的情况下中的中位数，对这两个数求均值即可

my.median <- function(x){
  sorted.x <- sort(x)
  if(length(x) %% 2 == 0){
    indices <- c(length(x)/2,length(x)/2 + 1)
    return(mean(sorted.x[indices]))
  }
  else{
    index <- ceiling(length(x)/2)
    return(sorted.x[index])
  }
}

my.vector <- c(0,0,100)
my.vector
mean(my.vector)  #应用上面的函数求均值
my.median(my.vector)  #应用上面的函数求中位数

#回到本列中
mean(heights)
my.median(heights)

#分位数
min(heights)
max(heights)
c(min(heights),max(heights))   #将身高的最小值和最大值结合起来
range(heights)  #求出身高的范围
quantile(heights)  
quantile(heights,probs = seq(0,1,by = 0.20))  #得到不同位置的分位数，需要用probs传入截取位置
seq(0,1,by = 0.20)  #seq函数在0-1之间尝试一个步长为0.2的序列

#计算包含50%数据的范围
c(quantile(heights,probs = 0.25),quantile(heights,probs = 0.75))
#计算包含95%的数据范围
c(quantile(heights,probs = 0.025),quantile(heights,probs = 0.095))

#方差：为了衡量数据集里面任意数值与均值的平均偏离程度
my.var <- function(x)
{
  m <- mean(x)
  return(sum((x - m) ^ 2) / (length(x)-1))
}
my.var(heights) - var(heights)

#与均值相差正负单位方差之间的数值
c(mean(heights) - var(heights),mean(heights) + var(heights))


#构造正态分布函数和柯西函数
set.seed(1)
normal.values <- rnorm(250,0,1)  #正态分布
cauchy.values <- rcauchy(250,0,1)  #柯西函数
range(normal.values)
range(cauchy.values)

#把结果画出来
ggplot(data.frame(X = normal.values),aes(x = X)) + geom_density()  #正态分布
ggplot(data.frame(X = cauchy.values),aes(x = X)) + geom_density() #柯西分布

#伽玛分布:伽玛分布是向右倾斜的，意味着中位数和均值有时差距很大
gamma.values <- rgamma(100000,1,0.001)
ggplot(data.frame(X = gamma.values),aes(x = X)) + geom_density()

#体重和身高数据可视化
ggplot(height.weight,aes(x = Height,y = Weight)) + geom_point()

#用平滑的模式画出来,在本列中，预测的趋势是一条简单的曲线，曲线的阴影区是体重预测值的范围
ggplot(height.weight,aes(x = Height,y =Weight)) + geom_point() + geom_smooth()

#选择不同数据进行拟合
ggplot(height.weight[1:20,],aes(x = Height,y = Weight)) + geom_point() + geom_smooth()
ggplot(height.weight[1:200,],aes(x = Height,y = Weight)) + geom_point() + geom_smooth()
ggplot(height.weight[1:2000,],aes(x = Height,y = Weight)) + geom_point() + geom_smooth()

#加入性别,不同颜色表示不同性别
ggplot(height.weight,aes(x = Height,y = Weight,color = Gender)) + geom_point()