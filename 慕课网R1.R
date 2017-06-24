#函数的覆盖
#覆盖c函数
c <- function(x,y) x*y
c(2,3)

rm(c)  #c函数其实是默认的连接函数，要使用连接函数的的功能必须先删除已经设置的函数
c(2,3)

#R中的循环
for(i in 1:3){
  cat(i,"+",i,"=",i + i,"\n")  #cat将所有元素按顺序连接起来了
}

#控制流
for(i in 1:3){
  if (i == 2) cat("this is sevn:","\n")
  cat(1,"\n")
}

for(i in 1:3){
  if(i==2) cat("the index is 2","\n")else
    cat("the index is not 2","\n")
}

#while循环的列子
k <- 0
y <- abs(rnorm(1000))
i <- 0
while(k <3 & i<1000){
  i <- i + 1
  temp <- y[i]
  k <- k+(temp > 2)
}
rm(temp)
i

#repeat循环的列子
eye.colors <- c("brown","blue","green","yellow","grey")
eyecolor <- data.frame(personId = 1:100,color=
                         sample(eye.colors,100,rep = T))
i <- 0
list.of.ids <- numeric(0)
repeat{
  i <- i +1
  if(eyecolor$color[i] == "yellow"|
     eyecolor$color[i]== "blue") next
  list.of.ids < c(list.of.ids,eyecolor$personId[i])
  if(i == 100|length(list.of.ids) == 20)break
}
list.of.ids


#lapply()和sapply()的列子
my.data <- data.frame(data1 = rnorm(10),data2 = rnorm(10),data3 = rnorm(10))
lapply(my.data,sum)  #前面是数据后面是作用在数据上的函数
sapply(my.data,sum)

#构造向量
y <- seq(0,1,length = 11) #从0带1由11个数构成的等差序列
z <- rep(1:2,5)    #由相同序列重复构成的向量
xz10 <- c(x,z,10)  #把数值和向量组合成新的向量


library("ggplot2")
ggplot(data=mpg,mapping = aes(x=cty,y=hwy))+geom_point()+
        aes(colour=factor(mpg$year))  


ggplot(data=mpg,mapping = aes(x=cty,y=hwy))  

ggplot(data=mpg,mapping = aes(x=cty,y=hwy))+
  geom_point(size=I(2))+aes(colour=factor(mpg$year))

ggplot(data=mpg,mapping = aes(x=cty,y=hwy))+geom_point()
       +aes(colour=factor(mpg$year))+stat_smooth()  

ggplot(data=mpg,mapping = aes(x=cty,y=hwy))+
  geom_point(aes(colour=factor(mpg$year)))+stat_smooth()  

ggplot(data=mpg,mapping =aes(x=cty,y=hwy))+  
  geom_point(aes(colour=factor(year),size=displ))+  
  stat_smooth()

ggplot(data=mpg,mapping = aes(x=cty,y=hwy))+  
  geom_point(aes(colour=factor(mpg$year),size=displ),alpha=0.25)+  
  stat_smooth()  

ggplot(mpg, aes(x=cty, y=hwy))+   
  geom_point(aes(colour=class,size=displ),alpha=0.6,position = "jitter")+  
  stat_smooth()+  
  scale_size_continuous(range = c(4, 10))+  
  facet_wrap(~ year,ncol=1)+  
  ggtitle("汽车油耗与型号")+  
  labs(y='每加仑高速公路行驶距离',  
       x='每加仑城市公路行驶距离')+  
  guides(size=guide_legend(title='排量'),  
         colour = guide_legend(title='车型',  
                               override.aes=list(size=5)))  

ggplot()+  
  geom_point(aes(x=mpg$cty,y=mpg$hwy),color="red")+  
  geom_point(aes(x=mpg$cty,y=mpg$displ),color="green")  

ggplot(mpg)+geom_bar(width=1, aes(x=factor(1),fill=mpg$class))+  
  coord_polar(theta="y") 