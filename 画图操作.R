#使用ggplot里面的qplot作图
library(ggplot2)
qplot(Wind,Temp,data = airquality,color = Month)#每个月分的颜色不一样，对应每个月是一个颜色渐变条
qplot(Wind,Temp,data = airquality,shape = Month) # 把形状赋值给Month，因此每个月点的形状不一样 
qplot(Wind,Temp,data = airquality,size = Month) #把size赋值给month，每个月点的大小不一样

airquality$Month <- factor(airquality$Month)  #把月份转换成因子，每个月对应的就是不同的颜色
qplot(Wind,Temp,data = airquality,color=I("red"))  #只使用一种颜色画图，即图中的所有点都是一种颜色

qplot(Wind,Temp,data = airquality,color = Month,
      geom = c("point","smooth"))  #添加geom.其中point表示点，smooth表示回归线

#数据层（airquality)+美学(aes(Wind横轴,Temp纵轴))层+几何客体层geom_point表示散点
#alpha表示透明度，size表示点的大小
ggplot(airquality,aes(Wind,Temp)) + geom_point(aes(color = factor(Month)),alpha = 0.4,size = 2) 
ggplot(airquality,aes(Wind,Temp)) +geom_point()+geom_smooth()

#method设置为“lm"：表示回归线设置为直线，se设置为FALSE:表示置信区间不在出现
ggplot(airquality,aes(Wind,Temp)) +geom_smooth(method = "lm",se=FALSE)

#aes(col=factor(Month)):表示把每个月按不同的颜色表示出了，Month被强制转换成了类别因子
ggplot(airquality,aes(Wind,Temp)) +geom_smooth(method = "lm",se=FALSE,aes(col = factor(Month)))

