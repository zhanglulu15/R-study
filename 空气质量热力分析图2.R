mytable <- read.csv("E:\\kqzl.csv",header = TRUE)

Year<-2014:2016

month<-sprintf("d",1:12)

#预览数据
dim(mytable)

#查看名称
attributes(mytable)$names

#使用DT表格预览数据集
mytable<-read.csv("E:\\kqzl.csv",stringsAsFactors=FALSE,check.names=FALSE)

#查看数据结构和变量属性是否符合分析需要
str(mytable)

#定义日期变量格式
mytable$日期<-as.Date(mytable$日期)
names(mytable)[c(1,2,4)]<-c("date","Level","Order")


mytable$year <-year(mytable$date)

breaks<-c(0,50,100,150,200,300,500)

label<-c("excellent","good","Mild pollution","moderate pollution","heavy pollution ","serious pollution")

filter(mytable,Year==2014)%>%calendarPlot(.,pollutant="AQI",
                                          breaks=breaks,labels=label,year=2014)

filter(mytable,Year==2015)%>%calendarPlot(.,pollutant="AQI",
                                          breaks=breaks,labels=label,year=2015)

filter(mytable,Year==2016)%>%calendarPlot(.,pollutant="AQI",
                                          breaks=breaks,labels=label,year=2016)