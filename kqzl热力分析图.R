mytable <- read.csv("E:\\kqzl.csv",header = TRUE)
library("ggplot2")
library("openair")
library(RCurl)

library(XML)

library(dplyr)

library(ggplot2)

library(stringr)

library(rvest)

library(lubridate)

library("DT")

library(openair)
#调整时间变量
mytable$date<-as.Date(mytable$date)
names(mytable)[1]<-"date"

#AQI指数年度分布日力图
calendarPlot(mytable,pollutant="AQI",year=2015)