#机器学习实例1

#read.delim在读函数时，会默认把字符串装换为factor的类型，这个类型是用来表示分类变量的，因此需要设置
#stringAsFactor=FALSE来防止其转换，此外，这份数据第一行并没有表头，因此还需要把表头的参数设置为false
#以防止R把第一行当成表头，最后，数据中有许多空元素，要把这些空元素设置为R中的=特殊值NA，定义显式的空字符串为
#na.string
library(ggplot2)    # We'll use ggplot2 for all of our visualizations
library(plyr)       # For data manipulation
library(scales)     # We'll need to fix date formats in plots

ufo <- read.delim("E:\\ML_for_Hackers-master\\01-Introduction\\data\\ufo\\ufo_awesome.tsv",sep = "\t",
    stringsAsFactors = FALSE,header = FALSE,na.strings = "" )

#给数据的 每一列命名
names(ufo) <- c("DateOccurred", "DateReported",
                "Location", "ShortDescription",
                "Duration", "LongDescription")

ufo$Dete0ccurred <- as.Date(ufo$Dete0ccurred,format = "%y%m%d")  #出错，原因是数据存在畸形

#处理数据畸形
head(ufo[which(nchar(ufo$DateOccurred)!=8|
                 nchar(ufo$DateReported)!=8),1])  #查看前两列的异常数据
good.rows <- ifelse(nchar(ufo$DateOccurred) != 8 |
                      nchar(ufo$DateReported) != 8,
                    FALSE,
                    TRUE)
length(which(!good.rows))

ufo <- ufo[good.rows,]

#移除或者忽略不规范数据
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format = "%Y%m%d")
#组织目击地点数据
#首先，strsplit函数被R的异常处理函数tryCatch所包围，其次，并非所有地点都是“city,state"的形式，
#甚至有些连逗号都没有，strsplit函数在遇到不符合格式的数据时会抛出一个异常，因此需要捕获（catch)
#这个异常，在本例中，对于不包含逗号的数据，我们会返回一个NA向量来表明这条数据无效
#用gsub移除每个字符串开头的空格
#最后增加一步检查，确保每个返回向量的长度是2，如果返回向量长度大于2，则返回NA向量
get.location <- function(l)
{
  split.location <- tryCatch(strsplit(l, ",")[[1]],
                             error = function(e) return(c(NA, NA)))
  clean.location <- gsub("^ ","",split.location)
  if (length(clean.location) > 2)
  {
    return(c(NA,NA))
  }
  else
  {
    return(clean.location)
  }
}

# We use 'lapply' to return a list with [City, State] vector as each element
city.state <- lapply(ufo$Location, get.location)

head(city.state)

#将List转换成一个两列的矩阵，其中城市数据作为其首列
#为从list构造一个矩阵，用do.call在list上执行一个函数调用，传入函数是rbind，这个函数把city.state链表中的
#所有向量一行一行的合并起来，从而创建一个矩阵，要把这个矩阵并入数据框中，还要用transform函数
#分别用location.matrix的第一列和第二列创建两个新列：USCcity和USState
#最后由于州的名字缩写形式不一样，有的采用大写，有的采用小写，因此用tolower函数把所有的州的名字的缩写都变成小写

location.matrix <- do.call(rbind,city.state)
ufo <- transform(ufo,
                 USCity = location.matrix[, 1],
                 USState = location.matrix[, 2],
                 stringsAsFactors = FALSE)

#state.abb表示美国50个州的双字母缩写，match函数有两个参数：第一个参数时待匹配的值，第二个参数是
#用于匹配的数据，函数返回值是一个长度与第一个参数相同的向量，向量中的值是其在第二个参数中所匹配的
#索引值，如果没有在第二个参数第二个参数中找到匹配的值，函数返回值为NA，在本列中，我们只关心数据返回值为NA
#因为这表明它们没有匹配上美国的洲名，然后利用is.na函数找出这些不是美国洲名的数据，并且将其在USState列中
#的值重置为NA，最后还要把USCity列中对应位置也设为NA

ufo$USState <- state.abb[match(ufo$USState, state.abb)]  
ufo$USState[is.na(ufo$USState)] <- NA
ufo.us <- subset(ufo, !is.na(USState))
summary(ufo.us)
head(ufo.us)

#分析时间维度
summary(ufo.us$DateOccurred)
ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram(aes(fill='white', color='red')) +
  scale_fill_manual(values=c('white'='white'), guide="none") +
  scale_color_manual(values=c('red'='red'), guide="none") +
  scale_x_date(breaks = "50 years")


#由直方图可知，绝大部分的事件是发生在1960-2010年，而其中最主要的又发生在过去的20年里，因此我们只需要关注1990-2010
#的数据即可，用subset函数把符合条件的数据挑出来构建一个新的数据框,挑出事件发生时间大于1990-01-01
#的事件
ufo.us <- subset(ufo.us,DateOccurred >= as.Date("1990-01-01"))
nrow(ufo.us)

#我们要统计1990-2010年每个洲每年-月的UFO目击次数，首先，要在数据中建一个列，这个列用来保存和
#当前数据中一样的年和月，用strftime函数把日期对象转换成一个“YY-MM"的格式的字符串
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred,format = "%y-%m")

#统计每个洲在每个年-月期间目击UFO的次数,在本例中我们要做的是用“洲名缩写”和“年-月”这一新增加的列来给数据分组，数据
#按照这种方式分好组之后，可以对每个组的数据进行统计并把统计结果以一个新列的形式返回，在这里
#只是简单地用nrow函数按照每行来化简每组数据
sighting.counts <- ddply(ufo.us,.(USState,YearMonth),nrow)
head(sighting.counts)

#我们需要一个覆盖整个数据集的“年-月”向量，用这个向量可以检查出哪些“年-月”已经存在于数据集中
#如果不存在就补上，并把次数设为0，因此要seq.Date函数创建一个日期序列，然后把格式设定为数据框中的日期格式
date.range <- seq.Date(from = as.Date(min(ufo.us$DateOccurred)),
                       to=as.Date(max(ufo.us$DateOccurred)),by="month")
date.strings <- strftime(date.range,"%y-%m")

#有了date.string这个新的向量，我们还需要新建一个包含所有年-月和洲的数据框，
#然后用这个数据框去匹配UFO，依旧先用lapply函数创建列，在用dou.call函数将其转换成矩阵并最终转换成数据框
states.dates <- lapply(state.abb,function(s) cbind(s,date.strings))
states.dates <- data.frame(do.call(rbind,states.dates),stringsAsFactors = FALSE)
head(states.dates)

#states,dates包含了每一年，每个月，每个洲所有组合对应的所有目击记录，现在已经有了阿拉斯加州1900年
#2月和3月的记录了，要给UFO目击记录数据中的缺失值补上0，还需要把这个新数据和原来的数据框合并
#为此，我们用merge函数给这个函数输入两个有序数据框，然后将数据框中相同的列合并，在本列中，两个数据
#框是按洲名的字母顺序以及年-月的时间顺序合并的，还要告诉函数将数据框框中的哪些列进行合并，这就给参数
#by.x和参数by.y指定每个数据框中对应的列名。最后，把参数all设置为TRUE，以告诉函数要把没匹配上的数据包含进来并填充为NA

all.sightings <- merge(states.dates,
                       sightings.counts,
                       by.x = c("s", "date.strings"),
                       by.y = c("USState", "YearMonth"),
                       all = TRUE)
head(all.sightings)

names(all.sightings) <- c("State","YearMonth","Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0

all.sightings$State <- as.factor(toupper(all.sightings$State))

