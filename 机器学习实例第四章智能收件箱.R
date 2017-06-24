#智能收件箱(类似于推荐系统)：按优先级给邮件排序

#邮件的优先级特征：和垃圾邮件分类相比，为谋首先定义一些可能的行为集合（阅读，回复，标记）在定义一个
#时间区间（如：用户看到邮件后1-10秒），为谋需要判定，在定义的时间区间内，用户会执行定义的行为集合
#的概率是多少
 
#对于创建邮件排序系统的特征集有了一个基本的蓝图。首先，对邮件按时间排序，因为在本例中，我们感兴趣的
#大部分预测都包含在时间维度中，第一部分邮件用于训练排序算法。接下来，四种特征将用于训练过程，首先是
#替代社交特征的表征量，即衡量训练数据中来着某个用户的邮件量。接下来，通过发现不同线程来压缩时间测量值
#按照活跃度给线程排序，最后，基于邮件主题和正文中的高频词项增加两个内容特征


#tm包：用于抽取主题和正文共同出席的词项
library(tm)
library(ggplot2)
easyham.path <- read.csv("E:\\ML_for_Hackers-master\\04-Ranking\\data\\final_df.csv")

#用于抽取特征集合的函数，我们要把邮件数据塑造成一个可用的特征集合，从邮件里抽取的特征组成了训练数据
#的方块的每一列，二每一行则是来自每一封邮件的特征唯一值
#函数的作用：paest.email函数，该函数会调用一系列的辅助函数，从每一封邮件中抽取相应的数据，然后依次
#放入一个向量中，命令c(data,from,subj,msg,path)创建的向量构成了数据中的一行，这个数据最终会
#构成训练数据

parse.email <- function(path){
  full.msg <- msg.full(path)
  date <- get.date(full.msg)
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  msg <- get.msg(full.msg)
  return(c(date,from,subj,msg,path))
}

msg.full <- function(path){
  con <- file(path,open = "rt",encoding = "latin1")
  msg <- readline(con)
  close(con)
  return(msg)
}
