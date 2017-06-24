library(tm)
library(ggplot2)


#设置路径变量


spam.path <- file.path("E\\03-Classification\\data", "spam")
spam2.path <- file.path("E/03-Classification/data", "spam_2")
easyham.path <- file.path("E/03-Classification/data", "easy_ham")
easyham2.path <- file.path("E/03-Classification/data", "easy_ham_2")
hardham.path <- file.path("E/03-Classification/data", "hard_ham")
hardham2.path <- file.path("E/03-Classification/data", "hard_ham_2")


###########################################
#构建垃圾邮件和正常邮件的特征词项类别知识库
###########################################
#######################
#构建垃圾邮件的特征词项
#######################
#打开每一个文件，找到空行，将空行之后的文本返回为一个字符串向量（只有一个元素）
#file用于打开文件，open设置rt(read as text), 由于邮件中可能包含非ACSⅡ码字符，
#设置encoding = "latin1"
#readLines按行读入文件
#定位到第一个空行“”并抽取后面的所有文本
#有些文件中未包含空行，会抛出错误，因此用tryCatch捕获这些错误并返回NA
#关闭文件，将所有行合并为一行并返回该向量


get.msg <- function(path){
  con <- file(path, open = "rt", encoding = "latin1")
  text <- readLines(con)
  #The message always begins after the first full line break
  #if not have a break, return NA
  msg <- tryCatch(text[seq(which(text == "")[1]+1, length(text), 1)], error = function(e) return(NA))
  close(con)
  return(paste(msg, collapse = "\n"))
}


#创建向量保存所有正文，向量的每个元素就是一封邮件的内容
#dir函数得到路径下文件列表，除掉cmds文件
#应用sapply函数时，先传入一个无名函数，目的是用paste函数把文件名和适当的路径拼接起来


spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
all.spam <- sapply(spam.docs,
                   function(p) get.msg(paste(spam.path, p, sep = "")))


#输入文本向量，输出TDM（Term Document Matrix,词项-文档矩阵）
#矩阵行表示词项，列表示文档，元素[i, j]代表词项i在文档j中出现的次数
#Corpus函数用于构建语料库（corpus对象），VectorSource用向量构建source对象
#source对象是用来创建语料库的数据源对象
#control变量是一个选项列表，用于设定提取文本的清洗规则
#stopwords移除停用词，removePunctuation, removeNumbers分别移除标点和数字
#minDocFreq设定最小两次出现的词才最终出现在TDM中


get.tdm <- function(doc.vec){
  doc.corpus <- Corpus(VectorSource(doc.vec))
  control <- list(stopwords = TRUE, removePunctuation = TRUE,
                  removeNumbers = TRUE, minDocFreq = 2)
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}
spam.tdm <- get.tdm(all.spam)


#用TDM构建垃圾邮件的训练数据：构建数据框保存所有特征词在垃圾邮件中的条件概率
#先将spam.tdm转为标准矩阵，rowSums创建一个包含每个特征在所有文档中总频次的向量
#注意禁止字符自动转为因子
#修改列名，frequency转数字类型


spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts), as.numeric(spam.counts)),
                      stringsAsFactors = FALSE)
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)


#关键训练数据1：计算一个特定特征词项所出现的文档在所有文档中所占比例
#sapply函数将行号传入无名函数，计算该行值为正数的元素个数，再除以文档总数（列数）
#关键训练数据2：统计整个语料库中每个词项的频次（不用于分类，但是可以通过对比频次知道某些词是否影响结果）


spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i) {length(which(spam.matrix[i, ] > 0))/ncol(spam.matrix)})
spam.density <- spam.df$frequency/sum(spam.df$frequency)
spam.df <- transform(spam.df, density = spam.density, occurrence = spam.occurrence)


#按照occurrence列降序排列并显示前6条（与书上的结果不同）





#######################
#构建正常邮件的特征词项
#######################

easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
#注意为了平衡数据，将正常邮件数量限定在500
easyham.docs<-easyham.docs[1:500]
all.easyham <- sapply(easyham.docs,
                      function(p) get.msg(paste(easyham.path, p, sep = "")))
easyham.tdm <- get.tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts), as.numeric(easyham.counts)),
                         stringsAsFactors = FALSE)
names(easyham.df) <- c("term", "frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)

easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                             function(i) {length(which(easyham.matrix[i, ] > 0))/ncol(easyham.matrix)})
easyham.density <- easyham.df$frequency/sum(easyham.df$frequency)
easyham.df <- transform(easyham.df, density = easyham.density, occurrence = easyham.occurrence)


#按照occurrence列降序排列并显示前6条（与书上的结果不同）





#########################################################################
#构造函数classify.email：输入文本返回这封邮件是垃圾邮件的贝叶斯概率估计值
#########################################################################
#抽取正文、转换成TDM、计算特征词项频率
#先验概率默认为50%，未出现词的概率设为0.0001%


classify.email <- function(path, training.df, prior = 0.5, c = 1e-6){
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  #find intersections of words找到邮件中的词项和出现在训练集中的词项的交集
  msg.match <- intersect(names(msg.freq), training.df$term)
  if(length(msg.match) < 1){
    #如果没有任何词出现在垃圾邮件集中
    #length(msg.freq)是词的个数
    #返回的值很小，因为没有训练集中出现过的词，无法判定
    return(prior*c^(length(msg.freq)))
  }else{
    #交集中词的频率存放到match.probs
    #用这些词的特征概率，计算这封邮件是训练集中对应类别的条件概率
    #返回值=是垃圾邮件的先验概率*各重合词在垃圾邮件训练集中的概率积*缺失词项的小概率积
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior*prod(match.probs)*c^(length(msg.freq) - length(msg.match)))
  }
}
