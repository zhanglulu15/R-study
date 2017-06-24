library(tm)
library(ggplot2)

#设置全局路径
spam.path <- file.path("E\\ML_for_Hackers-master\\03-Classification\\data","spam")
spam2.path <- file.path("E\\ML_for_Hackers-master\\03-Classification\\data", "spam_2")
easyham.path <- file.path("E\\ML_for_Hackers-master\\03-Classification\\data", "easy_ham")
easyham2.path <- file.path("E\\ML_for_Hackers-master\\03-Classification\\data", "easy_ham_2")
hardham.path <- file.path("E\\ML_for_Hackers-master\\03-Classification\\data", "hard_ham")
hardham2.path <- file.path("E\\ML_for_Hackers-master\\03-Classification\\data", "hard_ham_2")


#将两种类型文件转换成文本语料库
#以“rt"模式打开文件，rt表示以文本形式读取；编码"encoding"指定为latin1，是因为很多
#邮件包含非#readLines函数会把所打开的文件链接中的每一行文本返回为字符串向量的一个元素ASCII码字符，
#指定为为latin1就可以读取这些文件
# The message always begins after the first full line break
#加了一个tryCatch来捕获异常
#用paste函数把这个向量拼接成一个单条文本元素，同时指定参数collapse为“\n",表示由换行符来分隔各元素
#将msg以\n为空格拼接成向量单条文本元素

get.msg <- function(path)
{
  con <- file(path, open = "rt", encoding = "latin1")
  text <- readLines(con)
  msg <- tryCatch(text[seq(which(text == "")[1] + 1, length(text), 1)], error = function(e) e)#从第一个""读数据到最后一行
  close(con)
  return(paste(msg, collapse = "\n"))
}

#要训练分类器，就要从所有垃圾邮件和正常邮件中得到邮件正文，方法一：创建一个向量保存所有邮件正文，
#从而使向量的每个元素就是一封邮件内容，最直接的R实现方式就是结合我们自己的函数get.msg并使用apply
#函数完成
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!="cmds")]
all.spam <- sapply(spam.docs,function(p)get.msg(spam.path,p,seq=""))

#利用邮件向量构建一个文本语料库，这要用到tm程序包中提供的函数，一旦有了语料库形式的文本，
#就可以通过垃圾邮件正文抽取特征词项来构建垃圾邮件分类器的特征集
#量化垃圾邮件特征词项频率的方法之一就是构造一个词项-文本矩阵（TMD)，：是一个N*N矩阵，矩阵的行对应
#在特定语料库的所有文档中抽取的词项，矩阵的列对应语料库中所有的文档，在这个矩阵中，一个[i,j]表示：
#词项i在文档j中出现的次数。和之前一样，定义一个get.dm函数，该函数输入邮件的文本向量，输出TDM

#tm程序包提供了若干种用于构建语料库，在这个案例中因为要用到邮件向量构建语料库，所以用到了
#VectorSource函数来构建source对象，一旦加载了来源文本，会将corpus函数与VectorSource函数配合使用
#从而创建一个Corpus对象（预料对象库），然后要用到control变量，它是一个选项列表，用于设定如何
#提取文本。在这个案例中，用到了四个选项 （1）设定stopword=TRUE，告诉tm在所有文本中移除488个最常见的
#英语停用词，（2）参数removePunctuation（移除标点符号）和removeNumber(移除数字)设为TRUE，主要是为了
#减少与这些 字符有关的噪声，由其因为许多文档都包含HTML标签。（3）设定minDocFreq=2,这确保只有那些
#在文本中出现次数大于1次的字最终才会出现在TDM的行中
get.tdm <- function(doc.vec){
  doc.corpus <- Corpus(VectorSource(doc.vec))
  control <- list(stopwords = TRUE, removePunctuation = TRUE,
                  removeNumbers = TRUE, minDocFreq = 2)
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)  #构建一个词项文本矩阵
  return(doc.dtm)
}
spam.tdm <- get.tdm(all.spam)  #所有的垃圾邮件带入中国函数

#构建分类器，具体来说，可以利用TDM来构建一套垃圾邮件的训练数据，推荐方法：构建一个数据框来保存
#所有特征词项在垃圾邮件中的条件概率
#为了构建这个数据框，（1）首先利用as.matrix命令把TDM对象转换成R的标准数据矩阵。（2）y

spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts), as.numeric(spam.counts)),
                      stringsAsFactors = FALSE)
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)

spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i) {length(which(spam.matrix[i, ] > 0))/ncol(spam.matrix)})
spam.density <- spam.df$frequency/sum(spam.df$frequency)
spam.df <- transform(spam.df, density = spam.density, occurrence = spam.occurrence)

