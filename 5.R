install.packages("neuralnet")
library("neuralnet")
BuyOrNot<-read.table(file="E://消费决策数据.txt",header=TRUE)

##########neurealnet建立神经网络
set.seed(12345)
(BPnet1<-neuralnet(Purchase~Age+Gender+Income,data=BuyOrNot,hidden=2,err.fct="ce",linear.output=FALSE))
BPnet1$result.matrix
BPnet1$weight
BPnet1$startweights 
BPnet1$response  #各观测输出变量的实际值
BPnet1$net.result  #各观测输出变量的预测值

#评价输入变量的重要性
head(BPnet1$generalized.weights[[1]])  #显示前六个默认观测广义权重
par(mfrow = c(2,2))  #分别绘制三幅散点图
gwplot(BPnet1,selected.covariate = "Age")
gwplot(BPnet1,selected.covariate = "Gender")
gwplot(BPnet1,selected.covariate = "Income")

#不同变量组合对输出变量的影响
newdata <- matrix(c(39,1,1,39,1,2,39,1,3,39,2,2,39,2,3),nrow = 6,
                  ncol = 3,byrow = TRUE)
new.output <- compute(BPnet1,covariate = newdata)
new.output$net.result

#利用ROC曲线确定概率分割值
library("ROCR")
detach("package:neuralnet")  #卸载神经网络包
summary(BPnet1$net.result[[1]])  #浏览预测概率值
pred <- prediction(predictions = as.vector(BPnet1$net.result),labels = BPnet1$response)
par(mfrow = c(2,1))
perf <- performance(pred,measure = "tpr",x.measure = "fpr")  #计算标准ROC曲线
plot(perf,colorize = TRUE,print.cutoffs.at = c(0.2,0.45,0.46,0.47))
perf <- performance(pred,measure = "acc") #计算概率分割值变化预测精度的变化
plot(perf)
out <- cbind(BPnet1$response,BPnet1$net.result[[1]]) #合并实际值和概率值
out <- cbind(out,ifelse(out[,2] > 0.468,1,0))  #以0.468为概率分割值进行类别预测
(confm.bp <- table(out[,1],out[,3]))  #计算混淆矩阵
(err.bp <- (sum(confm.bp) - sum(diag(confm.bp)))/sum(confm.bp))  #计算错判率
