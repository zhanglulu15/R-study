install.packages("neuralnet")
library("neuralnet")
BuyOrNot<-read.table(file="E://���Ѿ�������.txt",header=TRUE)

##########neurealnet����������
set.seed(12345)
(BPnet1<-neuralnet(Purchase~Age+Gender+Income,data=BuyOrNot,hidden=2,err.fct="ce",linear.output=FALSE))
BPnet1$result.matrix
BPnet1$weight
BPnet1$startweights 
BPnet1$response  #���۲����������ʵ��ֵ
BPnet1$net.result  #���۲����������Ԥ��ֵ

#���������������Ҫ��
head(BPnet1$generalized.weights[[1]])  #��ʾǰ����Ĭ�Ϲ۲����Ȩ��
par(mfrow = c(2,2))  #�ֱ��������ɢ��ͼ
gwplot(BPnet1,selected.covariate = "Age")
gwplot(BPnet1,selected.covariate = "Gender")
gwplot(BPnet1,selected.covariate = "Income")

#��ͬ������϶����������Ӱ��
newdata <- matrix(c(39,1,1,39,1,2,39,1,3,39,2,2,39,2,3),nrow = 6,
                  ncol = 3,byrow = TRUE)
new.output <- compute(BPnet1,covariate = newdata)
new.output$net.result

#����ROC����ȷ�����ʷָ�ֵ
library("ROCR")
detach("package:neuralnet")  #ж���������
summary(BPnet1$net.result[[1]])  #���Ԥ�����ֵ
pred <- prediction(predictions = as.vector(BPnet1$net.result),labels = BPnet1$response)
par(mfrow = c(2,1))
perf <- performance(pred,measure = "tpr",x.measure = "fpr")  #�����׼ROC����
plot(perf,colorize = TRUE,print.cutoffs.at = c(0.2,0.45,0.46,0.47))
perf <- performance(pred,measure = "acc") #������ʷָ�ֵ�仯Ԥ�⾫�ȵı仯
plot(perf)
out <- cbind(BPnet1$response,BPnet1$net.result[[1]]) #�ϲ�ʵ��ֵ�͸���ֵ
out <- cbind(out,ifelse(out[,2] > 0.468,1,0))  #��0.468Ϊ���ʷָ�ֵ�������Ԥ��
(confm.bp <- table(out[,1],out[,3]))  #�����������
(err.bp <- (sum(confm.bp) - sum(diag(confm.bp)))/sum(confm.bp))  #���������