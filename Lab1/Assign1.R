#1 Split data
#install.packages("kknn")
library("kknn")
spambase=read.csv2("spambase.csv")
n=dim(spambase)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=spambase[id,]
test=spambase[-id,]  

#2
fit <- glm(formula=Spam~., data=train, family='binomial') 
results_train = predict(fit, newdata=train,type='response') 
results_test = predict(fit, newdata=test,type='response') 
confusionmatrixTrain <- table(train$Spam, results_train > 0.5) 
confusionmatrixTest <- table(test$Spam, results_test > 0.5)

misscon_rate = function (conf_mat, fit_mat) {
  n=length(fit_mat[,1])
  miss_rate=(1-sum(diag(conf_mat))/n)
  print(miss_rate)
}
misscon_rate(confusionmatrixTrain,test) 

#3
confusionmatrix_2Train <- table(train$Spam, results_train > 0.8) 
confusionmatrix_2Test <- table(test$Spam, results_test > 0.8) 
misscon_rate(confusionmatrix_2Train, test) 

#4
kknn_train_30=kknn(formula=Spam~., train, train, k=30) 
kknn_test_30=kknn(formula=Spam~., train, test, k=30) 

confusionmatrix_kknn_train = table(train$Spam,kknn_train_30[["fitted.values"]] > 0.5) 
confusionmatrix_kknn_test = table(test$Spam,kknn_test_30[["fitted.values"]] > 0.5)

misscon_rate(confusionmatrix_kknn_train,test) #=0.1671533 
misscon_rate(confusionmatrix_kknn_test,test) #=0.3131387

#5
kknn_train_1=kknn(formula=Spam~., train, train, k=1) 
kknn_test_1=kknn(formula=Spam~., train, test, k=1) 

confusionmatrix_kknn_train_1 = table(train$Spam,kknn_train_1[["fitted.values"]] > 0.5) 
confusionmatrix_kknn_test_1 = table(test$Spam,kknn_test_1[["fitted.values"]] > 0.5)

misscon_rate(confusionmatrix_kknn_test_1,test) #=0.3591241
misscon_rate(confusionmatrix_kknn_train_1,test) #=0