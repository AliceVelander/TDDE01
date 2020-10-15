# Assignment 2 
setwd("~/Studier+Studentliv/HT19/TDDE01/Labb2")
credit=read.csv2("creditscoring.csv")
RNGversion('3.5.1')

misscon_rate = function (conf_mat, fit_mat) {
  n=length(fit_mat[,1])
  miss_rate=(1-sum(diag(conf_mat))/n)
  print(miss_rate)
}

#1 Split data
n=dim(credit)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=credit[id,]

id1=setdiff(1:n, id)
set.seed(12345)

id2=sample(id1, floor(n*0.25))
valid=credit[id2,]

id3=setdiff(id1,id2)
test=credit[id3,]

#install.packages("tree")
library("tree")

#2 
par(mfrow = c(1,1))
#DEVIANCE 
fit_tree_dev = tree(good_bad~., data=train, split="deviance")

predict_train_dev = predict(fit_tree_dev, newdata=train, type="class")
predict_test_dev = predict(fit_tree_dev, newdata=test, type="class")

#misclassification rates with deviance
CM_dev_train<- table(train$good_bad, predict_train_dev)
misscon_rate(CM_dev_train, train) #misclassification rate = 0.212

CM_dev_test<- table(test$good_bad, predict_test_dev)
misscon_rate(CM_dev_test, test) #misclassification rate = 0.268 

#GINI
fit_tree_gini = tree(good_bad~., data=train, split="gini")

predict_train_gini = predict(fit_tree_gini, newdata=train, type="class")
predict_test_gini = predict(fit_tree_gini, newdata=test, type="class")

#misclassification rates with deviance
CM_gini_train<- table(train$good_bad, predict_train_gini)
misscon_rate(CM_gini_train, train) #misclassification rate = 0.242

CM_gini_test<- table(test$good_bad, predict_test_gini)
misscon_rate(CM_gini_test, test) #misclassification rate = 0.372

#3 -Use training av validation sets in order to choose the optimal tree depth.

#We take the fitted tre with deviance from #2 ! 

#create vectors for differents depths to try (?)
trainScore=rep(0,12)
testScore=rep(0,12)

for (i in 2:12) {
  prunedTree=prune.tree(fit_tree_dev, best=i)
  predTree = predict(prunedTree, newdata=valid, type="tree") #We don't need to do this for the trained data
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(predTree)
}

plot(2:12, trainScore[2:12], type = "b", col="red", ylim = c(200,700), ylab="deviance", xlab="amount of leaf nodes")  
points(2:12, testScore[2:12], type = "b", col="blue")

prunedTree_4=prune.tree(fit_tree_dev, best=4)
predTree_4 = predict(prunedTree_4, newdata=valid, type="class")

CM_tree_dev <- table(valid$good_bad, predTree_4)
misscon_rate(CM_tree_dev, valid) #misclassification rate=0.264


plot(prunedTree_4)
text(prunedTree_4, pretty=0)

CM_tree_dev

#4 - -NAIVES-

#install.packages("MASS")
#install.packages("e1071")
library("MASS")
library("e1071")

#Predict Values for Naives 
fit_NB = naiveBayes(good_bad~., data=train)

pred_NB_train = predict(fit_NB, newdata=train)
pred_NB_test = predict(fit_NB, newdata=test)

#Misclassification rates 
CM_NB_train<- table(train$good_bad, pred_NB_train)
misscon_rate(CM_NB_train, train) #misclassification rate = 0.3

CM_NB_test<- table(test$good_bad, pred_NB_test)
misscon_rate(CM_NB_test, test) #misclassification rate = 0.316 #much better - why a better model?? 

#5
#Create pi vector

get_FPR_TPR = function (pred_fit){
  
  pi=seq(0.05, 0.95, by=0.05)
  TPR = c()
  FPR = c()
 
  #pred_opti_tree = predict(prunedTree_4, newdata=test, type="vector")

  for (pi_value in pi) {
    #temporär classify vektor - ny för varje pi!
    own_classify=c()
    for (value in pred_fit[,2]){  #Only take the good values!
      if (pi_value > value){
        own_classify=c(own_classify, "bad") 
      } else {
        own_classify=c(own_classify, "good")    
      }
    }
    
    #for each pi - get the confusion matrix! 
    CM = table(valid$good_bad, own_classify)
    
    #If only a vector - fill the other column. Get the right format
    if (ncol(CM) == 1) {
      if(is.element("bad", colnames(CM))) {
        CM = cbind("good" = c(0,0), CM)
      } else {
        CM = cbind(CM, "bad" = c(0,0))
      } }
    
    # Computing TPR and FPR from the classification matrix
    TPR_value = CM[2,2]/sum(CM[2,])
    FPR_value = CM[1,2]/sum(CM[1,])
    # Adding the value to the TPR and FPR vectors
    TPR = c(TPR, TPR_value)
    FPR = c(FPR, FPR_value)
  }
  return (cbind(TPR, FPR))
}

#tree
pred_opti_tree = predict(prunedTree_4, newdata=valid, type="vector")
ROC_tree = get_FPR_TPR(pred_opti_tree)

#NB
pred_opti_NB = predict(fit_tree_dev, newdata=valid, type="vector")
ROC_NB = get_FPR_TPR(pred_opti_NB)

#sort the tree ROC values for correct plotting....
ROC_TPR_tree=sort(ROC_tree[,1])
ROC_FPR_tree=sort(ROC_tree[,2])
ROC_tree_sorted = cbind(ROC_TPR_tree, ROC_FPR_tree)

#Plot for Tree ROC and NB ROC 
plot(ROC_tree_sorted[,2], ROC_tree_sorted[,1], col="green",type="l", xlab="FPR", ylab="TPR", ylim=(0:1), xlim=(0:1))
par(new=TRUE)
plot(ROC_NB[,2], ROC_NB[,1], col="blue",type="l", xlab="FPR", ylab="TPR", ylim=(0:1), xlim=(0:1))

#6 - instead of using pi, we use 10/11 as threshold -- NAIVE BAYES----
fit_NB = naiveBayes(good_bad~., data=train)

pred_NB_test = predict(fit_NB, newdata=test, type="raw")
pred_NB_train = predict(fit_NB, newdata=train, type="raw")

#Se föreläsningsanteckningar 
CM_test = table(test$good_bad, pred_NB_test[,2]/pred_NB_test[,1] > 10/1) 
misscon_rate(CM_test, test)

CM_train = table(train$good_bad, pred_NB_train[,2]/pred_NB_train[,1] > 10/1)
misscon_rate(CM_train, train) 