setwd("~/Studier+Studentliv/HT19/TDDE01/Labb1")
tecator=read.csv2("tecator.csv")

#1
plot(x=tecator$Protein, y = tecator$Moisture, xlab = "Protein", ylab = "Moisture", main="Protein VS Moisture")

#2
#MSE

#3
set.seed(12345)
sample <- sample.int(n = nrow(tecator), size = floor(.5*nrow(tecator)), replace = F)
train = tecator[sample, ]
validation = tecator[-sample, ]
validation=validation[c(1:107),]

M1 = lm(Moisture~poly(Protein, degree=1), data=train)
M2 = lm(Moisture~poly(Protein, degree=2), data=train)
M3 = lm(Moisture~poly(Protein, degree=3), data=train)
M4 = lm(Moisture~poly(Protein, degree=4), data=train)
M5 = lm(Moisture~poly(Protein, degree=5), data=train)
M6 = lm(Moisture~poly(Protein, degree=6), data=train)

#Do prediction with new data according to previous created model
M1_fit_train = predict.lm(M1, newdata=train)
M2_fit_train = predict.lm(M2, newdata=train)
M3_fit_train = predict.lm(M3, newdata=train)
M4_fit_train = predict.lm(M4, newdata=train)
M5_fit_train = predict.lm(M5, newdata=train)
M6_fit_train = predict.lm(M6, newdata=train)

M1_fit_val = predict.lm(M1, newdata=validation)
M2_fit_val = predict.lm(M2, newdata=validation)
M3_fit_val = predict.lm(M3, newdata=validation)
M4_fit_val = predict.lm(M4, newdata=validation)
M5_fit_val = predict.lm(M5, newdata=validation)
M6_fit_val = predict.lm(M6, newdata=validation)

MSE = function(y, y_hat) {
  squaredError = (y - y_hat)^2
  sum_squared_error = sum(squaredError)
  n = length(squaredError)
  return(sum_squared_error/n)
}

mse_train = numeric(6)
mse_train[1]=MSE(train$Moisture, M1_fit_train)
mse_train[2]=MSE(train$Moisture, M2_fit_train)
mse_train[3]=MSE(train$Moisture, M3_fit_train)
mse_train[4]=MSE(train$Moisture, M4_fit_train)
mse_train[5]=MSE(train$Moisture, M5_fit_train)
mse_train[6]=MSE(train$Moisture, M6_fit_train)

mse_validation = numeric(6)
mse_validation[1]=MSE(validation$Moisture, M1_fit_val)
mse_validation[2]=MSE(validation$Moisture, M2_fit_val)
mse_validation[3]=MSE(validation$Moisture, M3_fit_val)
mse_validation[4]=MSE(validation$Moisture, M4_fit_val)
mse_validation[5]=MSE(validation$Moisture, M5_fit_val)
mse_validation[6]=MSE(validation$Moisture, M6_fit_val)

par(mfrow=c(1,2))
plot(c(1:6), mse_train, type="l", col="pink")
plot(c(1:6), mse_validation, type="l", col="blue")

#4 --- Perform variable selection of a linear model in which Fat 
#is response and Channel1-Channel100 are predictors by using stepAIC.
#Fit a Ridge regression model with the same predictor and response variable 

install.packages("MASS")
library("MASS")

channels = tecator[,2:102]
set.seed(1234)

channel_model = glm(channels$Fat~., data = channels)
choose_var = stepAIC(channel_model, direction="backward")
#64 variables where choosen


#5 -- 
## Fit a Ridge regression model with the same predictor and response variables. 
#install.packages("glmnet") 
library("glmnet")

observation = scale(channels[,1:100]) #Normalize, important for Lasso/Ridge
response = scale(channels$Fat) #extract column Fat

set.seed(12345) 
#alpha=0 for ridge
predictionRidge = glmnet(as.matrix(observation), response, alpha=0,family="gaussian") 
par(mfrow=c(1,2)) 
plot(predictionRidge, xvar="lambda", label=TRUE)
# The coefficients goes towards 0, when lambda goes towards infinity

#6 
predictionLasso = glmnet(as.matrix(observation), response, alpha=1,family="gaussian") 
par(mfrow=c(1,2)) 
plot(predictionLasso, xvar="lambda", label=TRUE)

#7 
#install.packages("boot")
#library("boot")
library(glmnet)

lasso_modelfit_cv = cv.glmnet(as.matrix(observation), response, alpha=1, 
                              family="gaussian", lambda=seq(0,1,0.001)) 
plot(lasso_modelfit_cv, xvar="lamdba", label=TRUE)

