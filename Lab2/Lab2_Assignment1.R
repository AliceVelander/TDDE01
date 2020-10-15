setwd("~/Studier+Studentliv/HT19/TDDE01/Labb2")
crabs=read.csv("australian-crabs.csv")

#1 
plot(crabs[["CL"]], crabs[["RW"]], main="Black=Female, Red=Male First Correlation",
     xlab="CL", ylab="RW", col=crabs$sex) 

#2 LDA 
#install.packages("MASS")
library("MASS")

misscon_rate = function (conf_mat, fit_mat) {
  n=length(fit_mat[,1])
  miss_rate=(1-sum(diag(conf_mat))/n)
  print(miss_rate)
}

fitLda = lda(crabs$sex ~ CL + RW, data = crabs)
predict = predict(fitLda, newdata=crabs)

confusionmatrix<- table(crabs$sex, predict$class) 

misscon_rate(confusionmatrix, crabs) 

plot(crabs[["CL"]], crabs[["RW"]], main="Black=Female, Red=Male Predicted",
     xlab="CL", ylab="RW", col=predict$class) 

#3 - set prior(male) =0.1, prior(female)=0.9 

new_prior_lda = lda(crabs$sex ~ CL + RW, data = crabs, prior = c(0.1,0.9))
predict_new_prior = predict(new_prior_lda, newdata=crabs)
confusionmatrix_new_prior<- table(crabs$sex, predict_new_prior$class)
misscon_rate(confusionmatrix_new_prior, crabs) 

plot(crabs[["CL"]], crabs[["RW"]], main="Black=Female, Red=Male, New prior",
     xlab="CL", ylab="RW", col=predict_new_prior$class) 

# 4 -logistic regression instead of LDA-----------------------------------------

fit_logistic = glm(sex ~ CL + RW, data = crabs, family='binomial')

predict_logistic = predict(fit_logistic, newdata=crabs, type='response')
confusionmatrix_logistic<- table(crabs$sex, predict_logistic > 0.5)
misscon_rate(confusionmatrix_logistic, crabs) 

plot(x=crabs[["CL"]], y=crabs[["RW"]], main="Black=Female, Red=Male, Logistic Regression",
     xlab="CL", ylab="RW", col=ifelse (predict_logistic >0.5, "red", "black" ), ylim=c(0,20),xlim=c(0,50)) 

#Equation of decision boundary (since ln(0.5/0.5)=ln(1)=0 --> )

beta_0 = fit_logistic$coefficients[1]
beta_1 = fit_logistic$coefficients[2]
beta_2 = fit_logistic$coefficients[3]

CLIntercept <- - beta_0 / beta_2
slope <- - beta_1 / beta_2

par(new=TRUE)
curve(slope*x + CLIntercept,
                    from=0, to=50, add=TRUE, col="green")





