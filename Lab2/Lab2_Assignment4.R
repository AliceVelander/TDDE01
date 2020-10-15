#ASSIGNMENT 4 ---------

setwd("~/Studier+Studentliv/HT19/TDDE01/Labb2")
data_frame=read.csv2("NIRSpectra.csv")
RGNversion('3.5.1')

# ----1 -----Principal Component Analysis - PCA
source("https://bioconductor.org/biocLite.R") 
biocLite("pcaMethods")

data_frame$Viscosity=c()
PCA=prcomp(data_frame) 
lambda=PCA$sdev^2

#eigenvalues - bygger upp rummet
lambda 

par(mfrow = c(1,1))
prop_vari = sprintf("%2.3f",lambda/sum(lambda)*100) 
screeplot(res)

#Scores 
plot(PCA$x[,1], PCA$x[,2], ylim=c(-0.3,0.6)) 

#----- 2 TRACE PLOTS-------
U_new=PCA$rotation
head(U)

#loading - which variables are correlated?
par(mfrow = c(1,2))
plot(U_new[,1], main="Traceplot, PC1")
plot(U_new[,2],main="Traceplot, PC2")

#------ 3 ---------
library("fastICA")
set.seed(12345)

#ICA
result_ICA = fastICA(data_frame, 2, fun = "logcosh", alpha = 1, row.norm = FALSE)
Wprime = result_ICA$K%*%result_ICA$W  #matrix multiplication 

# Traceplots
par(mfrow = c(2,2))

plot(U_new[,1], main="Traceplot orginal, PC1")
plot(U_new[,2],main="Traceplot original, PC2")

plot(Wprime[,1], main="Traceplot ICA1") 
plot(Wprime[,2], main="Traceplot ICA2")

# Scores
par(mfrow = c(1,2))
res=prcomp(data_frame)
plot(res$x[,1], res$x[,2], ylim=c(-2,2), xlim=c(-0.3,3), xlab="PC1", ylab="PC2", main="Original PCA")
plot(result_ICA$S[,1], result_ICA$S[,2], xlab="PC1", ylab="PC2", main="ICA")


