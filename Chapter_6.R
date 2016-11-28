rm(list=ls())
library(ISLR)
library(leaps)
names(Hitters)
sum(is.na(Hitters))
hitters= na.omit(Hitters)
dim(hitters)
regfit.full = regsubsets(Salary~.,data = hitters)
summary(regfit.full)
regfit.full = regsubsets(Salary~.,data= hitters,nvmax=19)
names(summary(regfit.full))
summary(regfit.full)$rsq
params=1:19
plot(summary(regfit.full)$rsq,pch=20)
plot(summary(regfit.full)$rss,pch=20)
plot(summary(regfit.full)$adjr2,type='l')
which.max(summary(regfit.full)$adjr2)
points(11,summary(regfit.full)$adjr2[11],col='red',pch=20)
plot(regfit.full,scale='adjr2')
summary(regfit.full)$adjr2[10]
which.max(summary(regfit.full)$adjr2)
coef(regfit.full,12)
regfit.fwd = regsubsets(Salary~.,data = hitters)
which.max(summary(regfit.fwd)$adjr2)
coef(regfit.fwd,8)
dim(hitters)
train = sample(263,263/2,replace= TRUE)
length(train)
regfit.full = regsubsets(Salary~.,hitters,nvmax=19,subset=train)
regfit2.full = regsubsets(Salary~.,hitters[train,],nvmax=19)
test = -train
length(test)
mat_data <- model.matrix(Salary~.,data = hitters[test,])
colnames(mat_data)
regfit.full = regsubsets(Salary~.,hitters[train,],nvmax=19)
test_mse= rep(0,19)
for (i in 1:19){
  coeffi <- coef(regfit.full,i)
  coef_names = names(coef(regfit.full,i))
  predicted_y <- mat_data[,coef_names]%*%coeffi
  test_mse[i]=mean((predicted_y-hitters$Salary[test])^2)
}
which.min(test_mse)
test_mse

set.seed(1)
train = sample(c(TRUE,FALSE),nrow(hitters),rep=T)
train
regfit.best <- regsubsets(Salary~.,data = hitters[train,],nvmax=19)
test = (!train)
test.mat = model.matrix(Salary~.,hitters[test,])  
val.errors = rep(0,19)
for (i in 1:19){
  coeffi <-coef(regfit.best,i)
  coef_names <- names(coef(regfit.best,i))
  pred_y <- test.mat[,coef_names]%*%coeffi
  val.errors[i]<-mean((pred_y-hitters$Salary[test])^2)
}
which.min(val.errors)
val.errors
test_mse
names(regfit.full)
regfit.full$call[]
## function for the predict function of regsubsets
predict_regsubsets <- function(object,newdata,id,...){
  form = as.formula(object$call[[2]])
  print(form)
  mat_dat <- matrix.model(form,newdata)
  coeffi <- coef(object,id=id)
  coef_names = names(coeffi)
  pred_y <- mat_dat[,coef_names]%*%coeffi
  return (pred_y)
}
id=7
predict_regsubsets(regfit.best,hitters,id)
## Implementation of k-fold cross-validation for regsubsets
k = 10
set.seed(1)
cv.error <- matrix(0,nrow=k,ncol=19)

for (i in 1:10){
  data.train = hitters[markings!=i,]
  data.test =  hitters[markings==i,]
  regfit.full = regsubsets(Salary~.,data.train,nvmax = 19)
  for (j in 1:19){
    pred_y <- predict.regsubsets(regfit.full,data.test,j)
    cv.error[i,j]<-mean((pred_y-data.test$Salary)^2)
  }
  print(i)
  print(cv.error[i,])
}
  
  
