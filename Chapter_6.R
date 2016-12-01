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
  mat_dat <- model.matrix(form,newdata)
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
markings = sample(1:10,nrow(hitters),replace= TRUE)
for (i in 1:10){
  data.train = hitters[markings!=i,]
  data.test =  hitters[markings==i,]
  regfit.full = regsubsets(Salary~.,data.train,nvmax = 19)
  for (j in 1:19){
    pred_y <- predict_regsubsets(regfit.full,data.test,j)
    cv.error[i,j]<-mean((pred_y-data.test$Salary)^2)
  }
  print(i)
  print(cv.error[i,])
}
mean_errors = apply(cv.error,2,mean)
mean_errors
which.min(mean_errors)

## LAB 2 Ridge regression and lasso

# glmnet is the library used. It does not use formula y~x,
#instead uses a matrix
library(glmnet)
x=model.matrix(Salary~.,hitters)
x = x[,-1]
y = hitters$Salary
dim(x)
lambdas = 10^seq(10,-2,length = 100)
?glmnet
ridge.fit = glmnet(x,y,alpha =0,lambda = lambdas)
dim(coef(ridge.fit))
summary(ridge.fit)
ridge.fit$call
ridge.fit$lambda
ridge.fit$lambda[50]
coef(ridge.fit)[,50]
sqrt(sum((coef(ridge.fit)[-1,50])^2))
ridge.fit$lambda[60]
sqrt(sum((coef(ridge.fit)[-1,60])^2))
# predict funtion can be used to try different values of lambda.
#the values of lambda is passed as parameter,s
predict(ridge.fit,s=50,newx=x)
train = sample(nrow(hitters),nrow(hitters)/2)
test = -train
train.data = hitters[train,]
test.data = hitters[test,]
train.x = model.matrix(Salary~.,train.data)[,-1]
train.y = train.data$Salary
ridge.fit = glmnet(train.x,train.y,lambda= lambdas)
test.x = model.matrix(Salary~.,hitters[test,])[,-1]
test.y = hitters$Salary[test]
mse = rep(0,100)
x = model.matrix(Salary~.,hitters)[,-1]
y = hitters$Salary
cvoutput = cv.glmnet(x,y,alpha = 0,lambda = lambdas)
names(cvoutput)
bestlam = cvoutput$lambda.min
cvout2 = cv.glmnet(train.x,train.y,alpha=0,lambda = lambdas)
cvout2$lambda.min
out = glmnet(x,y,alpha=0)
predict(out,type = "coefficients",s=bestlam)

## Implementation of the lasso function
lasso.mod = glmnet(x,y,alpha=1,lambda= lambdas)
plot(lasso.mod)
set.seed(1)
cv.out= cv.glmnet(train.x,train.y,alpha =1)
plot(cv.out)
bestlam = cv.out$lambda.min
pred_vals = predict(lasso.mod,s= bestlam,test.x)
mean((pred_vals-test.y)^2)
dim(coef(lasso.mod))
coef_values = predict(lasso.mod,s=bestlam,type = 'coefficients')
coef_values
sum(coef_values>0)
coef_values[coef_values!=0]
## PCR and PLS Regression
library(pls)
set.seed(1)
train = sample(nrow(hitters),nrow(hitters)/2)
test = -train
pcr.fit = pcr(Salary~.,data = hitters,subset = train,validation = "CV")
validationplot(pcr.fit)
test_data = model.matrix(Salary~.,data = hitters)
test_data = test_data[test,-1]
nrow(test_data)
test_data
predicted_y <- predict(pcr.fit,test_data,comp=7)
length(predicted_y)
x = model.matrix(Salary~.,data = hitters)
y = hitters$Salary
nrow(x)
train= sample(1:nrow(x),nrow(x)/2)
test = -train
y.test = y[test]
x.test= x[test,-1]
x.train = x[train,]
y.train = y[train]
pcr.fit = pcr(Salary~.,data = hitters,scale= TRUE,validation = "CV")
pcr.pred = predict(pcr.fit,x.test,comp =7)
length(pcr.pred)
mean(pcr.pred-y.test)
test_data = model.matrix(Salary~.,hitters)
pcr.newfit = pcr(Salary~.,data = hitters,scale= TRUE,ncomp =7)
pcr.newfit
summary(pcr.newfit)
### We implement the partial least squares using the plsr function which is part of the pls library
plsr.fit = plsr(Salary~.,data = hitters,subset = train, scale = TRUE,valisation = "CV")
summary(plsr.fit)
x.test = model.matrix(Salary~.,data = hitters[test,])[,-1]
plsr.pred = predict(plsr.fit,newdata= x.test,ncomp= 10)
length(plsr.pred)
mean((plsr.pred-y.test)^2)
plsr.pred2 = predict(plsr.fit,newdata = x.test,ncomp = 2)
mean((plsr.pred2-y.test)^2)
summary(plsr(Salary~.,data = hitters,subset = train,ncomp =2))
