rm(list=ls())
library(ISLR)
library(boot)
set.seed(1)
names(Auto)
dim(Auto)
#Use of sample function to create random test and train samples
train = sample(nrow(Auto),nrow(Auto)/2)
train_data = sample[train,]
lm.fit = lm(mpg~poly(horsepower,3),data= Auto,subset= train)
summary(lm.fit)
pred_vals <- predict(lm.fit,newdata = Auto[-train,])
length(pred_vals)
actual_y<- Auto$mpg[-train]
length(actual_y)
mse_test = mean((actual_y-pred_vals)^2)
mse_train<- mean((Auto$mpg[train]-predict(lm.fit))^2)
mse_train
lm.fit2<- lm(mpg~poly(horsepower,2),data = Auto,subset= train)
lm.fit3 <- lm(mpg~poly(horsepower,3),data= Auto,subset=train)
get_mse<-function(model,subset){
  return(mean(Auto$mpg[subset]- predict(model,newdata= Auto[-subset,]))^2)
}
get_mse(lm.fit2,train)
get_mse(lm.fit3,train)
glm.fit = glm(mpg~horsepower,data = Auto)
cv.err= cv.glm(Auto,glm.fit)$delta[1]
cv.glm(Auto,glm.fit)$delta[2]
set.seed(17)
cv.error.10= rep(0,10)
for (i in 1:10){
  print("inside the loop")
  glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
  print('fitting done')
  cv.error.10[i] <- cv.glm(Auto,glm.fit,K=10)$delta[1]
  print(i)
}
print(cv.error.10)
set.seed(17)
cv.error.10=rep(0 ,10)
for (i in 1:10){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error.10[i]=cv.glm(Auto ,glm.fit ,K=10) $delta [1]
    }
mean(cv.error.10)
#implementation of bootstrap sampling
boot.fn<-function(data,index){
  return(coefficients(lm(mpg~horsepower,data = data,subset= index)))
}
boot(Auto,boot.fn,R=1000)

train= sample(nrow(Auto),nrow(Auto),replace=TRUE)
summary(lm(mpg~horsepower,data = Auto,subset= train))















