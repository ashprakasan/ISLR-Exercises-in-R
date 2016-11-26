ls()
rm(list= ls())
getwd()
library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
lm.fit =lm(medv~lstat, data = Boston)
names(lm.fit)
coefficients(lm.fit)
confint(lm.fit)
length(residuals(lm.fit))
attach(Boston)
y.result = predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval = "confidence")
y.result
confint(lm.fit)
predict(lm.fit,data.frame(lstat=c(5,10,15)), interval = "prediction")
abline(lm.fit,lwd = 3,col= "red",pch= 20)
plot(Boston$medv,Boston$lstat,col = 'darkgrey', pch = 20)
abline(lm.fit,lwd=2)
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
hatvalues(lm.fit)[375]
## Multiple Linear Regression
lm.fit= lm(medv~lstat+age,Boston)
summary(lm.fit)
lm.full = lm(medv~.,data = Boston)
library(car)
coefficients(lm.full)
summary(lm.full)
vif(lm.full)
cor(Boston$rad,Boston$tax)
lm.effective = lm(medv~.-rad, data = Boston)
coefficients((lm.effective))
# Including an interaction term
lm.interaction = lm(medv~lstat*age,data = Boston)
summary(lm.interaction)
coefficients(lm.interaction)
y.interaction = predict(lm.interaction)
# To directly get values from the summary
summary(lm.interaction)$adj.r.squared
#gives names of all parameters accessible from the model summary
lm.nonlinearfit = lm(medv~lstat+I(lstat^2))
summary(lm.nonlinearfit)
# Testing hypothesis using the anova function. The null hypothesis assumes that both models
#fit equally well. The alternate hypothesis is that the nonlinear model is better.
anova(lm.fit,lm.nonlinearfit)
plot(Boston$medv,Boston$lstat)
plot(lm.nonlinearfit)
# fitting a polynomial function
lm.poly = lm(medv~poly(lstat,5), data = Boston)
summary(lm.poly)
coefficients(lm.poly)
lm.morepoly = lm(medv~poly(lstat,8),data = Boston)
summary(lm.morepoly)

#Analysis of 'Carseats' data that is a part of ISLR library
names(Carseats)
lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)
# Contrasts function shows the encoding of categorical variables
contrasts(Carseats$ShelveLoc)

#Writing functions
LoadLibraries= function(){
  library(MASS)
  library(ISLR)
  print("The libraries have been loaded")
}
LoadLibraries
LoadLibraries()
