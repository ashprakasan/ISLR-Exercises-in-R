rm(list = ls())
library(ISLR)
library(MASS)
names(Smarket)
dim(Smarket)
# get correlation of all parameters with one another in matrix form
cor(Smarket) # Error because 'Direction' is non numeric
cor(Smarket[,-9])
Smarket[1,-5]
# Plot one parameter from the data
plot(Smarket$Volume)
dim(Smarket['Volume'])
length(Smarket$Volume)
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = binomial,data = Smarket)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
# The predict function can be used with glm 
glm.results = predict(glm.fit, data = smarket, type= "response")
glm.results[1:10]
plot(glm.results)
contrasts(Smarket$Direction)
glm.preds = rep("Down",length(Smarket$Direction))
glm.preds[glm.results>0.5] = "Up"
glm.preds[1:10]
glm.results[1:10]
table(glm.preds,Smarket$Direction)
sum(glm.preds== Smarket$Direction)
length(glm.preds)
652/1250
train_rows = Smarket$Year<2005
Smarket.train = Smarket[train_rows,]
dim(Smarket.train)
Smarket.test = Smarket[!train_rows,]
glm.train = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = binomial, data = Smarket.train)
summary(glm.train)
test_result = predict(glm.train, newdata = Smarket.test,type = "response")
length(test_result)
dim(Smarket.test)
test_preds = rep("Down",length(Smarket.test$Direction))
test_preds[test_result>0.5] = "Up"
length(test_preds)
length(Smarket.test$Direction)
table(test_preds,Smarket.test$Direction)
mean(test_preds == Smarket.test$Direction)
# Testing a different fit model
glm.fit2 = glm(Direction~Lag1+Lag2, family = binomial, data = Smarket.train)
summary(glm.fit2)
fit2.results = predict(glm.fit2,newdata = Smarket.test,type = "response")
dim(Smarket.test)
fit2.preds = rep("Down", length(Smarket.test$Direction))
fit2.preds[fit2.results >0.5] = "Up"
length(fit2.preds)
table(fit2.preds,Smarket.test$Direction)
mean(fit2.preds == Smarket.test$Direction)
Smarket.part = Smarket[Smarket$Lag1 == Smarket$Lag2,]
dim(Smarket.part)
particular_preds = predict(glm.fit2,newdata = Smarket.part,type = "response")
particular_preds = predict(glm.fit2,newdata = data.frame(Lag1= c(1.5), Lag2 = c(-0.8)), type= "response")
particular_preds
# Example of Linear Discriminant Analysis. It is included as a part of the MASS library and the syntax is
# exactly like lm and glm function calls
lda.fit = lda(Direction~Lag1+Lag2, data = Smarket.train)
lda.fit
contrasts(Smarket$Direction)
names(lda.fit)
lda.results = predict(lda.fit,newdata= Smarket.test)
names(lda.results)
lda.results$class
lda.results$posterior
table(lda.results$class,Smarket.test$Direction)
qda.fit =  qda(Direction~Lag1+Lag2, data = Smarket.train)
names(qda.fit)
qda.results = predict(qda.fit,newdata = Smarket.test)
table(qda.results$class,Smarket.test$Direction)
length(Smarket.test$Direction)
30+121/252
sum(qda.results$class == Smarket.test$Direction)
mean(qda.results$class == Smarket.test$Direction)
library(class)
train = (Smarket$Year<2005)
sum(!train)
train.X<- cbind(Smarket$Lag1,Smarket$Lag2)[train,]
dim(train.X)
test.X = cbind(Smarket$Lag1,Smarket$Lag2)[!train,]
dim(test.X)
train.Direction = Smarket$Direction[train]
set.seed(1)
length(train.Direction)
length(train.X)
train.X[1,]
knn.results = knn(train.X,test.X,train.Direction,k=1)
summary(knn.results)
table(knn.results,Smarket$Direction[!train])
mean(knn.results==Smarket$Direction[!train])
# Analysis of the Caravan Insurance Data set that is a part of the ISLR library
names(Caravan)
attach(Caravan)
summary(Caravan$Purchase)
search()
detach(Caravan)
search()
sapply(Caravan,class)
var(Caravan[,1])
var(Caravan)
std.X = scale(Caravan[,-86])
length(names(Caravan))
var(std.X[,1])
var(std.X[,2])
test = 1:1000
train = 1001:length(Caravan$Purchase)
X.train = Caravan[-test,-86]
dim(X.train)
X.test = Caravan[test,-86]
dim(X.test)
length(Caravan[test,86])
Y.train = Caravan$Purchase[train]
sum(is.na(X.test))
set.seed(1)
knn.preds = knn(X.train,X.test,Y.train,k=1)
names(knn.preds)
knn.preds
mean(knn.preds == Caravan$Purchase[test])
summary(Caravan$Purchase)
table(knn.preds,Caravan$Purchase[test])
