rm(list = ls())
library(ISLR)
library(tree)
names(Carseats)
High = ifelse(Carseats$Sales<=8,"NO","YES")
Carseats = data.frame(Carseats,High)
names(Carseats)
tree.fit = tree(High~.-Sales,data = Carseats)
summary(tree.fit)
set.seed(2)
train = sample(400,200)
test = -train
train_data = Carseats[train,]
test_data = Carseats[test,]
fit.carseats = tree(High~.-Sales, data = train_data)
summary(fit.carseats)
preds = predict(fit.carseats,newdata = test_data,type = "class")
head(preds)
table(preds,test_data$High)
(86+57)/200
plot(fit.carseats)
text(fit.carseats,pretty=0)
fit.carseats
?cv.tree
cv.err = cv.tree(fit.carseats ,FUN=prune.misclass)
summary(cv.err)
names(cv.err)
cv.err$size
cv.err$dev
cv.err$k
par(mfrow=c(1,2))
plot(cv.err$size,cv.err$dev,pch = 16)
plot(cv.err$k,cv.err$dev,pch=16)
pruned.tree = prune.misclass(fit.carseats,best = 9)
plot(pruned.tree)
text(pruned.tree,pretty=0)
tree.pred = predict(pruned.tree,newdata = test_data,type = "class")
tree.pred
table(tree.pred,test_data$High)
(94+60)/200
cverr = cv.tree(fit.carseats,FUN = prune.misclass)
names(cverr)
cverr$size
cverr$k

# Fitting regression tree using the tree function
library(MASS)
set.seed(1)
names(Boston)
tree.boston = tree(medv~.,Boston,subset = train)
cv.tree.boston = cv.tree(tree.boston)
which.min(cv.tree.boston$dev)
pruned.tree.boston = prune.tree(tree.boston,best =2)
plot(pruned.tree.boston)
plot(cv.tree.boston$size, cv.tree.boston$dev,type = 'b')
which.min(cv.tree.boston$dev)
cv.tree.boston$size
cv.tree.boston$dev
cv.tree.boston$size[which.min(cv.tree.boston$dev)]
pruned.boston5 = prune.tree(tree.boston, best =5)
pruned.boston8 = prune.tree(tree.boston, best = 8)
dim(Boston)[1]
train = sample(dim(Boston)[1],dim(Boston)[1]/2)
test = -train
yhat5 = predict(pruned.boston5,newdata = Boston[test,])
yhat8 = predict(pruned.boston8,newdata = Boston[test,])
y = Boston$medv[test]
mse5 = mean((yhat5-y)^2)
mse8 = mean((yhat8-y)^2)
mse5
mse8
# Apply bagging and random forest to Boston data.
#Use Library Random Forest
install.packages('randomForest')
library(randomForest)
library(ISLR)

names(Boston)
random.boston = randomForest(medv~.,data = Boston,importance= TRUE,mtry = 12,ntree=25)
plot(random.boston)
tree(random.boston)
nrow(Boston)
set.seed(1)
train = sample(506,253)
test = -train
rand1.tree = randomForest(medv~.,data = Boston[train,],importance = TRUE,ntree = 25,mtry =6)
rand2.tree = randomForest(medv~., data = Boston,importance = TRUE,mtry =13,ntree =25)
pred_y1 = predict(rand1.tree,newdata = Boston[test,])
pred_y2 = predict(rand2.tree,newdata = Boston[test,])
mse1 = mean((pred_y1-Boston$medv[test])^2)
mse2 = mean((pred_y2-Boston$medv[test])^2)
mse1
mse2
install.packages("gbm")
library(gbm)
?gbm
boost.boston = gbm(medv~.,data = Boston,distribution = 'gaussian',interaction.depth = 4,n.trees=5000)
pred_y.gbm = predict(boost.boston,newdata = Boston[test,],n.trees= 5000)
mse.gbm = mean((pred_y.gbm-Boston$medv[test])^2)
mse.gbm
 
 