## Lab 8: Classification and Regression Trees, Bagging and Random Forest
library(tree)
library(ISLR)

## Lab 8.1.1: Classification Tree
## We analyze Carseats data set
attach(Carseats)
## We create a High variable, which takes on a value of Yes if the Sales variable exceeds 8, and takes on a value of No otherwise
High <-ifelse(Sales <=8, "No", "Yes")
## We use data.frame() function to merge High with the rest of Carseats data
Carseats <-data.frame(Carseats, High)
## We use the tree() function to fit a classification tree in order to predict High using all variables but Sales
tree.carseats <-tree(High~. -Sales, data=Carseats)
## The summary() function lists the variables that are used as internal nodes in the tree, number of terminal nodes and the traning error rate
## Please read page 325 of ISLR for the defination of residual mean deviance reported below
summary(tree.carseats)
## Show the tree plot graphically
plot(tree.carseats)
## We use text() function to display nodel labels; pretty=0 instructs R to include the category names for any qualitative predictors, rather than simply displaying a letter for each category
text(tree.carseats, pretty=0)
## output corresponding to each branch of the tree
tree.carseats

## We estimate the test error using 200 of 400 as traning set and the rest as test data
set.seed(2)
train <-sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <-High[-train]
## Run classification tree on the traning data
tree.carseats <-tree(High~.-Sales, Carseats, subset=train)
## Predict the class on the test data
tree.pred <-predict(tree.carseats, Carseats.test, type="class")
## Confusion matrix
table(tree.pred, High.test)
## Mis-classification error
mean(tree.pred!=High.test)

## We next consider whether prunning the tree might leadt to improved results
## The function cv.tree() performs cross-validation in order to determine the optimal level of tree complexity
## Cost complexity pruning is used to select a sequence of trees for consideration
## We use the argument FUN=prune.misclass to indicate that we want the classification error rate to guild the cross-validation and pruning process, 
## rather than the default for the cv.tree() which id deviance
set.seed(3)
cv.carseats <-cv.tree(tree.carseats, FUN= prune.misclass)
## size: the number of terminal nodes of each tree considered
## dev: corresponding error rate
## k: value of the cost-complexity parameter, which corresponds to alpha used in our slides
names(cv.carseats)
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")
## The optimal number of terminal node is 9 and we display the pruned tree graphically
par(mfrow=c(1,1))
prune.carseats <-prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

## Compute the test error rate using the pruned tree 
tree.pred <-predict(prune.carseats, Carseats.test, type="class")
table(tree.pred,High.test)
mean(tree.pred!=High.test)


## Lab 8.1.2: Regression Tree
## We use the Boston Housing dataset again here
library(MASS)
set.seed(1)
train <-sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <-tree(medv~., Boston, subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0)

cv.boston <-cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")
## In this case, the most complex tree is selected by cross-validation
## However, if we wish to prune the tree, we could do so as follows using prune.tree() function
prune.boston <-prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

## In keeping with the CV results, we use the unpruned tree to make predictions on the test data set
## Predicted values on the testing data using regression tree
yhat <-predict(tree.boston, newdata=Boston[-train,])
## True values on the testing data
boston.test <-Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
## Compute the test MSE
mean((yhat-boston.test)^2)

## Lab 8.2: Bagging and Random Forest
library(randomForest)
set.seed(1)
## Recall that bagging is simply a special case of a random forest with m=p, here we use mtry=13, i.e. bagging would be done
bag.boston <-randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston

## Predicted values on the testing data using bagging
yhat.bag <-predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
## Compute the test MSE
mean((yhat.bag-boston.test)^2)
## We can view the importance of each variable
importance(bag.boston)
varImpPlot(bag.boston)
## The plot indicates lstat and rm are two most important variables

## We could change the number of trees grown by randomForest() using ntree argument
bag.boston <-randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag <-predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

## Growing a random forest is exactly the same way.
## By default, randomForest() uses p/3 variables when building a random forest of regression trees
## and root p variables when building a random forest of classiciation trees. 
## We use mtry=6 here
set.seed(1)
rf.boston <-randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf <-predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

## Lab 8.3: Boosting
## Please learn the topic of Boosting from Session 8.3.4 of the textbook ISLR 
library(gbm)
