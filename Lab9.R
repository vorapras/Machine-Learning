## Lab9: Support Vector Machines
library(e1071)

## Lab 9.1: Support Vector Classifier
set.seed(1)
## We begin by generating the observations, which belong to two classes
x <-matrix(rnorm(20*2), ncol=2)
y <-c(rep(-1,10), rep(1,10))
x[y==1,] <-x[y==1,]+1
## We begin by checking whether the classes are linarly separable
plot(x, col=(3+y), pch=19)
## They are not!


## We fit the support vector classifier, to ask svm() function to perform classification, we must encode the response as a factor variable
## We now create a data frame with the response coded as a factor
dat <-data.frame(x=x, y=as.factor(y))
## svm() function can be used to fit a support vector classifier when the argument kernel="linear" is used
## A cost argument allow us to specifiy the cost of a violation to the margin. (length of the margin)
## When the cost argument is small, the margins will be wide and many support vectors will be on the margin or will violate the margin
svmfit <-svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
## You can play with smaller values of the cost argument, cost=0.1
print(svmfit)
## Display the identities of the support vectors
svmfit$index
## Plot the support vector classifier
plot(svmfit, dat)

## The plot looks a little bit jagged here, not too much control on the color and puts x1 and x2 not on a conventional way
## Below is a quick way of generating your own plot
## Read and understand the code by yourself if you donot like the plot function in the e1071 package
##########################################################################################################################
make.grid <-function(x, n=75){
  grange <-apply(x, 2, range)
  x1 <-seq(from=grange[1,1], to=grange[2,1], length=n)
  x2 <-seq(from=grange[1,2], to=grange[2,2], length=n)
  expand.grid(x.1=x1, x.2=x2)
}

xgrid <-make.grid(x)
ygrid <-predict(svmfit, newdata=xgrid)
plot(xgrid, col=c("red","blue")[as.numeric(ygrid)], pch=20, cex=0.2)
points(x, col=y+3, pch=19)
## Display points on the incorrect side of the margin
points(x[svmfit$index,], pch=5, cex=2)
beta <-drop(t(svmfit$coefs) %*% x[svmfit$index,])
beta0 <-svmfit$rho
## plot of linear decision boundary
abline(beta0/beta[2], -beta[1]/beta[2])
## plot of margins
abline((beta0-1)/beta[2], -beta[1]/beta[2], lty=2)
abline((beta0+1)/beta[2], -beta[1]/beta[2], lty=2)
##########################################################################################################################  

## we now use tune() function to perform 10-fold corss validation to determine an optimal cost parameter
set.seed(1)
tune.out <-tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
## we see that cost=0.1 results in the lowest cross validation error rate
## tune() function stores the best model obtained, which can be assessed as follows
bestmod <-tune.out$best.model
summary(bestmod)

## the predict() function is used to predict the class label on a set of test observations
## first generate the test observations
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <-sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] <-xtest[ytest==1,] + 1
testdat <-data.frame(x=xtest, y=as.factor(ytest))
## Now we predict the class labels of these test obervations. Here we use the best model obtained through cross validation in order to make predictions
ypred <-predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

## Lab 9.2: Support Vector Machine
## We first genrate some data with non-linear class boundary as follows
set.seed(1)
x <-matrix(rnorm(200*2), ncol=2)
x[1:100, ] <-x[1:100, ] +2
x[101:150, ] <-x[101:150, ] -2
y <-c(rep(1,150), rep(2, 50))
dat <-data.frame(x=x, y=as.factor(y))
## Points are not separable
plot(x, col=y, pch=19)

## randomly split the data into traning and testing groups
train <-sample(200, 100)
## We then fit the traning data using svm() function with a radial kernel with gamma=1 and cost=1
svmfit <-svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)

## We can perform cross validation using tune() to select the best choice of gamma and cost for an SVM with a radial kernel
set.seed(1)
tune.out <-tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
tune.out$best.model
## we see that cost=1 and gamma=2 results in the lowest cross validation error rate

## we can view the test set predictions for this model by applying the predict() function to the data
truth= dat[-train, "y"]
pred= predict(tune.out$best.model, newx=dat[-train,])
## Confusion matrix
table(pred,truth)
## Misclassification rate
mean(pred!=truth)

## After the class, you need to follow the textbook for the labs on the following sections
## Section 9.6.3 (ROC Curves)
## Section 9.6.4 (SVM with Multiple Classes)
## Section 9.6.5 (Applications to Gene Expression Data)


