# Lab 5 on Best Subset Selection, Forward and Backward Stepwise Selection, Validation Set Approach and K fold Cross Validation

library(ISLR)
?Hitters
summary(Hitters)
View(Hitters)
Hitters$Salary
## Some missing values in Salary
sum(is.na(Hitters$Salary))

## Remove those missing values
Hitters <-na.omit(Hitters)
dim(Hitters)
## Check any missing values?
sum(is.na(Hitters))

############################################## Lab 5.1: Best Subset Selection
## regsubsets() function perfroms best subset selection by identifying the best model that contains a given number of predictors, where best is quantified by RSS
library(leaps)
regfit.full  <-regsubsets(Salary~., data=Hitters)
reg.summary <-summary(regfit.full)
## It gives by default best-subsets up to size 8; let us increase to 19
regfit.full <-regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary <-summary(regfit.full)
names(reg.summary)
reg.summary$rsq

## Plot RSS, adjusted Rsq, Cp and BIC for all of the models at once, this would help us decide which model to select
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS")

## Plot adjusted R2 vs Number of Variables
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
## which.max() function is used to identify the location of the maximum point of a vector
which.max(reg.summary$adjr2)
## Plot a red dot to indicate the model with the largest adjusted R2
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)

## In a similar fashion, we can plot Cp and BIC
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)

plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col="red", cex=2, pch=20)

## regsubsets() function has a build-in plot() command which can display the selected variable for the best model with a given number of predictors
# black colour means that it is selected
par(mfrow=c(1,1))
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

## Top row of each plot contains a black square for each variable selected according to the optimal model associated with that statistic
## e.g. BIC choose seven-variable model we use coef() to see the coefficient estimates associated with this model
coef(regfit.full, 7)

################################################ Lab 5.2: Forward and Backward Stepwise Selection
## regsubsets() function can perform forward or backward stepwise selection as
regfit.fwd <-regsubsets(Salary~., data=Hitters,nvmax=19, method="forward")
summary(regfit.fwd)
par(mfrow=c(2,2))
plot(regfit.fwd, scale="r2")
plot(regfit.fwd, scale="adjr2")
plot(regfit.fwd, scale="Cp")
plot(regfit.fwd, scale="bic")

regfit.bwd <-regsubsets(Salary~., data=Hitters,nvmax=19, method="backward")
summary(regfit.bwd)
plot(regfit.bwd, scale="r2")
plot(regfit.bwd, scale="adjr2")
plot(regfit.bwd, scale="Cp")
plot(regfit.bwd, scale="bic")

## Check the coefficient estimates associated with models (size 7) using different approaches, e.g. best subset selection, forward stepwise selection and backward stepwise selection
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

################################################ Lab 5.3 Choosing among models using Validation Set Approach and Cross-Validation
set.seed(1)

## Validation Set Approach, randomly split the data into training set and validation data
train <-sample(seq(263), 180, replace=FALSE)
train
## We now apply resubsets() on the training data to perform forward stepwise selection
regfit.fwd <-regsubsets(Salary~.,data=Hitters[train,], nvmax=19, method="forward")

val.errors <- rep(0,19)
## We make a model matrix from the testing data
test.mat <-model.matrix(Salary~., data=Hitters[-train,])

## Try all models with size i ranges from 1 to 19
for(i in 1:19){
  ## Get the coefficient estimates associated with model (size i) using forward stepwise method
  coef.i <-coef(regfit.fwd, id=i)
  ## Get the prediction for the tesing data using the corresponding columns in the design matrix X multipled by the estimated coefficients in coef.i 
  pred.test <-test.mat[, names(coef.i)] %*% coef.i
  ## Compute the mean square error
  val.errors[i] <-mean((Hitters$Salary[-train]-pred.test)^2)
}
which.min(val.errors)
coef(regfit.fwd, 5)

par(mfrow=c(1,1))
## Plot of Root MSE vs model size for validation data
plot(sqrt(val.errors) ,ylab="Root MSE", ylim=c(300,400), pch=19, type="b")
## Plot of Root MSE vs model size for training data
points(sqrt(regfit.fwd$rss[-1]/180), col="red", pch=19, type="b")
legend("topright",legend=c("Validation","Training"), col=c("red","black"), pch=19)

## There is no predict() method for rersubsets(), we summarize the steps for our computation above and write our own version of the predict function as
predict.regsubsets <-function(object, newdata, id,...){
  form <-as.formula(object$call[[2]])
  mat <-model.matrix(form, newdata)
  coef.i <-coef(object, id=id)
  mat[,names(coef.i)] %*% coef.i
}

## Repeat the above steps for computing test error rate for models with size from 1 to 19
val.errors2 <-rep(0, 19)
for(i in 1:19){
  val.errors2[i] <-mean((Hitters$Salary[-train] - predict.regsubsets(regfit.fwd, Hitters[-train,], id=i))^2)  
}
## Check whether our written function could provide the same result or not
sum(abs(val.errors2-val.errors))


## K-Cross Validation Approach using forward stepwise selection (FSS), this part is very important, since it provides you a sample code of writing K-fold CV
K <-10
set.seed(11)
folds <-sample(rep(1:10, length=nrow(Hitters)))
folds
table(folds)
## We initialize a error matrix with row (10 different folds) and column (19 different predictors)
cv.errors <-matrix(0, 10, 19)
## We write a for loop that performs cross-validation, in the kth fold, the elemetns of folds that equal k are in the test set and the remiander are in the training set
for(k in 1:10){
  fit.fwd <- regsubsets(Salary~., data=Hitters[folds!=k,], nvmax=19, method="forward")
  for(i in 1:19){
    pred <-predict(fit.fwd, Hitters[folds==k,], id=i)
    cv.errors[k,i] <-mean((Hitters$Salary[folds==k]-pred)^2)
  }
}

## Average of the cv.error over all 10 folds
rmse.cv <-sqrt(apply(cv.errors,2,mean))
## Plot of Root MSE vs model size and choose the optimal model size
plot(rmse.cv, ylab="Root MSE", xlab="Model Size", pch=19, type="b")
which.min(rmse.cv)
points(which.min(rmse.cv), rmse.cv[which.min(rmse.cv)], col="red", cex=2, pch=20)
