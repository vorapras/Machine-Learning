
## Lab 4: Cross Validation and Bootstrap

## Cross Validation
library(ISLR)
plot(mpg~horsepower,data=Auto)
View(Auto)
dim(Auto)
attach(Auto)

# A validation set approach
# Randomly split the full data into half training and half testing
set.seed(1)
train <-sample(392,196)

# linear model: regree mpg on horsepower using training data set
lm.fit <-lm(mpg~horsepower,data=Auto,subset=train)

# -train index below selects only the observations that are not in the training set
# MSE OF 196 observations in the validation set
mean((mpg-predict(lm.fit,Auto))[-train]^2)

# we use poly() function to estimate the test error for the polynomial regressions
lm.fit2 <-lm(mpg~ poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <-lm(mpg~ poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


library(boot)
# Leave-one-out cv (LOOCV)
glm.fit <-glm(mpg~horsepower,data=Auto)
# cv.glm can be used to perform cv, if we use glm() to fit a model without passing in the family argument, then it performs linear regression like lm() function
?cv.glm
cv.glm(Auto, glm.fit)$delta ## Very slow (does not use formula (5.2) on page 180)

# write a simple function to use formula (5.2)
loocv <-function(fit){
  h <-lm.influence(fit)$h # leverage effect of the observations
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)

# Plot the cv errors vs degree of the polynomial
cv.error <-rep(0,5)
degree <-1:5
for(i in degree){
  glm.fit <-glm(mpg~ poly(horsepower,i), data=Auto)
  cv.error[i] <- loocv(glm.fit)
}
plot(degree, cv.error, type="b")

## 10 fold cross validation, you can try 5 fold CV by setting K=5  in cv.glm function
cv.error10 <-rep(0,5)
for(i in degree){
  glm.fit <-glm(mpg~poly(horsepower,i), data=Auto)
  # Note the value of K is the number of groups which the data should be split to estimate the CV error, by default K=n, i.e. LOOCV
  cv.error10[i] <-cv.glm(Auto, glm.fit, K=10)$delta[1] 
}
lines(degree, cv.error10, type="b", col="red")  

# Bootstrap
# We use Portfolio data set in the ISLR package
# alph.fn() function takes as input the (X,Y) data as well as a vector indicating which observations should be used to estimate alpha
alpha.fn <-function(data,index){
  X <-data$X[index]
  Y <-data$Y[index]
  (var(Y) -cov(X,Y))/(var(X)+ var(Y) - 2*cov(X,Y))
}

set.seed(1)
# randomly select 100 observations from 1 to 100 with replacement, i.e. construct a new bootstrap data and compute the corresponding alpha
alpha.fn(Portfolio, sample(100,100, replace=T))

# Produce R=1000 bootstrap estimates for alpha
boot(Portfolio, alpha.fn, R=1000)

# Estimating the accuracy of a linear regression model
boot.fn <-function(data,index){
  coef(lm(mpg~horsepower, data=data,subset=index))
}
# This returns the intercept and slope estimates for the linear regression model
boot.fn(Auto, 1:392)
set.seed(1)
boot.fn(Auto, sample(392,392, replace=T))

# Now we use boot() to compute the standard errors of 1000 bootstrap estimates for the intercept and slope
boot(Auto, boot.fn, R=1000)
# Compare with standard formula results for the regression coefficients in a linear model

summary(lm(mpg~horsepower, data=Auto))$coef
# HW, what can you conclude from the different results(Std.Error)?
# Regression provide more consistent result if the error term from the data is based on normal distribution.
# However,if the error term is not defined or not normally distributed, the bootstrap technique is more valid

# Redo everything for polynomial regression with degree=2
boot.fn <-function(data, index)
  coefficients(lm(mpg~ horsepower + I(horsepower^2), data=data, subset=index))
set.seed(1)
# Bootstrap with 1000 replications
boot(Auto, boot.fn, 1000)
summary(lm(mpg~ horsepower + I(horsepower^2), data= Auto))$coef

# Bootstrap technique is useful when the error term is not normally distributed and 
# if we want to get more consisitent estimators for time series data, just google block-bootstapping
