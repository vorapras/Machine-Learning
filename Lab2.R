# Lab 2 Libraries

library(MASS)
library(ISLR)

# Lab 2 Simple Linear Regression

fix(Boston)
names(Boston)
?Boston
lm.fit=lm(medv~lstat)
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
plot(lm.fit)
names(lm.fit)
lm.fit$coefficients
coef(lm.fit)
# Confidenc interval for the coefficient estimates
confint(lm.fit) 

# Preditive values, confidence intervals and prediction intervals for the prediction of medv for a given value of lstat
predict(lm.fit,data.frame(lstat=(c(5,10,15))))
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")

plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))
# plot of fitted values vs (standardized) residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
# plot of leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#Lab 2 Multiple Linear Regression
lm.fit1=lm(medv~lstat+age,data=Boston)
summary(lm.fit1)
lm.fit2=lm(medv~.,data=Boston)
summary(lm.fit2)
lm.fit3=lm(medv~.-age,data=Boston)
summary(lm.fit3)
lm.fit4 <-lm(medv~lstat+age+tax+rad,data=Boston)
summary(lm.fit4)
anova(lm.fit1, lm.fit4)  ## F test, anova() function performs a hypothesis test comparing the two models

summary(lm(medv~.-age+lstat:black,data=Boston))
summary(lm(medv~.-age+I(rm^2),data=Boston))
summary(lm(medv~.-age+log(rm),data=Boston))
summary(lm(medv~lstat*age,data=Boston))


# Lab 2 Categorical Predictors

fix(Carseats)
names(Carseats)
# Predict sales (child car seat sales) in 400 locations on a number of predictors
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)
# ShelveLoc: an indicator of the quality of the shelving location, i.e. the space within a store in which the car seat is displayed at each location
contrasts(ShelveLoc)
contrasts(Urban)

# Lab 2 Writing Functions
LoadLibraries
LoadLibraries()
LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()
