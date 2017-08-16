## Lab 6: Moving Beyond Linearity
library(ISLR)
?Wage
View(Wage)
attach(Wage)

## Lab 6.1: Polynomial Regression and Step Functions
## Fit a polynomial regression with degree=4
fit <-lm(wage~ poly(age,4), data=Wage)
summary(fit)
## ploy() function generates a basis of orthogonal polynomials

## Let us create a grid of values for age at which we want predictions 
agelims <-range(age)
age.grid <-seq(from=agelims[1], to =agelims[2])
preds <-predict(fit, newdata=list(age=age.grid), se= TRUE)
se.bands <-cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(age, wage, col="darkgrey")
title("Degree-4 Ploynomial")
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col= "red", lty=3)

## Some other ways of doing this in R. For example
## poly() function generates a basis of orthogonal polynomials
## We generate a basis of polynomials
fit.a <-lm(wage~ age + I(age^2)+ I(age^3)+ I(age^4), data=Wage)
summary(fit.a)
## Two methods provide the same fitted values
plot(fitted(fit), fitted(fit.a))

## Do polynomial regression with different degrees
fit.1  <-lm(wage~ age, data=Wage)
fit.2 <- lm(wage~ poly(age, 2), data=Wage)
fit.3 <- lm(wage~ poly(age, 3), data=Wage)
fit.4 <- lm(wage~ poly(age, 4), data=Wage)
fit.5 <- lm(wage~ poly(age, 5), data=Wage)

## Anova to compare different models 
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

## Polynomial logistic regression
fit.logit <-glm(I(wage>250) ~ poly(age, 4), data= Wage, family="binomial")
## Further details can be learnt from textbook ISLR: PAGE 292

## Step function
table(cut(age,4))
fit.cut <-lm(wage~ cut(age, 4), data= Wage)
summary(fit.cut)


## Lab 6.2: Splines
library(splines)
## Fit wage to age using a regression spline
fit <-lm(wage~ bs(age, knots=c(25, 40, 60)), data=Wage)
pred <-predict(fit, newdata=list(age=age.grid), se=TRUE)
plot(age, wage, col="grey")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2*pred$se, lty="dashed")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed")

## prespecified knots at age 25, 40 and 60 produces a spline with six basis functions (Recall that a cubic spline with three knots has seven degree of freedoms)
dim(bs(age, knots=c(25, 40, 60)))
## We could use the df option to produce a spline with knots at uniform quantiles of the data
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")

## Now we fit a natural spline with four degrees of freedom
fit2 <-lm(wage ~ ns(age, df=4), data= Wage)
pred2 <-predict(fit2, newdata=list(age=age.grid), se= TRUE)
lines(age.grid, pred2$fit, col="red", lwd=2)

## To fit a smoothing spline, we use the smooth.spline() function
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Smoothing Spline")
fit<- smooth.spline(age, wage, df=16)
fit2 <-smooth.spline(age, wage, cv=TRUE)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","6.8 DF"), col=c("red","blue"), lty=1, lwd=2, cex=0.8)

## Lab 6.3: Generalized Additive Models (GAM)
## We now fit a GAM to predict wage using natural spline functions of year and age treating education as a qualitative predictor
library(gam)
gam1 <-lm(wage ~ ns(year, 4) + ns(age, 5) + education, data=Wage)
par(mfrow=c(1,3))
plot.gam(gam1, se=TRUE, col="red")

## We next fit a GAM using smoothing splines rather than natural splines, here we need to use the gam library in R
## s() function, which is part of gam library, is used to indicate that we would like to use a smoothing spline
gam2 <-gam(wage~ s(year,4) + s(age,5) +education, data=Wage)
summary(gam2)
plot(gam2, se=TRUE, col="blue")

## Since the function of year looks rather linear, we can perform a series of ANOVA test in order to determine which of the these models is the best
## gam2: a GAM uses a spline function of year
## gam3: a GAM uses a linear function of year
## gam4: a GAM that excluds year
gam3 <-gam(wage~ year + s(age,5) + education, data=Wage)
gam4 <-gam(wage~ s(age,5) + education, data=Wage)
anova(gam4, gam3, gam2, test="F")

## we can make predictions from gam objects
pred <-predict(gam2, newdata=Wage)

## Further details can be learnt from textbook ISLR: PAGE 296
