#HW5
# Part1
# Regression splines often give superior results to polynomial regression. 
# This is because unlike polynomials, which must use a high degree 
# (exponent in the highest monomial term, e.g. X15) to produce flexible fits, 
# splines intro- duce flexibility by increasing the number of knots but keeping the degree fixed. 
# Generally, this approach produces more stable estimates. Splines also allow us to place more knots, 
# and hence flexibility, over regions where the function f seems to be changing rapidly, and fewer knots 
# where f appears more stable

# Part2
x = -2:2
y = c(1 + 0 + 0, # x = -2
      1 + 0 + 0, # x = -1
      1 + 1 + 0, # x = 0
      1 + (1-0) + 0, # x = 1
      1 + (1-1) + 0 # x =2
)
plot(x,y)



# Part6
set.seed(1)
library(MASS)
attach(Boston)
# a)
lm.fit = lm(nox ~ poly(dis, 3), data = Boston)
summary(lm.fit)
dislim = range(dis)
dis.grid = seq(from = dislim[1], to = dislim[2], by = 0.1)
lm.pred = predict(lm.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, lm.pred, col = "red", lwd = 2)
# Summary shows that all polynomial terms are significant while predicting nox using dis. Plot shows a smooth curve fitting the data fairly well.

# b) We plot polynomials of degrees 1 to 10 and save train RSS.
all.rss = rep(NA, 10)
for (i in 1:10) {
  lm.fit = lm(nox ~ poly(dis, i), data = Boston)
  all.rss[i] = sum(lm.fit$residuals^2)
}
all.rss
# As expected, train RSS monotonically decreases with degree of polynomial.

# c) We use a 10-fold cross validation to pick the best polynomial degree.
library(boot)
all.deltas = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(nox ~ poly(dis, i), data = Boston)
  all.deltas[i] = cv.glm(Boston, glm.fit, K = 10)$delta[2]
}
plot(1:10, all.deltas, xlab = "Degree", ylab = "CV error", type = "l", pch = 20, 
     lwd = 2)
# A 10-fold CV shows that the CV error reduces as we increase degree from 1 to 3, stay almost constant till degree 5, and the starts increasing for higher degrees. We pick 4 as the best polynomial degree.

# d) We see that dis has limits of about 1 and 13 respectively. We split this range in roughly equal 4 intervals and establish knots at [4,7,11][4,7,11]. Note: bs function in R expects either df or knots argument. If both are specified, knots are ignored.
library(splines)
sp.fit = lm(nox ~ bs(dis, df = 4, knots = c(4, 7, 11)), data = Boston)
summary(sp.fit)
sp.pred = predict(sp.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, sp.pred, col = "red", lwd = 2)
# The summary shows that all terms in spline fit are significant. Plot shows that the spline fits data well except at the extreme values of disdis, (especially dis>10dis>10).

# e) We fit regression splines with dfs between 3 and 16.
all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = lm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = sum(lm.fit$residuals^2)
}
all.cv[-c(1, 2)]
# Train RSS monotonically decreases till df=14 and then slightly increases for df=15 and df=16.

# f) Finally, we use a 10-fold cross validation to find best df. We try all integer values of df between 3 and 16.
all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = cv.glm(Boston, lm.fit, K = 10)$delta[2]
}
plot(3:16, all.cv[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")
# CV error is more jumpy in this case, but attains minimum at df=10. We pick 1010 as the optimal degrees of freedom.

# Part7
# a)
set.seed(1)
library(ISLR)
library(leaps)
attach(College)
train = sample(length(Outstate), length(Outstate)/2)
test = -train
College.train = College[train, ]
College.test = College[test, ]
reg.fit = regsubsets(Outstate ~ ., data = College.train, nvmax = 17, method = "forward")
reg.summary = summary(reg.fit)
par(mfrow = c(1, 3))
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min.cp = min(reg.summary$cp)
std.cp = sd(reg.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "red", lty = 2)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = min(reg.summary$bic)
std.bic = sd(reg.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "red", lty = 2)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", 
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(reg.summary$adjr2)
std.adjr2 = sd(reg.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)
# All cp, BIC and adjr2 scores show that size 6 is the minimum size for the subset for which the scores are withing 0.2 standard deviations of optimum. We pick 6 as the best subset size and find best 6 variables using entire data.
reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coefi = coef(reg.fit, id = 6)
names(coefi)

# b)
library(gam)
gam.fit = gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) + 
                s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = College.train)
par(mfrow = c(2, 3))
plot(gam.fit, se = T, col = "blue")

# c)
gam.pred = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.pred)^2)
gam.err
gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss
test.rss
# We obtain a test R-squared of 0.77 using GAM with 6 predictors. This is a slight improvement over a test RSS of 0.74 obtained using OLS.

# d)
summary(gam.fit)
# Non-parametric Anova test shows a strong evidence of non-linear relationship between response and Expend, and a moderately strong non-linear relationship (using p value of 0.05) between response and Grad.Rate or PhD.











