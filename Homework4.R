#HW4
# Part1
#a) Cp = 1/n(RSS+2d*sigma^2) = -2logL+2d = AIC

# Part2 
# Lasso Regression
# a (iv) Steadily decreases: As we increase ss from 00, all ???? 's increase from 00 to their least square estimate values. Training error for 00 ???? s is the maximum and it steadily decreases to the Ordinary Least Square RSS
# b (ii) Decrease initially, and then eventually start increasing in a U shape: When s=0s=0, all ???? s are 00, the model is extremely simple and has a high test RSS. As we increase ss, betabeta s assume non-zero values and model starts fitting well on test data and so test RSS decreases. Eventually, as betabeta s approach their full blown OLS values, they start overfitting to the training data, increasing test RSS.
# c (iii) Steadily increase: When s=0s=0, the model effectively predicts a constant and has almost no variance. As we increase ss, the models includes more ???? s and their values start increasing. At this point, the values of ???? s become highly dependent on training data, thus increasing the variance.
# d (iv) Steadily decrease: When s=0s=0, the model effectively predicts a constant and hence the prediction is far from actual value. Thus bias is high. As ss increases, more ???? s become non-zero and thus the model continues to fit training data better. And thus, bias decreases.
# e (v) Remains constant: By definition, irreducible error is model independent and hence irrespective of the choice of ss, remains constant.
# Ridge Regression
# a (iii) Steadily increase: As we increase ???? from 00, all ???? 's decrease from their least square estimate values to 00. Training error for full-blown-OLS ???? s is the minimum and it steadily increases as ???? s are reduced to 00.
# b (ii) Decrease initially, and then eventually start increasing in a U shape: When ??=0??=0, all ???? s have their least square estimate values. In this case, the model tries to fit hard to training data and hence test RSS is high. As we increase ????, betabeta s start reducing to zero and some of the overfitting is reduced. Thus, test RSS initially decreases. Eventually, as betabeta s approach 00, the model becomes too simple and test RSS increases.
# c (iv) Steadily decreases: When ??=0??=0, the ???? s have their least square estimate values. The actual estimates heavily depend on the training data and hence variance is high. As we increase ????, ???? s start decreasing and model becomes simpler. In the limiting case of ???? approaching infinity, all betabeta s reduce to zero and model predicts a constant and has no variance.
# d (iii) Steadily increases: When ??=0??=0, ???? s have their least-square estimate values and hence have the least bias. As ???? increases, ???? s start reducing towards zero, the model fits less accurately to training data and hence bias increases. In the limiting case of ???? approaching infinity, the model predicts a constant and hence bias is maximum.
# e (v) Remains constant: By definition, irreducible error is model independent and hence irrespective of the choice of ????, remains constant.

# Part3
# Lasso does a sparse selection, while Ridge does not.
# When you have highly-correlated variables, Ridge regression shrinks the two coefficients towards one another. Lasso is somewhat indifferent and generally picks one over the other. Depending on the context, one does not know which variable gets picked. Elastic-net is a compromise between the two that attempts to shrink and do a sparse selection simultaneously.
# Ridge estimators are indifferent to multiplicative scaling of the data. That is, if both X and Y variables are multiplied by constants, the coefficients of the fit do not change, for a given ???? parameter. However, for Lasso, the fit is not independent of the scaling. In fact, the ???? parameter must be scaled up by the multiplier to get the same result. It is more complex for elastic net.
# Ridge penalizes the largest ????'s more than it penalizes the smaller ones (as they are squared in the penalty term). Lasso penalizes them more uniformly. This may or may not be important. In a forecasting problem with a powerful predictor, the predictor's effectiveness is shrunk by the Ridge as compared to the Lasso.

# Part7 A
# a) Load and split the College data
library(ISLR)
set.seed(11)
sum(is.na(College))
train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = -train
College.train = College[train, ]
College.test = College[test, ]
# b) Number of applications is the Apps variable.
lm.fit = lm(Apps~., data=College.train)
lm.pred = predict(lm.fit, College.test)
mean((College.test[, "Apps"] - lm.pred)^2)
# c) Pick ???? using College.train and report error on College.test
library(glmnet)
train.mat = model.matrix(Apps~., data=College.train)
test.mat = model.matrix(Apps~., data=College.test)
grid = 10 ^ seq(4, -2, length=100)
mod.ridge = cv.glmnet(train.mat, College.train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best
ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - ridge.pred)^2)
# d) Pick ???? using College.train and report error on College.test
mod.lasso = cv.glmnet(train.mat, College.train[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best = mod.lasso$lambda.min
lambda.best
lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - lasso.pred)^2)
# Again, Test RSS is slightly higher that OLS, 16352801635280.
# The coefficients look like
mod.lasso = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
predict(mod.lasso, s=lambda.best, type="coefficients")
# e) Use validation to fit pcr
library(pls)
pcr.fit = pcr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, College.test, ncomp=10)
mean((College.test[, "Apps"] - data.frame(pcr.pred))^2)
# f) Use validation to fit pls
pls.fit = plsr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")
pls.pred = predict(pls.fit, College.test, ncomp=10)
mean((College.test[, "Apps"] - data.frame(pls.pred))^2)
# g) Results for OLS, Lasso, Ridge are comparable. Lasso reduces the F.UndergradF.Undergrad and BooksBooks variables to zero and shrinks coefficients of other variables. Here are the test R2R2 for all models.
test.avg = mean(College.test[, "Apps"])
lm.test.r2 = 1 - mean((College.test[, "Apps"] - lm.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
ridge.test.r2 = 1 - mean((College.test[, "Apps"] - ridge.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
lasso.test.r2 = 1 - mean((College.test[, "Apps"] - lasso.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
pcr.test.r2 = 1 - mean((College.test[, "Apps"] - data.frame(pcr.pred))^2) /mean((College.test[, "Apps"] - test.avg)^2)
pls.test.r2 = 1 - mean((College.test[, "Apps"] - data.frame(pls.pred))^2) /mean((College.test[, "Apps"] - test.avg)^2)
barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2), col="red", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test R-squared")

# Part7 B
# a) Best Subset Selection
set.seed(1)
library(MASS)
library(leaps)
library(glmnet)
# Best Subset Selection
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k = 10
p = ncol(Boston) - 1
folds = sample(rep(1:k, length = nrow(Boston)))
cv.errors = matrix(NA, k, p)
for (i in 1:k) {
  best.fit = regsubsets(crim ~ ., data = Boston[folds != i, ], nvmax = p)
  for (j in 1:p) {
    pred = predict(best.fit, Boston[folds == i, ], id = j)
    cv.errors[i, j] = mean((Boston$crim[folds == i] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")
which.min(rmse.cv)
rmse.cv[which.min(rmse.cv)]

# Lasso
x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim
cv.lasso = cv.glmnet(x, y, type.measure = "mse")
plot(cv.lasso)
coef(cv.lasso)
sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])

# Ridge regression
x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim
cv.ridge = cv.glmnet(x, y, type.measure = "mse", alpha = 0)
plot(cv.ridge)
coef(cv.ridge)
sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])

# PCR
library(pls)
pcr.fit = pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
summary(pcr.fit)
# 13 component pcr fit has lowest CV/adjCV RMSEP.

# b) See above answers for cross-validate mean squared errors of selected models.
# c) I would choose the 9 parameter best subset model because it had the best cross-validated RMSE, next to PCR, but it was simpler model than the 13 component PCR model.


















