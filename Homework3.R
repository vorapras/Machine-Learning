#HW3
# Part1 What are the advantages and disadvantages of k-fold cross-validation relative to:
# i)The validation set approach ?
# The validation set approach has two main drawbacks compared to k-fold cross-validation.
# First, the validation estimate of the test error rate can be highly variable (depending on precisely which observations are included 
# in the training set and which observations are included in the validation set).
# Second, only a subset of the observations are used to fit the model. Since statistical methods tend to perform worse when trained on fewer observations, 
# this suggests that the validation set error rate may tend to overestimate the test error rate for the model fit on the entire data set.
# ii)LOOCV ?
# The LOOCV cross-validation approach is a special case of k-fold cross-validation in which k=nk=n. This approach has two drawbacks compared to k-fold cross-validation. 
# First, it requires fitting the potentially computationally expensive model nn times compared to k-fold cross-validation which requires the model to be fitted only kk times. 
# Second, the LOOCV cross-validation approach may give approximately unbiased estimates of the test error, since each training set contains n???1n???1 observations; 
# however, this approach has higher variance than k-fold cross-validation (since we are averaging the outputs of nn fitted models trained on an almost identical set of observations, 
# these outputs are highly correlated, and the mean of highly correlated quantities has higher variance than less correlated ones). 
# So, there is a bias-variance trade-off associated with the choice of kk in k-fold cross-validation; typically using k=5k=5 or k=10k=10 yield test error rate estimates that suffer
# neither from excessively high bias nor from very high variance.

# Part2 Bias-Variance Trade-off for k-Fold CV. Suppose k < n, where n is the number observations.
# a) LOOCV is less bias than k-fold CV, why?
# the validation set approach can lead to overestimates of the test error rate, since in this approach the training set used to fit the statistical learning method contains only half the observations of the entire data set. 
# Using this logic, it is not hard to see that LOOCV will give approximately unbiased estimates of the test error, since each training set contains n ??? 1 observations, which is almost as many as the number of observations in the full data set. 
# And performing k-fold CV for, say, k = 5 or k = 10 will lead to an intermediate level of bias, since each training set contains (k ??? 1)n/k observations???fewer than in the LOOCV approach, but substantially more than in the validation set approach.
# Therefore, from the perspective of bias reduction, it is clear that LOOCV is to be preferred to k-fold CV.

# b) LOOCV has higher variance than k-fold CV, why?
# When we perform LOOCV, we are in effect averaging the outputs of n fitted models, each of which is trained on an almost identical set of observations; therefore, these outputs are highly (positively) corre- lated with each other. 
# In contrast, when we perform k-fold CV with k < n, we are averaging the outputs of k fitted models that are somewhat less correlated with each other, since the overlap between the training sets in each model is smaller. 
# Since the mean of many highly correlated quantities has higher variance than does the mean of many quantities that are not as highly correlated, the test error estimate resulting from LOOCV tends to have higher variance 
# than does the test error estimate resulting from k-fold CV.



# Part4 Boston Housing Data (Bootstrap)
#a) Based on this data set, provide an estimate for the population mean of ???medv???. Call this estimate ???? ??^
library(MASS)
attach(Boston)
mu.hat <- mean(medv)
mu.hat

#b) Provide an estimate of the standard error of ???? ??^. Interpret this result.
se.hat <- sd(medv) / sqrt(dim(Boston)[1])
se.hat

#c) Now estimate the standard error of ???? ??^ using the bootstrap. How does this compare to your answer from (b) ?
library(boot)
set.seed(1)
boot.fn <- function(data, index) {
  mu <- mean(data[index])
  return (mu)
}
boot(medv, boot.fn, 1000)
# The bootstrap estimated standard error of ???? ??^ of 0.4119 is very close to the estimate found in (b) of 0.4089.

#d) Based on your bootstrap estimate from (c), provide a 95% confidence interval for the mean of ???medv???. Compare it to the results obtained using t.test(Boston$medv).
t.test(medv)
# or
CI.mu.hat <- c(22.53 - 2 * 0.4119, 22.53 + 2 * 0.4119)
CI.mu.hat
# The bootstrap confidence interval is very close to the one provided by the t.test() function.

#e) Based on this data set, provide an estimate, ???? med??^med, for the median value of ???medv??? in the population.
med.hat <- median(medv)
med.hat

#f) We now would like to estimate the standard error of ???? med??^med. Unfortunately, there is no simple formula for computing the standard error of the median. Instead, estimate the standard error of the median using the bootstrap. Comment on your findings.
boot.fn <- function(data, index) {
  mu <- median(data[index])
  return (mu)
}
boot(medv, boot.fn, 1000)
# We get an estimated median value of 21.2 which is equal to the value obtained in (e), with a standard error of 0.3874 which is relatively small compared to median value.
#g) Based on this data set, provide an estimate for the tenth percentile of ???medv??? in Boston suburbs. Call this quantity ???? 0.1??^0.1.
percent.hat <- quantile(medv, c(0.1))
percent.hat

#h) Use the bootstrap to estimate the standard error of ???? 0.1??^0.1. Comment on your findings.
boot.fn <- function(data, index) {
  mu <- quantile(data[index], c(0.1))
  return (mu)
}
boot(medv, boot.fn, 1000)
# We get an estimated tenth percentile value of 12.75 which is again equal to the value obtained in (g), with a standard error of 0.5113 which is relatively small compared to percentile value.

# Part5
# We will now derive the probability that a given observation is part of a bootstrap sample. Suppose that we obtain a bootstrap sample from a set of nn observations.
# What is the probability that the first bootstrap observation is not the jth observation from the original sample ? Justify your answer.
# 1???1/n
# What is the probability that the second bootstrap observation is not the jth observation from the original sample ?
# 1???1/n
# Argue that the probability that the jth observation is not in the bootstrap sample is (1???1/n)^n
# As bootstrapping sample with replacement, we have that the probability that the jth observation is not in the bootstrap sample is the product of the probabilities that each bootstrap observation is not the jth observation from the original sample
# (1???1/n)???(1???1/n)=(1???1/n)^n









.