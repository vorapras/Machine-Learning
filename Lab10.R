install.packages("neuralnet")
library(neuralnet)
## Traning of neural networks published in R journal 2010
## By Frauke Gunther and Stefan Fritsch
?infert
dim(infert)
attach(infert)
?neuralnet

## Randomly split the data into training and testing data.
train <-sample(248, 124)

## err.fct is used to calculate the error, you can set "sse" and "ce" 
## for sum of squared errors and cross-entropy, respectively.

## For classification problem, set the linear.ouput be FALSE that 
## the output of the activation function is mapped to the interval [0,1]

## Default algorithm "is Resilient Backpropagation".

nn <- neuralnet(case ~ age + parity + induced + spontaneous, 
                data=infert[train, ], hidden=2, err.fct="ce", linear.output=FALSE)

summary(nn)
plot(nn)

## hidden is a vector of integers specifying the number of hidden units in each layer.
## E.g set hidden=c(3,2,1) induces a neural network with three hidden layers
## the first with three units, the second with two units and the third with one unit

nn1 <- neuralnet(case ~ age + parity + induced + spontaneous, 
                data=infert[train, ], hidden=c(2,1), err.fct="ce", linear.output=FALSE)
plot(nn1)

# The number of steps to converge  and results can be changed each time since the random starting value is different.

## A list containing the overall result of the nn for every petition
nn$net.result
## A list containing the fitted weights of the nn for every petition
nn$weights
nn$result.matrix

## See the covariates of the training data
nn$covariate
## See the responses
infert$case

## Predicted probabilities
nn$net.result[[1]]
## Produce predicted binary output setting threshold value=0.5
Y.train <-ifelse(nn$net.result[[1]]>0.5, 1, 0)
Y.train

## misclassification error rate for training data
mean(infert$case[train] !=  Y.train)

## Apply backpropagation algorithm with learning rate=0.01
nn.bp <- neuralnet(case ~ age + parity + induced + spontaneous, data=infert[train, ], 
                   hidden=2, learningrate=0.01, algorithm="backprop", err.fct="ce", linear.output=FALSE)
plot(nn.bp) 

## Construct covariate matrix for testing data
X.test <-cbind(age, parity, induced, spontaneous)[-train, ]
## Use nn for prediction for testing data
prob.test <-compute(nn, covariate = X.test)
Y.test <-ifelse(prob.test$net.result > 0.5, 1, 0)
## misclassification error rate for testing data
mean(infert$case[-train] !=  Y.test)
## You can use this for cross-validation

## Construct the confidence intervals
ci <-confidence.interval(nn, alpha=0.05)
ci
par(mfrow=c(2,2))
gwplot(nn, selected.covariate = "age", min=-2.5, max=5)
gwplot(nn, selected.covariate = "parity", min=-2.5, max=5)
gwplot(nn, selected.covariate = "induced", min=-2.5, max=5)
gwplot(nn, selected.covariate = "spontaneous", min=-2.5, max=5)
