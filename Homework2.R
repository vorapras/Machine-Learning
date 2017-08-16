#HW2
#Part1 Weekly Stock Market Data (Classification)
#a)
library(ISLR)
summary(Weekly)
attach(Weekly)
plot(Volume)
# The correlations between the ???lag??? variables and today???s returns 
# are close to zero. The only substantial correlation is between ???Year??? and ???Volume???. 
# When we plot ???Volume???, we see that it is increasing over time.
#b)
fit.glm = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(fit.glm)

#c)
glm.probs=predict(fit.glm,type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs > 0.5]="Up"
# Confusion matrix
table(glm.pred,Direction)
# We may conclude that the percentage of correct predictions on the training data is 
# (54+557)/1089(54+557)/1089 wich is equal to 56.1065197%. In other words 43.8934803% is 
# the training error rate, which is often overly optimistic. We could also say that for weeks 
# when the market goes up, the model is right 92.0661157% of the time (557/(48+557)557/(48+557)). 
# For weeks when the market goes down, the model is right only 11.1570248% of the time (54/(54+430)54/(54+430)).

#d)
# Using data from the begining up until 2009 as training data set
train = (Year < 2009)
# Testing data between 2009 and 2010
Weekly.20092010 = Weekly[!train, ]
Direction.20092010 = Direction[!train]
fit.glm2 = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(fit.glm2)
# Confusion matrix
probs2 = predict(fit.glm2, Weekly.20092010, type = "response")
pred.glm2 = rep("Down", length(probs2))
pred.glm2[probs2 > 0.5] = "Up"
table(pred.glm2, Direction.20092010)
# In this case, we may conclude that the percentage of correct predictions on the test data is 
# (9+56)/104(9+56)/104 wich is equal to 62.5%. In other words 37.5% is the test error rate. 
# We could also say that for weeks when the market goes up, the model is right 91.8032787% of the time 
# (56/(56+5)56/(56+5)). For weeks when the market goes down, the model is right only 20.9302326% of the time 
# (9/(9+34)9/(9+34)).

#e) Repeat by using LDA
library(MASS)
# LDA Model
fit.lda = lda(Direction ~ Lag2, data = Weekly, subset = train)
# Validate with the testing data
pred.lda = predict(fit.lda, Weekly.20092010)
setThreshold(pred.lda, 0.6)
table(pred.lda$class, Direction.20092010)
# In this case, we may conclude that the percentage of correct predictions on the test data is 62.5%. In other words 37.5% is the test error rate. 
# We could also say that for weeks when the market goes up, the model is right 91.8032787% of the time. For weeks when the market goes down, 
# the model is right only 20.9302326% of the time. These results are very close to those obtained with the logistic regression model which is not surpising.

#f)
# QDA Model
fit.qda = qda(Direction ~ Lag2, data = Weekly, subset = train)
fit.qda
# Validate with the testing data
pred.qda = predict(fit.qda, Weekly.20092010)
table(pred.qda$class, Direction.20092010)
# In this case, we may conclude that the percentage of correct predictions on the test data is 58.6538462%. 
# In other words 41.3461538% is the test error rate. We could also say that for weeks when the market goes up, 
# the model is right 100% of the time. For weeks when the market goes down, the model is right only 0% of the time. 
# We may note, that QDA achieves a correctness of 58.6538462% even though the model chooses ???Up??? the whole time !


#g)
# Perform K-nearest neighbours on the traning data set
library(class)
# Create training data for X
train.X=as.matrix(Lag2[train])
# Create testing data for X
test.X=as.matrix(Lag2[!train])
# Create training data for Y
train.Direction=Direction[train]
# Set k=1
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.20092010)
# In this case, we may conclude that the percentage of correct predictions on the test data is 50%. 
# In other words 50% is the test error rate. We could also say that for weeks when the market goes up, the model is right 50.8196721% of the time. 
# For weeks when the market goes down, the model is right only 48.8372093% of the time.

#h) Find optimal k
len = 499
Grid = seq(0,1,length.out = len)
Err = rep(1,len)
for (i in 1:len)
{
  knn.pred=knn(train.X,test.X,train.Direction,k=i)
  table(knn.pred,Direction.20092010)
  Err[i]=mean(knn.pred!=Direction.20092010)
}
plot(Grid,Err,xlab="Number of k",ylab="Misclassification Rate")

#i)
# If we compare the test error rates, we see that logistic regression and LDA have the minimum error rates, 
# followed by QDA and KNN.

#j) ROC
# ROC for logistic regression

Grid = seq(0.3,0.65,0.005)
TruePosRate = matrix(0,ncol=1,nrow=length(Grid))
FalsePosRate = matrix(0,ncol=1,nrow=length(Grid))
for(i in 1:length(Grid)){
fit.glm = glm(Direction ~ Lag2, data = Weekly, family = binomial)
glm.probs=predict(fit.glm,type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs > Grid[i]]="Up"
# Confusion matrix
CMat=table(glm.pred,Direction)
TruePosRate[i,1]=CMat[2,2]/(CMat[1,2]+CMat[2,2])
FalsePosRate[i,1]=CMat[2,1]/(CMat[1,1]+CMat[2,1])
}
plot(FalsePosRate,TruePosRate,xlab="False positive rate",ylab="True positive rate")

# ROC for K-nearest neighbours
len = 499
Grid = seq(0,1,length.out = len)
TruePosRate = matrix(0,ncol=1,nrow=length(Grid))
FalsePosRate = matrix(0,ncol=1,nrow=length(Grid))
for (i in 1:len)
{
  knn.pred=knn(train.X,test.X,train.Direction,k=i)
  # Confusion matrix
  CMat=table(knn.pred,Direction.20092010)
  TruePosRate[i,1]=CMat[2,2]/(CMat[1,2]+CMat[2,2])
  FalsePosRate[i,1]=CMat[2,1]/(CMat[1,1]+CMat[2,1])
}
plot(FalsePosRate,TruePosRate,xlab="False positive rate",ylab="True positive rate")

# Part2 Testing Error Classification
# logistic regression training error rate = 20% ,testing error rate = 30%
# 1-nearest neighbours average error rate is 18%
# We should select logistic regression since testin error rate in 1-nearest neighbours
# can be higher than 30% and no more than 35% and the other reason is K=1 can lead 
# to overfitted value in training data since the model itself is too flexible

# Part6 Summary of Classification Methods
# The difference between LDA and logistic regression: The
# linear coefficients are estimated differently. MLE for logistic models
# and estimated mean and variance based on Gaussian assumptions
# for the LDA. LDA makes more restrictive Gaussian assumptions
# and therefore expected to work better than logistic models if they
# are met.
# KNN is a completely non-parametric approach: no assumptions
# are made about the shape of the decision boundary. Therefore, we
# can expect this approach to dominate LDA and logistic regression
# when the decision boundary is highly non-linear. On the other
# hand, KNN does not tell us which predictors are important; we
# don???t get a table of coefficients with p-values.
# QDA serves as a compromise between the non-parametric KNN
# method and the linear LDA and logistic regression approaches.
# Since QDA assumes a quadratic decision boundary, it can
# accurately model a wider range of problems than can the linear
# methods. Though not as flexible as KNN, QDA can perform better
# in the presence of a limited number of training observations
# because it does make some assumptions about the form of the
# decision boundary.
