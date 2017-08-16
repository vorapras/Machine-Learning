# Lab 3 Stock Market Data

library(ISLR)
?Smarket
fix(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)


# Lab 3 Logistic Regression

# fit a gernalized linear regression using a logit link function, set distribution of the response variable to be binomial
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]

contrasts(Direction)
glm.pred=rep("Down",1250)
# For predicted probabilities greater than 0.5, assign Y to be "Up"; otherwise assign Y to be "Down"
glm.pred[glm.probs>.5]="Up" 
# Confusion matrix
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)

# Generate training (before year 2005) and testing data sets (on year 2005) 

# ID for training data
train=(Year<2005)
# Create testing data set
Smarket.2005=Smarket[!train,]
# Create testing data for Y
Direction.2005=Direction[!train]

# logistic regression on the training data set
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)

# Predicted probabilities for the testing data set
glm.probs=predict(glm.fit,Smarket.2005,type="response")

# Sample size for the testing data
dim(Smarket.2005)

# For predicted probabilities greater than 0.5, assign Y to be "Up"; otherwise assign Y to be "Down"
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"

# Confusion matrix
table(glm.pred,Direction.2005)
# Proportation of make correct classification
mean(glm.pred==Direction.2005)
# Misclassfication error rate
# glm.pred is the predicted Y for testing data and Direction.2005 is the true Y for testing data
mean(glm.pred!=Direction.2005)

# Redo the logistic regression using only two predictors, 1st lag and 2nd lag
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
# Proportation of make correct classification
table(glm.pred,Direction.2005)
# Misclassfication error rate
mean(glm.pred!=Direction.2005)


# Lab 3 Linear Discriminant Analysis

library(MASS) 
# Perform LDA on the traning data set using only two predictors, 1st lag and 2nd lag
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
names(predict(lda.fit,Smarket.2005))
predict(lda.fit,Smarket.2005)$posterior
lda.pred=predict(lda.fit,Smarket.2005)$class
lda.pred

# Confusion matrix
table(lda.pred,Direction.2005)
# Misclassfication error rate
mean(lda.pred!=Direction.2005)
mean(lda.pred==Direction.2005)

# Lab 3 Quadratic Discriminant Analysis}}
# Perform QDA on the traning data set using only two predictors, 1st lag and 2nd lag
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.pred=predict(qda.fit,Smarket.2005)$class

# Confusion matrix
table(qda.pred,Direction.2005)
# Misclassfication error rate
mean(qda.pred!=Direction.2005)


# Lab 3 k Nearest Neighbors
# Perform K-nearest neighbours on the traning data set
library(class)
# Create training data for X
train.X=cbind(Lag1,Lag2)[train,]
# Create testing data for X
X.2005=cbind(Lag1,Lag2)[!train,]
# Create training data for Y
train.Direction=Direction[train]

# The more number of k, the model become less flexible
# To find the optimal k, plot different k and get U shape (Select the bottom one) 
# before deciding which k is the most suitable.(Bias-Variance Tradeoff)

# Set k=1
knn.pred=knn(train.X,X.2005,train.Direction,k=1)
table(knn.pred,Direction.2005)
mean(knn.pred!=Direction.2005)

# Set k=3
knn.pred=knn(train.X,X.2005,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred!=Direction.2005)

len = 499
Grid = 1:len
Err = rep(1,len)
for (i in 1:len)
{
  knn.pred=knn(train.X,X.2005,train.Direction,k=i)
  table(knn.pred,Direction.2005)
  Err[i]=mean(knn.pred!=Direction.2005)
}
plot(Grid,Err,xlab="Number of k",ylab="Misclassification Rate")

