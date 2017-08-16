credit=read.csv("/Users/ZerWinner/Desktop/R/Machine Learning/credit1.csv",header = T)
train=1:25000
test=credit[25001:30000,]
library(boot)
library(class)
library(MASS)
library(ROCR)
library(leaps)
library(ISLR)
library(tree)
reg.glm=regsubsets(Y~.,data=credit,nvmax=23,method="forward")
summary(reg.glm)
glm1=glm(Y~X6,family = binomial,data=credit,subset=train)
glm2=glm(Y~X6+X12,family = binomial,data=credit,subset=train)
glm3=glm(Y~X6+X12+X7,family = binomial,data=credit,subset=train)
glm4=glm(Y~X6+X12+X7+X5,family = binomial,data=credit,subset=train)
glm5=glm(Y~X6+X12+X7+X5+X8,family = binomial,data=credit,subset=train)
glm6=glm(Y~X6+X12+X7+X5+X8+X18,family = binomial,data=credit,subset=train)
glm7=glm(Y~X6+X12+X7+X5+X8+X18+X4,family = binomial,data=credit,subset=train)
glm8=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1,family = binomial,data=credit,subset=train)
glm9=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3,family = binomial,data=credit,subset=train)
glm10=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2,family = binomial,data=credit,subset=train)
glm11=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10,family = binomial,data=credit,subset=train)
glm12=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19,family = binomial,data=credit,subset=train)
glm13=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19+X13,family = binomial,data=credit,subset=train)
glm14=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19+X13+X22,family = binomial,data=credit,subset=train)
glm15=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19+X13+X22+X21,family = binomial,data=credit,subset=train)
glm16=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19+X13+X22+X21+X17,family = binomial,data=credit,subset=train)
glm17=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19+X13+X22+X21+X17+X9,family = binomial,data=credit,subset=train)
glm18=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19+X13+X22+X21+X17+X9+X23,family = binomial,data=credit,subset=train)
glm19=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19+X13+X22+X21+X17+X9+X23+X15,family = binomial,data=credit,subset=train)
glm20=glm(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19+X13+X22+X21+X17+X9+X23+X15+X14,family = binomial,data=credit,subset=train)
cv.err1=cv.glm(credit[train,],glm1,K=10)
cv.err1$delta
cv.err2=cv.glm(credit[train,],glm2,K=10)
cv.err2$delta
cv.err3=cv.glm(credit[train,],glm3,K=10)
cv.err3$delta
cv.err4=cv.glm(credit[train,],glm4,K=10)
cv.err4$delta
cv.err5=cv.glm(credit[train,],glm5,K=10)
cv.err5$delta
cv.err6=cv.glm(credit[train,],glm6,K=10)
cv.err6$delta
cv.err7=cv.glm(credit[train,],glm7,K=10)
cv.err7$delta
cv.err8=cv.glm(credit[train,],glm8,K=10)
cv.err8$delta
cv.err9=cv.glm(credit[train,],glm9,K=10)
cv.err9$delta
cv.err10=cv.glm(credit[train,],glm10,K=10)
cv.err10$delta
cv.err11=cv.glm(credit[train,],glm11,K=10)
cv.err11$delta
cv.err12=cv.glm(credit[train,],glm12,K=10)
cv.err12$delta
cv.err13=cv.glm(credit[train,],glm13,K=10)
cv.err13$delta
cv.err14=cv.glm(credit[train,],glm14,K=10)
cv.err14$delta
cv.err15=cv.glm(credit[train,],glm15,K=10)
cv.err15$delta
cv.err16=cv.glm(credit[train,],glm16,K=10)
cv.err16$delta
cv.err17=cv.glm(credit[train,],glm17,K=10)
cv.err17$delta
cv.err18=cv.glm(credit[train,],glm18,K=10)
cv.err18$delta
cv.err19=cv.glm(credit[train,],glm19,K=10)
cv.err19$delta
cv.err20=cv.glm(credit[train,],glm20,K=10)
cv.err20$delta
cv.errr=c(cv.err1$delta[1],cv.err2$delta[1],cv.err3$delta[1],cv.err4$delta[1],cv.err5$delta[1],cv.err6$delta[1],cv.err7$delta[1],cv.err8$delta[1],cv.err9$delta[1],cv.err10$delta[1],cv.err11$delta[1],cv.err12$delta[1],cv.err13$delta[1],cv.err14$delta[1],cv.err15$delta[1],cv.err16$delta[1],cv.err17$delta[1],cv.err18$delta[1],cv.err19$delta[1],cv.err20$delta[1])
x=c(1:20)
plot(x,cv.errr,col="blue")
glm.p=predict(glm13,test,type="response")
pred=prediction(glm.p,test$Y)
rp=performance(pred,measure ="tpr",x.measure ="fpr")
plot(rp,col=1:10)
abline(a=0,b=1)
s.lg=performance(pred,measure ="auc")
s.lg
####lda
glm.lda=lda(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19+X13,family = binomial,data=credit,subset=train)
glm.lp=predict(glm.lda,test,type="response")
pred2=prediction(glm.lp$posterior[,2],test$Y)
rp2=performance(pred2,measure ="tpr",x.measure ="fpr")
plot(rp,col=1:10)
abline(a=0,b=1)
s.lg2=performance(pred2,measure ="auc")
s.lg2
####qda
glm.qda=qda(Y~X6+X12+X7+X5+X8+X18+X4+X1+X3+X2+X10+X19+X13,family = binomial,data=credit,subset=train)
glm.qp=predict(glm.qda,test,type="response") 
pred3=prediction(glm.qp$posterior[,2],test$Y)
rp3=performance(pred3,measure ="tpr",x.measure ="fpr")
plot(rp,col=1:10)
abline(a=0,b=1)
s.lg3=performance(pred3,measure ="auc")
s.lg3
###KNN
train.x=credit[train,-24]
View(train.x)
View(test)
test.x=credit[25001:30000,-24]
View(test.x)
status=credit[train,24]
knn.pred1=knn(train.x,test.x,status,k=1)
table(knn.pred1,test[,24])
mean(knn.pred1==test[,24])
knn.pred10=knn(train.x,test.x,status,k=10)
table(knn.pred10,test[,24])
mean(knn.pred10==test[,24])
knn.pred100=knn(train.x,test.x,status,k=100)
table(knn.pred100,test[,24])
mean(knn.pred100==test[,24])
###tree
ST=ifelse(credit[,24]==0,"no","yes")
Cr=credit[,-24]
Cr=data.frame(Cr,ST)
tree.credit=tree(ST~.-ST,Cr,subset=train)
Cr.test=Cr[-train,]
tree.pred=predict(tree.credit,Cr.test,type="class")
ST.test=ST[-train]
table(tree.pred,ST.test)
(3813+346)/5000
###prune tree
cv.credit=cv.tree(tree.credit,FUN = prune.misclass)
names(cv.credit)
cv.credit
par(mfrow=c(1,2))
plot(cv.credit$size,cv.credit$dev,type="b")
plot(cv.credit$k,cv.credit$dev,type="b")
prune.credit=prune.misclass(tree.credit,best=2)
plot(prune.credit)

text(prune.credit,pretty = 0)
tree.pred1=predict(prune.credit,Cr.test,type="class")
table(tree.pred1,ST.test)
###test various knot
prune.credit1=prune.misclass(tree.credit,best=4)
plot(prune.credit1)
text(prune.credit1,pretty = 0)
tree.pred2=predict(prune.credit1,Cr.test,type="class")
table(tree.pred2,ST.test)
##bagging and random forrest
library(randomForest)



