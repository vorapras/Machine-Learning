############# generate random numbers and add Gaussian white noice##########

x<-seq(-5.1,5.13,0.01)
y<-rep(0,length(x))
for(i in 1:length(x)){
  y[i]=sin(x[i])+2*cos(2*x[i]+20)
}
wavedata<-data.frame(x,y)
plot(wavedata)

library(RMThreshold)
wavematrix<-data.matrix(wavedata, rownames.force = NA)
new<-add.Gaussian.noise(wavematrix, mean = 0, stddev = c(0.1,0.3,0.5,0.8), symm = FALSE)
newdata<-data.frame(new)
plot(newdata)
x<-sort(newdata$x, decreasing = FALSE)
y<-rep(0,length(x))
for (i in 1:length(x)){
  y[i]<-newdata$y[newdata$x==x[i]] 
}

######################## compare with hard and soft################

library(MASS)
library(wavethresh)
waveletmap<-wd(y,family="DaubLeAsymm", filter.number=8)  # initial wavelet transform, Daubechies symmlet
softthre <- threshold(waveletmap,type="soft",policy="universal") # soft thresholding, universal thresholding
hardthre <- threshold(waveletmap,type="hard",policy="universal") # hard thresholding, universal thresholding
inv1<-wr(softthre) # invert the transform
inv2<-wr(hardthre)

plot(x,y, col="dark gray")
lines(x,inv1, col="green4", lwd=1.5)
title("Soft Thresholding")
plot(x,y, col="dark gray")
lines(x,inv2, col="red", lwd=1.5) 
title("Hard Thresholding")

################ wavelet### smoothing spline###################

plot(x,y, col="dark gray")
title("Smooth Function (Simulated)")
lines(x,inv2, col="red", lwd=1.5) # with hard
lines(x,inv1, col="green4", lwd=1.5) # with soft

library(splines)
fit <-smooth.spline(x, y, cv=TRUE)
fit$df #Equivalent Degrees of Freedom (Df)
lines(fit, col="blue", lwd=1.5)
legend("bottomright", legend=c("Wavelet (hard)", "Wavelet (soft)", "Smoothing Spline"), col=c("red", "green4", "blue"), lty=1, lwd=1.5, cex=1, bty="n")
pred<-predict(fit)
yhat<-pred$y

##########calculate residual sum of squares(RSS), MSE, variance,... #################

#wavelet hard: 
HwaveError<-rep(0, length(x))
for (i in 1:length(x)){
  HwaveError[i]=(inv2[i]-y[i])^2
}
sumHwaveRSS<-sum(HwaveError)
summary(HwaveError)
var(inv2)

HwaveRes<-rep(0,length(x))
for (i in 1:length(x)){
  HwaveRes[i]=inv2[i]-y[i]
}
plot(inv2, HwaveRes)

HwaveR2<-rep(0,length(x))
for (i in 1:length(x)){
  HwaveR2<-1 - (sum((y[i]-inv2[i])^2)/sum((y[i]-mean(y))^2))
}
HwaveR2

#wavelet soft: 
SwaveError<-rep(0,length(x))
for (i in 1:length(x)){
  SwaveError[i]=(inv1[i]-y[i])^2
}
sumSwaveRSS<-sum(SwaveError)
summary(SwaveError)
var(inv1)

SwaveRes<-rep(0,length(x))
for (i in 1:length(x)){
  SwaveRes[i]=inv1[i]-y[i]
}
plot(inv1, SwaveRes)

SwaveR2<-rep(0,length(x))
for (i in 1:length(x)){
SwaveR2<-1 - (sum((y[i]-inv1[i])^2)/sum((y[i]-mean(y))^2))
}
SwaveR2

#smoothing spline: 
splineError<-rep(0,length(x))
for (i in 1:length(x)){
  splineError[i]=(yhat[i]-y[i])^2
}
sumSplineRSS<-sum(splineError)
summary(splineError)
var(yhat)

splineRes<-rep(0,length(x))
for (i in 1:length(x)){
  splineRes[i]=yhat[i]-y[i]
}
plot(yhat, splineRes)

splineR2<-rep(0,length(x))
for (i in 1:length(x)){
  splineR2<-1 - (sum((y[i]-yhat[i])^2)/sum((y[i]-mean(y))^2))
}
splineR2


