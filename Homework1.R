# Homework1
# Clear current workspace
rm(list = ls()) 

#2 Simple and Multiple Linear Regression
auto=read.csv(file.choose(),header=T,na.strings="?")
View(auto)

# Part a) When Horsepower increase 1 units, Mpg will decrease -0.157 units
# with the strongest significant p-value
# To calculate the residual error relative to the response 
# we use the mean of the response and the RSE. The mean of mpg 
# is 23.4459184. The RSE of the lm.fit was 4.9057569 which indicates 
# a percentage error of 20.9237141%. We may also note that as the R2R2 is equal to 0.6059483,
# almost 60.5948258% of the variability in “mpg” can be explained using “horsepower”.
lm.fit=lm(Mpg~Horsepower,data=auto)
lm.fit
summary(lm.fit)

# Part b)
attach(auto)
predict(fit, data.frame(horsepower = 98), interval = "confidence")
predict(fit, data.frame(horsepower = 98), interval = "prediction")
plot(Horsepower,Mpg)
abline(lm.fit)

# Part c) The plot of residuals versus fitted values indicates the presence of non linearity in the data. 
# The plot of standardized residuals versus leverage indicates the presence of a few outliers (higher than 2 or lower than -2) 
# and a few high leverage points.
par(mfrow = c(2, 2))
plot(lm.fit)

# Part d)
pairs(auto)
# Part e)
library(corrplot)
R = cor(auto[,1:8])
corrplot(R, method="number")

# Part f) Multiple Linear Regression
names(auto)
mullm.fit=lm(Mpg~Cylinders+Displacement+Horsepower+Weight+Acceleration+Year+Origin,data=auto)
summary(mullm.fit)
# The significant variables are Displacement(+),Weight(-),Year(+) and Origin(+).
# All are statitiscally significant with almost 100 percent except Displacement with p-value 0.001
# The coefficient ot the “year” variable suggests that 
# the average effect of an increase of 1 year is an increase of 0.7507727 in “mpg” 
# (all other predictors remaining constant). In other words, cars become more fuel efficient 
# every year by almost 1 mpg / year.

# Part g)
plot(mullm.fit)
# There are some large outliners which stay outside the Cook's distance
# As before, the plot of residuals versus fitted values indicates the presence of mild non linearity in the data. 
# The plot of standardized residuals versus leverage indicates the presence of a few outliers (higher than 2 or lower than -2) 
# and one high leverage point (point 14).

# Part h)
anova1=aov(Mpg~Horsepower)
anova2=aov(Mpg~Cylinders+Displacement+Horsepower)
summary(anova1)
summary(anova2)

# Part i)
# From the correlation matrix, we obtained the two highest correlated pairs 
# and used them in picking interaction effects.
fitinter = lm(mpg ~ cylinders * displacement+displacement * weight, data = Auto[, 1:8])
summary(fitinter)
# From the p-values, we can see that the interaction between displacement and weight 
# is statistically signifcant, while the interactiion between cylinders and displacement is not.

# Part j)
par(mfrow = c(2, 2))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)
# We cannot see any difference and it seems that the log transformation gives the most linear looking plot.


#5
# The KNN classifier is typically used to solve classification problems 
# (those with a qualitative response) by identifying the neighborhood of 
# x0x0 and then estimating the conditional probability P(Y=j|X=x0)P(Y=j|X=x0) 
# for class jj as the fraction of points in the neighborhood whose response values 
# equal jj. The KNN regression method is used to solve regression problems 
# (those with a quantitative response) by again identifying the neighborhood of x0x0 
# and then estimating f(x0)f(x0) as the average of all the training responses in the neighborhood.

