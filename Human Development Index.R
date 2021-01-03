rm(list=ls())
library(tidyr)
library(rio)
library(arules)
library(arulesViz)
library(car)
library(dplyr)
library(lmtest)
library(RColorBrewer)
library(ggplot2)
library(lmtest)
library(stargazer)
library(corrplot)
library(ggeffects)
setwd("C:/Users/ajohnson/Downloads")
econ = read.csv("Economist2011.csv")
names(econ) <- tolower(names(econ))
str(econ)

#Question 2
plot(CPI,HDI, pch=20, main ="Correlation Between CPI and HDI", data = econ)
#Density 
den <- density(econ$hdi)                      
hist(HDI, breaks = 20, prob=T, main="HDI Histogram and Density", data = econ)
lines(den, col="red")

cor(CPI,HDI)

#Question 3
#Linear
mLinear <- lm(hdi ~ cpi, data = econ)
summary(mLinear)
plot(CPI,HDI, data=econ)
abline(mLinear, col="red")
plot(mLinear$res~mLinear$fit)
abline(0,0, col="red")

#Exponential
mExponential <- lm(hdi*hdi ~ cpi, data = econ)
summary(mExponential)
plot(CPI,HDI, data=econ)
abline(mExponential, col="red")
plot(mExponential$res~mExponential$fit)
abline(0,0, col="red")

#Logarithmic
mLogarithmic <- lm(log(hdi) ~ cpi, data = econ)
summary(mLogarithmic)
plot(CPI,log(HDI), data=econ)
abline(mLogarithmic, col="red")
plot(mLogarithmic$res~mLogarithmic$fit)
abline(0,0, col="red")

#Quadratic
hdi2 <- econ$hdi^2
mQuadratic <- lm(hdi+hdi2 ~ cpi, data = econ)
summary(mQuadratic)
plot(CPI,HDI+hdi2, data=econ)
abline(mQuadratic, col="red")
plot(mQuadratic$res~mQuadratic$fit)
abline(0,0, col="red")

stargazer(mLinear,mExponential,mLogarithmic,mQuadratic, type="text", title="HDI Model Comparisons", out="table.txt")

#Question 4- Marginal Effects
ggpredict(mExponential,"cpi")
pred <- ggpredict(mExponential,"cpi")
plot(pred)

#Question 5
# Shapiro-Wilk's test of multivariate normality
shapiro.test(mExponential$res)

# Kolmogorov-Smirnov test
norm <- rnorm(174)
ks.test(norm, mExponential$res)                       

# Bartlett's test of homoskedasticity
bartlett.test(list(mExponential$res, mExponential$fit))       

# Levene's test of homoskedasticity
leveneTest(mExponential$res, mExponential$fit, center=mean)   

# Test of multicollinearity - Variance inflation factor
vif(mExponential)                                    

# Durbin-Watson test of autocorrelation
dwtest(mExponential)           
