rm(list=ls())
library(ggplot2)
library("car")
library(lmtest)
setwd("C:/Users/ajohnson/Downloads")
medex = read.csv("medicalexpenses.csv")
attach(medex)
names(medex) <- tolower(names(medex))

#Question 1 - Histogram
hist(MedExpense, breaks=20, main="Histogram of MedExpense")
hist(log(MedExpense), breaks=20)

#Question 3 - Linear Regression Test
#change relevant variables into factors
columns <- c("publicins","privateins","female","hisp","black","married","urban","priority")
medex[columns]<- lapply(medex[columns],as.factor)
str(medex)
m1 = lm(log(MedExpense) ~ age + illnesses + hisp + black + married  + priority, data=medex)
summary(m1)
plot(m1)

#Question 4 - Test Assumptions

# Linearity
plot(MedExpense ~m1$fit)
abline(0,1,lwd=3,col="red")

# Residual plot
plot(m1$res ~ m1$fit, main = "Plot of m1 Residuals against Fit", pch =20)  
abline(c(0,0),col ="red")
hist(m1$fit, breaks=20, main = "Histogram of m1 Fit")

# Q-Q plot
qqnorm(m1$res, main = "m1 Normal Q-Q Plot")                            
qqline(m1$res, col="red")

# Shapiro-Wilk's test of multivariate normality
resSample <- sample(m1$res, 2000, replace = FALSE)
shapiro.test(resSample)

# Kolmogorov-Smirnov test
norm <- rnorm(10000)
ks.test(norm, m1$res)                       

# Bartlett's test of homoskedasticity
bartlett.test(list(m1$res, m1$fit))       

# Levene's test of homoskedasticity
leveneTest(m1$res, m1$fit, center=mean)   

# Test of multicollinearity - Variance inflation factor
vif(m1)                                    

# Durbin-Watson test of autocorrelation
dwtest(m1)                                  

#Question 5
library(MASS)
library(leaps)
#Stepwise regression
#Create a full "kitchen-sink" mode
mFull <- lm(log(MedExpense) ~.-medexpense, data=medex)
stepwise <- stepAIC(mFull, direction = "both", trace = FALSE)
summary(stepwise)

#Question 6
#Create new "hasInsurance" variable by combining publicins and privateins using ifelse
medex$hasInsurance <- ifelse(medex$publicins==1 | medex$privateins==1,1,0)
#Change to factor
medex$hasInsurance <- as.factor(medex$hasInsurance)
#Shows that hasInsurance is now a factor with 2 levels, no insurance and hasinsurance
str(medex)

m6A = lm(MedExpense ~ hasInsurance, data=medex)
summary(m6A)
plot(m6A)

m6B = lm(MedExpense ~ illnesses, data=medex)
summary(m6B)
plot(m6B)

m6C = lm(MedExpense ~ female, data=medex)
summary(m6C)
plot(m6C)

m6D = lm(MedExpense ~ age, data=medex)
summary(m6D)
plot(m6D)

#Create new "hasInsurance" variable by combining publicins and privateins using ifelse
medex$isMinority <- ifelse(medex$hisp==1 | medex$black==1,1,0)
#Change to factor
medex$isMinority <- as.factor(medex$isMinority)
#Shows that hasInsurance is now a factor with 2 levels, no insurance and hasinsurance
str(medex)
m6E1 = lm(MedExpense ~ isMinority, data=medex)
summary(m6E1)
m6E2 = lm(MedExpense ~ hisp + black, data=medex)
summary(m6E2)
plot(m6E)

m6F = lm(MedExpense ~ publicins + privateins, data=medex)
summary(m6F)
plot(m6F)

m6G = lm(MedExpense ~ income, data=medex)
summary(m6G)
plot(m6G)
