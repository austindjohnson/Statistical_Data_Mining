#ISM6317 Assignment 3 
#Austin Johnson
library(tidyverse)
library(readxl)
library(ggplot2)
library(stargazer)
library(AER)
library(MASS)
library(car)
library(lmtest)

setwd("C:/Users/Ajohnson/Downloads")
df <- read_excel("CollegeAdmissions_USnews1995.xlsx", sheet = "Sheet1")
names(df) <- tolower(names(df))
str(df)
df$Private <- as.factor(df$private) #convert Private to factor

#------------------------------------------------------------------------------------------------
#Modeling Enrollment
#------------------------------------------------------------------------------------------------

#Linear Models
hist(df$enrollment, main ="Histogram of Enrollment", col="red",breaks=15) #looks Poisson
hist(log(df$enrollment), main ="Histogram of log(Enrollment)", col="cyan",breaks=15)  #looks more normally distributed

#Modeling log(enrollment) with all variables except college
em1 = lm(log(df$enrollment)~.-college, data=df)
summary(em1) #shows 9 variables with p<.05

#Generalized Linear Model (Maximum Likelihood Estimation)
emle1 = glm(log(df$enrollment)~.-college, data=df, family=gaussian) 
summary(emle1) 

stargazer(em1,emle1, type="text", no.space=TRUE, title="Linear Models of log(Enrollment)") #two models are identical

#Poisson Model
epoisson1 = glm(df$enrollment~.-college, data=df, family=poisson) 
summary(epoisson1) #every variable is significant, very high AIC

stargazer(em1,emle1,epoisson1, type="text", title ="Linear and Poisson Comparison") 

#Check for Overdispersion
dispersiontest(epoisson1) #p<.05 suggests overdispersion
#Dispersion is far from 1 so we should try a negative binomial regression

#Negative Binomial Regression
enb <- glm.nb(df$enrollment~.-college, data=df)
summary(enb)
confint(enb) #confidence intervals
exp(confint(enb)) #exponential function
exp(cbind(coef=coef(enb), confint(enb))) #Effect size

stargazer(em1,emle1,epoisson1,enb, type="text", no.space=TRUE, title ="Enrollment Linear/Poisson/Neg Binom Comparison")

#The Generalized Linear Model (emle) appears to have the lowest AIC by a significant margin
#Try to improve the GLM by adjusting the variables used
emle2 = glm(log(df$enrollment)~private+acceptance+top10perc+fulltimeug+parttimeug+outstatetuition+
              books+personalexpense+phd+studentfacultyratio+alumnidonate+instrexpend+gradrate
            , data=df, family=gaussian) 
summary(emle2)

stargazer(emle1,emle2,type="text", no.space=TRUE,title="GLM Model Comparison") #the AIC is slightly lower with different

#Check for LM Assumptions
# Residual plot
plot(emle2$res ~ emle2$fit, pch=20, main = "Residuals vs. Fit")                    
abline(c(0,0),col ="red")

# Q-Q plot
qqnorm(emle2$res, pch=20)                             
qqline(emle2$res, col="red")

# Shapiro-Wilk's test of multivariate normality
shapiro.test(emle2$res)

# Kolmogorov-Smirnov test
norm <- rnorm(777) #create normal dist of same nubmer of observations
ks.test(norm, emle2$res)                       

# Homoskedasticity - Bartlett 
bartlett.test(list(emle2$res, emle2$fit))   

# Homoskedasticity - Levene
leveneTest(emle2$res, emle2$fit, center=mean)   

# Multicollinearity - Variance Inflation Factor
vif(emle2)                                     

# Autocorrelation - Durbin Watson
dwtest(emle2)

#------------------------------------------------------------------------------------------------
#Modeling GradRate
#------------------------------------------------------------------------------------------------

#Linear Models
hist(df$gradrate, main ="Histogram of gradrate", breaks=15, col="green") #looks fairly normal

#Modeling log(gradrate) with all variables except college
gm1 = lm(df$gradrate~.-college, data=df)
summary(gm1) #shows 9 variables with p<.05

#New model removing least significant variables
gm2 = lm(df$gradrate~private + appsrec + top25perc + parttimeug + outstatetuition + 
           roomandboard + personalexpense + alumnidonate + instrexpend, data=df)
summary(gm2)

stargazer(gm1, gm2, type="text", no.space=TRUE, title ="GradRate Linear Model Comparison")

#Check for LM Assumptions
# Residual plot
plot(gm2$res ~ gm2$fit, pch=20, main="Residuals vs Fit")  
abline(c(0,0),col ="red")
hist(gm2$fit, col="orange")

# Q-Q plot
qqnorm(gm2$res, pch=20)                             
qqline(gm2$res, col="red")

# Shapiro-Wilk's test of multivariate normality
shapiro.test(gm2$res)

# Kolmogorov-Smirnov test
norm <- rnorm(777) #create normal dist of same number of observations
ks.test(norm, gm2$res)                       

# Homoskedasticity - Bartlett 
bartlett.test(list(gm2$res, gm2$fit))   

# Homoskedasticity - Levene
leveneTest(gm2$res, gm2$fit, center=mean)   

# Multicollinearity - Variance Inflation Factor
vif(gm2)                                     

# Autocorrelation - Durbin Watson
dwtest(gm2)

