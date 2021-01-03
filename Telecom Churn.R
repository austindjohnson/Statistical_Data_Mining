#ISM6317 Assignment 8 
#Austin Johnson
library(tidyverse)
library(readxl)
library(ggplot2)
library(stargazer)
library(AER)
library(MASS)
library(car)
library(lmtest)
library(lattice)
library(corrplot)
library(PerformanceAnalytics)
library(lme4)
library(dplyr)
library(caTools)
library(DescTools)
library(ROCR)

setwd("C:/Users/Ajohnson/Downloads")
df <- read_excel("TelcoChurn.xlsx", sheet = "Data")
names(df) <- tolower(names(df))
#names(df)<-make.names(names(df),unique = TRUE)
str(df)
summary(df)

#change seniorcitizen var from 0/1 to No/Yes to match rest of binary vars
df <- df %>%
  mutate(seniorcitizen = ifelse(seniorcitizen == "0", "No" , "Yes"))

#change appropriate vars to factors
factorcols <- c("gender","seniorcitizen","partner","dependents","phoneservice","multiplelines",
                "internetservice","onlinesecurity","onlinebackup","deviceprotection","techsupport",
                "streamingtv","streamingmovies","contract","paperlessbilling","paymentmethod","churn")

df[factorcols] <- lapply(df[factorcols], factor)

str(df)
summary(df)

# Check for missing data, 11 rows have NA for totalcharges
colSums(is.na(df)) 
# Keep only complete cases (11 rows out of 7043 isn't significant)
df <- df[complete.cases(df), ] 

## test model to check factor levels
#testmodel = glm(churn ~ . -customerid , family=binomial (link = "logit"), data=df)
#summary(testmodel)

#test correlation of tenure, monthlycharges, totalcharges
corsub <- df[c("tenure","monthlycharges","totalcharges")]
corrdf <- cor(corsub[1:3])
corrplot(corrdf, method ="pie")
#shows that tenure and totalcharges are highly correlated, so we should pick one as a predictor

#Partition datasets by service
dfTel <- subset(df, internetservice == 'No')
dfInt <- subset(df, phoneservice == 'No')
dfBoth  <- subset(df, phoneservice == 'Yes' & internetservice != 'No')

#----------------------------------------------------------------------------
# Question 3 
#----------------------------------------------------------------------------
# Create training and test data sets, 75:25 split, seed=1024
set.seed(1024)

#dfTel
sample = sample.split(dfTel, SplitRatio = .75)
dfTelTrain = subset(dfTel, sample == TRUE)
dfTelTest  = subset(dfTel, sample == FALSE)

#dfInt
sample = sample.split(dfInt, SplitRatio = .75)
dfIntTrain = subset(dfInt, sample == TRUE)
dfIntTest  = subset(dfInt, sample == FALSE)

#dfBoth
sample = sample.split(dfBoth, SplitRatio = .75)
dfBothTrain = subset(dfBoth, sample == TRUE)
dfBothTest  = subset(dfBoth, sample == FALSE)

#------------------
#Logit Models

TelLOGIT <- glm(churn ~ seniorcitizen + tenure + multiplelines + contract + paperlessbilling 
                  + paymentmethod + monthlycharges
                  , family=binomial (link = "logit"), data=dfTelTrain)
summary(TelLOGIT)

IntLOGIT <- glm(churn ~ seniorcitizen + tenure + onlinesecurity + techsupport + contract 
                  + paperlessbilling + paymentmethod + monthlycharges
                  , family=binomial (link = "logit"), data=dfIntTrain)
summary(IntLOGIT)

BothLOGIT <- glm(churn ~ seniorcitizen + tenure + contract 
                   + paperlessbilling + paymentmethod + monthlycharges
                   , family=binomial (link = "logit"), data=dfBothTrain)
summary(BothLOGIT)

stargazer(TelLOGIT, IntLOGIT, BothLOGIT, type="text", single.row=TRUE)

#----------------------------------------------------------------------------
# Question 4 
#----------------------------------------------------------------------------
#Telephone Odds and Probability
cbind( Coeff = coef(TelLOGIT) , 
       Odds = exp(coef(TelLOGIT)) , 
       Probability = exp(coef(TelLOGIT))/(1+exp(coef(TelLOGIT))) ,
       DistancefromEven = abs(0.5 - exp(coef(TelLOGIT))/(1+exp(coef(TelLOGIT))))
)
#Internet Odds and Probability
cbind( Coeff = coef(IntLOGIT) , 
       Odds = exp(coef(IntLOGIT)) , 
       Probability = exp(coef(IntLOGIT))/(1+exp(coef(IntLOGIT))) ,
       DistancefromEven = abs(0.5 - exp(coef(IntLOGIT))/(1+exp(coef(IntLOGIT))))
)
#Both Odds and Probability
cbind( Coeff = coef(BothLOGIT) ,
       Odds = exp(coef(BothLOGIT)) , 
       Probability = exp(coef(BothLOGIT))/(1+exp(coef(BothLOGIT))) ,
       DistancefromEven = abs(0.5 - exp(coef(BothLOGIT))/(1+exp(coef(BothLOGIT))))
)

#----------------------------------------------------------------------------
# Question 5 
#----------------------------------------------------------------------------
#AIC
AIC(TelLOGIT,IntLOGIT,BothLOGIT)

#PseudoR2
cbind(TelLOGIT=PseudoR2(TelLOGIT, "McFadden"),
      IntLOGIT=PseudoR2(IntLOGIT, "McFadden"),
      BothLOGIT=PseudoR2(BothLOGIT, "McFadden"))

#----------------------------------------------------------------------------
# Question 6 
#----------------------------------------------------------------------------
#Predicting Churn
#-------------------
#Telephone
predTel <- predict(TelLOGIT, newdata=dfTelTest, type="response")
predTel <- ifelse(predTel > 0.5, 1, 0)
ClassificationError <- mean(predTel != dfTelTest$churn)
# Confusion matrix
CMTel <- table(dfTelTest$churn, predTel)  
#Precision
precTel <- CMTel[2,2]/(CMTel[2,2]+CMTel[1,2])
#Recall
recTel <- CMTel[2,2]/(CMTel[2,2]+CMTel[2,1])
#F1-Score
F1Tel <- 2 * ((recTel * precTel) / (recTel + precTel))
#AUC
Telpr <- prediction(predTel, dfTelTest$churn)
TelAUC <- performance(Telpr, measure = "auc")
TelAUC <- TelAUC@y.values[[1]]

#-------------------
#Internet
predInt <- predict(IntLOGIT, newdata=dfIntTest, type="response")
predInt <- ifelse(predInt > 0.5, 1, 0)
# Confusion matrix
CMInt <- table(dfIntTest$churn, predInt) 
#Precision
precInt <- CMInt[2,2]/(CMInt[2,2]+CMInt[1,2])
#Recall
recInt <- CMInt[2,2]/(CMInt[2,2]+CMInt[2,1])
#F1-Score
F1Int <- 2 * ((recInt * precInt) / (recInt + precInt))
#AUC
Intpr <- prediction(predInt, dfIntTest$churn)
IntAUC <- performance(Intpr, measure = "auc")
IntAUC <- IntAUC@y.values[[1]]

#-------------------
#Both
predBoth <- predict(BothLOGIT, newdata=dfBothTest, type="response")
predBoth <- ifelse(predBoth > 0.5, 1, 0)
# Confusion matrix
CMBoth <- table(dfBothTest$churn, predBoth) 
#Precision
precBoth <- CMBoth[2,2]/(CMBoth[2,2]+CMBoth[1,2])
#Recall
recBoth <- CMBoth[2,2]/(CMBoth[2,2]+CMBoth[2,1])
#F1-Score
F1Both <- 2 * ((recBoth * precBoth) / (recBoth + precBoth))
#AUC
Bothpr <- prediction(predBoth, dfBothTest$churn)
BothAUC <- performance(Bothpr, measure = "auc")
BothAUC <- BothAUC@y.values[[1]]

#--Combine the above metrics

cbind(precInt,precBoth)

cbind(recInt,recBoth)

cbind(F1Int,F1Both)

cbind(TelAUC,IntAUC,BothAUC)
