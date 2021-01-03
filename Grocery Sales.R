#ISM6317 Assignment 5 
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

setwd("C:/Users/Ajohnson/Downloads")
df <- read_excel("BigMartSales.xlsx", sheet = "Data")
names(df) <- tolower(names(df))
str(df)
summary(df)

#Replace inconsistently formatted values
df$item_fat_content[df$item_fat_content == "low fat"] <- "Low Fat"

colSums(is.na(df)) # Check for missing data, item_weight and outlet_size have missing values 

# The weight of the item should be irrelavent to sales so throw it out
df$item_weight <- NULL

#too many missing values in outlet_size so we'll throw it out
#it also seems to be closely correlated with outlet_type anyway
df$outlet_size <- NULL

#change appropriate vars to factors
factorcols <- c("item_fat_content","item_type","city_type","outlet_type")
df[factorcols] <- lapply(df[factorcols], factor)

# Relevel fat content so Regular is base
df$item_fat_content <- relevel(df$item_fat_content, "Regular") 

str(df)
summary(df)
#---------------------------------------
# Question 1 - Sales by Outlet Type
#---------------------------------------
#Histogram and Density
histogram(~item_sales, breaks = 20, main = "Item_Sales Histogram", data=df) #right skewed distribution shows most items low sale amounts              
densityplot(~item_sales, data=df)
histogram(~item_sales | outlet_type, data=df, main = "Item_Sales by Outlet_Type Hist")
densityplot(~item_sales | outlet_type, data=df)

#Box and Whisker
bwplot(item_sales ~ outlet_type, data=df, main = "Item_Sales by Outlet_Type Box & Whisker")

#XY Plot
xyplot(item_sales ~ outlet_type, data=df)

#OLS Model as Control
ols1 <- lm(item_sales ~ item_mrp + outlet_type, data = df)
ols2 <- lm(item_sales ~ item_mrp*outlet_type, data = df)
stargazer(ols1,ols2, type="text", single.row=TRUE)

#Fixed effects model (controls for between item and between outlet differences)
fe1 <- lm(item_sales ~ item_mrp + outlet_type + outlet_id, data=df)
fe2 <- lm(item_sales ~ item_mrp*outlet_type + outlet_id, data=df)

stargazer(fe1,fe2, type="text", single.row=TRUE)

#Random effects model (controls for within item and within outlet differences)
re1 <- lmer(item_sales ~ item_mrp + outlet_type + (1 | outlet_id), data = df, REML=FALSE)
# summary(re1)
# fixef(re1)
# ranef(re1)
# coef(re1)

re2 <- lmer(item_sales ~ item_mrp*outlet_type + (1 | outlet_id), data = df, REML=FALSE)

re3 <- lmer(item_sales ~ item_visibility + item_mrp + outlet_type + (1 | outlet_id), data = df, REML=FALSE)

stargazer(re1, re2, fe2, type="text", single.row=TRUE)

anova(re1, re2, fe2)

#---------------------------------------
# Question 2 - Sales by City Type
#---------------------------------------
#Histogram and Density
histogram(~item_sales | city_type + outlet_type, data=df)
densityplot(~item_sales | city_type, data=df)

#Box and Whisker
bwplot(item_sales ~  city_type | outlet_type, data=df)

#XY Plot
xyplot(item_sales ~ outlet_type | city_type, data=df)

#OLS Model as Control
olsC <- lm(item_sales ~ item_mrp + outlet_type + city_type, data = df)
summary(olsC)

#Fixed effects model (controls for individual-level differences)
feC1 <- lm(item_sales ~ item_mrp + outlet_type + outlet_id + city_type, data=df)
feC2 <- lm(item_sales ~ item_mrp + outlet_type*city_type + outlet_id , data=df)
#summary(feC1)

#Random effects model (controls for within-block differences)
#reC1 <- lmer(item_sales ~ item_mrp + outlet_type*city_type + (1 | outlet_id) + (1 | city_type), data=df, REML=FALSE)
reC1 <- lmer(item_sales ~ item_mrp + outlet_type + city_type + (1 | outlet_id), data=df, REML=FALSE)
#summary(reC1)
#ranef(reC1)
#coef(reC1)

reC2 <- lmer(item_sales ~ item_mrp + outlet_type*city_type + (1 | outlet_id) , data=df, REML=FALSE)
reC3 <- lmer(item_sales ~ item_mrp + outlet_type*city_type + (1 | outlet_id/city_type) , data=df, REML=FALSE)

stargazer(reC1, feC1, feC2, type="text", single.row=TRUE)

anova(reC1, feC1, feC2)
