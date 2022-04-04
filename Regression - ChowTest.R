install.packages("tidyverse")
library(readxl)

## Question 2
Data2 <- read_excel("/Users/aemensultan/OneDrive - Queen's University/MMA 860/
                    Assignment 2/MMA_860_Assignment2_Data.xlsx",
                    sheet="Missing")

Model2 <- lm(Y ~ X1 + X2 + X3 + X4 + X5, Data2)

install.packages("mice")
library(mice)

Impute2 <- mice(Data2, m=5, maxit=5, meth="pmm", seed=1)
Reg2 <- with(Impute2,lm(Y ~ X1 + X2 + X3 + X4 + X5, Data2))
summary(Reg2)
summary(pool(Reg2))

## Question 3
Data3 <- read_excel("/Users/aemensultan/OneDrive - Queen's University/MMA 860/
                    Assignment 2/MMA_860_Assignment2_Data.xlsx",
                    sheet="Wine")

Data3_New <- dummyVars("~.",Data3)
Data3_Dummy <- data.frame(predict(Data3_New,Data3))
str(Data3_Dummy)

Model3_1 <- lm(Rating ~ 1, Data3_Dummy)
Model3_2 <- lm(Rating ~ Price + Alcohol + Residual_Sugar + Sulphates + pH + 
                 CountryCanada + CountryFrance + CountryItaly, Data3_Dummy)
Model_Step_Wise <- step(Model3_1, direction = 'forward', scope=formula(Model3_2))

## Question 4 Part A
Data4 <- read_excel("/Users/aemensultan/OneDrive - Queen's University/MMA 860/
                    Assignment 2/MMA_860_Assignment2_Data.xlsx",
                    sheet="Curry")
str(Data4)

##Data4$Country <- ifelse(Data4$Country == "US", "1","0")

install.packages("caret")
install.packages("car")
library(caret)
library(car)

Data4_New <- dummyVars("~.",Data4)
Data4_Dummy <- data.frame(predict(Data4_New,Data4))

Model4_1 <- lm(Sales~1.,Data4_Dummy)
Model4_2 <- lm(Sales ~ Ad_Budget + Price + Distance + CountryCA + CountryUS, Data4_Dummy)

Stepwise4 <- step(Model4_1, direction="forward", scope=formula(Model4_2))
Stepwise4

Model4_Final <- lm(Sales~Ad_Budget + CountryCA + Price + Distance, Data4_Dummy)
plot(Model4_Final)
summary(Model4_Final)

## Question 4 Part B
Data4_Dummy$US_Ad_Budget <- Data4_Dummy$Ad_Budget*Data4_Dummy$CountryUS
Model4_US <- lm(Sales ~ Ad_Budget + US_Ad_Budget, Data4_Dummy)
summary(Model4_US)

## Question 4 Part C
Data4_Dummy$US_Price <- Data4_Dummy$Price*Data4_Dummy$CountryUS
Data4_Dummy$US_Distance <- Data4_Dummy$Distance*Data4_Dummy$CountryUS

ChowTest <- lm(Sales ~ Ad_Budget + Distance + Price +
                 US_Ad_Budget + US_Distance + US_Price, Data4_Dummy)

ChowTestHyp <- linearHypothesis(ChowTest, c("CountryUS=0","US_Ad_Budget=0",
                                            "US_Distance=0","US_Price=0"))
ChowTestHyp
