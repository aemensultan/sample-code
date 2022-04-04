##Install required packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("magrittr")
install.packages("caret")
install.packages("tidyverse")

##Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(caret)
library(tidyverse)

##Import files in to R Studio
Rev_Train_DF <- read.csv("/Users/aemensultan/Desktop/train.csv", stringsAsFactors = F)
Rev_Test_DF <- read.csv("/Users/aemensultan/Desktop/test.csv", stringsAsFactors = F)

##Check structures for Test and Train
str(Rev_Test_DF)
str(Rev_Train_DF)
head(Rev_Train_DF)

##Adding SalePrice column in the Test dataset
Rev_Test_DF$SalePrice = NA
head(Rev_Test_DF)

##Binding Test and Train data frames for cleaning
Data_DF <- rbind(Rev_Train_DF, Rev_Test_DF)
str(Data_DF)

##Variable Selection
New_Data_DF <- select(Data_DF, -c(GarageYrBlt, MiscFeature, MiscVal, MoSold, YrSold, 
                                  SaleType, SaleCondition, OverallCond, OverallQual, 
                                  ExterQual, ExterCond, KitchenQual))

##Check structure of the new data frame
str(New_Data_DF)
glimpse(New_Data_DF)

##Creating factors for all categorical variables 
New_Data_DF1 <- New_Data_DF %>%
  mutate(MSZoning <- fct_recode(MSZoning, c="c(all)"),
         Street <- factor(Street),
         Alley <- factor(Alley),
         LotShape <- factor(LotShape),
         LandContour <- factor(LandContour),
         Utilities <- factor(Utilities),
         LotConfig <- factor(LotConfig),
         LandSlope <- factor(LandSlope),
         Neighborhood <- factor(Neighborhood),
         Condition1 <- factor(Condition1),
         Condition2 <- factor(Condition2),
         BldgType <- factor(BldgType),
         HouseStyle <- factor(HouseStyle),
         RoofStyle <- factor(RoofStyle),
         RoofMatl <- factor(RoofMatl),
         Exterior1st <- factor(Exterior1st),
         Exterior2nd <- factor(Exterior2nd),
         MasVnrType <- factor(MasVnrType),
         Foundation <- factor(Foundation),
         BsmtQual <- recode(BsmtQual, "Ex"=1,"Gd"=2,"TA"=3,"Fa"=4,"Po"=5,"NA"=0),
         BsmtCond <- recode(BsmtCond, "Ex"=1,"Gd"=2,"TA"=3,"Fa"=4,"Po"=5,"NA"=0),
         BsmtExposure <- recode(BsmtExposure, "Ex"=1,"Gd"=2,"TA"=3,"Fa"=4,"Po"=5,"NA"=0),
         BsmtFinType1 <- recode(BsmtFinType1, "GLQ"=1,"ALQ"=2,"BLQ"=3,"Rec"=4,"LwQ"=5,"Unf"=6,"NA"=0),
         BsmtFinType2 <- recode(BsmtFinType2, "GLQ"=1,"ALQ"=2,"BLQ"=3,"Rec"=4,"LwQ"=5,"Unf"=6,"NA"=0),
         Heating <- recode(Heating, "Ex"=1,"Gd"=2,"TA"=3,"Fa"=4,"Po"=5,"NA"=0),
         HeatingQC <- factor(HeatingQC),
         CentralAir <- factor(CentralAir),
         Electrical <- factor(Electrical),
         Functional <- factor(Functional),
         FireplaceQu <- factor(FireplaceQu),
         GarageType <- factor(GarageType),
         GarageFinish <- factor(GarageFinish),
         GarageQual <- recode(GarageQual, "Ex"=1,"Gd"=2,"TA"=3,"Fa"=4,"Po"=5,"NA"=0),
         GarageCond <- recode(GarageCond, "Ex"=1,"Gd"=2,"TA"=3,"Fa"=4,"Po"=5,"NA"=0),
         PavedDrive <- factor(PavedDrive),
         PoolQC <- recode(PoolQC, "Ex"=1,"Gd"=2,"TA"=3,"Fa"=4,"Po"=5,"NA"=0),
         Fence <- factor(Fence))

##Identify number of missing values
MV_Count <- sapply(New_Data_DF, function(New_Data_DF) sum(is.na(New_Data_DF)))
print (MV_Count)

##Removing values where NAs big portion of data-set
New_Data_DF_v1 <- select(New_Data_DF, -c(Alley, FireplaceQu, PoolQC, Fence))

##Taking the log of SalePrice
New_Data_DF_v1$LogSalePrice <- log(New_Data_DF_v1$SalePrice)

##Dropping variables with Factor Level < 2
names(New_Data_DF_v1) <- make.names(names(New_Data_DF_v1))

features <- setdiff(colnames(New_Data_DF_v1), c("Id", "LogSalePrice"))  
for (f in features) {
  if (any(is.na(New_Data_DF_v1[[f]])))
    if (is.character(New_Data_DF_v1[[f]])){
      New_Data_DF_v1[[f]][is.na(New_Data_DF_v1[[f]])] <- "Others"
    } else{
      New_Data_DF_v1[[f]][is.na(New_Data_DF_v1[[f]])] <- -999
    }
}

column_class <- lapply (New_Data_DF_v1, class)
column_class <- column_class[column_class != "factor"]
factor_levels <- lapply (New_Data_DF_v1, nlevels)
factor_levels <- factor_levels[factor_levels > 1]
New_Data_DF_v11 <- New_Data_DF_v1[,which(names(New_Data_DF_v1) %in% 
                                           c(names(factor_levels),names(column_class)))]
str(New_Data_DF_v11)

##Running initial regression model
Model_v1 <- lm(LogSalePrice ~ MSSubClass +	MSZoning +	LotFrontage +	LotArea	+ 
                 Street + Utilities + CentralAir + LotShape + LandContour +	LotConfig	+ 
                 LandSlope + Neighborhood	+ Condition1 +	Condition2 +	BldgType +
                 HouseStyle +	YearBuilt	+ YearRemodAdd +	RoofStyle +	RoofMatl +	
                 Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	Foundation +
                 BsmtQual	+ BsmtCond	+ BsmtExposure	+ BsmtFinType1	+ BsmtFinSF1 +
                 BsmtFinType2	+ BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating	+ HeatingQC	+
                 Electrical	+ X1stFlrSF +	X2ndFlrSF	+ LowQualFinSF	+ GrLivArea	+
                 BsmtFullBath	+ BsmtHalfBath +	FullBath + HalfBath +	BedroomAbvGr	+
                 KitchenAbvGr	+ TotRmsAbvGrd + 	Functional +	Fireplaces	+ GarageType +	
                 GarageFinish	+ GarageCars + GarageArea	+ GarageQual +	GarageCond	+ 
                 PavedDrive	+ WoodDeckSF +	OpenPorchSF	+ EnclosedPorch +	X3SsnPorch	+ 
                 ScreenPorch	+ PoolArea, New_Data_DF_v11)
str(New_Data_DF_v11)

summary(Model_v1)

##Checking for outliers in Data
Outliers1 <-  boxplot.stats(New_Data_DF_v11$LogSalePrice)$out
Outliers1
max(Outliers1)
min(Outliers1)

boxplot(New_Data_DF_v11$LogSalePrice, main="SalePrice", boxweb=0.1)
hist(New_Data_DF_v11$LogSalePrice)
hist(New_Data_DF_v11$SalePrice)

Outliers_Impute <- function(x, removeNA=TRUE){
  quantiles <- quantile(x, c(.05,.95),na.rm = removeNA)
  x[ x < quantiles[1]] <- mean(x,na.rm = removeNA)
  x[ x > quantiles[2]] <- median(x,na.rm = removeNA)
  x
}

Outliers_Data <- Outliers_Impute(New_Data_DF_v11$LogSalePrice)

par(mfrow = c(1,2))
boxplot(Outliers_Data,main="SalePrice", boxweb=0.1)

Outliers_Data_v1 <- cbind(Outliers_Data,New_Data_DF_v11)

##Imputation for missing values
install.packages("mice")
library(mice)

New_Data_DF_v3 <- mice(Outliers_Data_v1, m=5, maxit=5, method = 'cart', seed=500)
summary(New_Data_DF_v3)

New_Data_DF_v4 <- complete(New_Data_DF_v3)
str(New_Data_DF_v4)

##Running initial regression model
Model_v2 <- lm(LogSalePrice ~ MSSubClass +	MSZoning +	LotFrontage +	LotArea	+ 
                 Street +	LotShape + LandContour +	Utilities +	LotConfig	+ 
                 LandSlope + Neighborhood	+ Condition1 +	Condition2 +	BldgType +
                 HouseStyle +	YearBuilt	+ YearRemodAdd +	RoofStyle +	RoofMatl +	
                 Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	Foundation +
                 BsmtQual	+ BsmtCond	+ BsmtExposure	+ BsmtFinType1	+ BsmtFinSF1 +
                 BsmtFinType2	+ BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating	+ HeatingQC	+
                 CentralAir	+ Electrical	+ X1stFlrSF +	X2ndFlrSF	+ LowQualFinSF	+ GrLivArea	+
                 BsmtFullBath	+ BsmtHalfBath +	FullBath + HalfBath +	BedroomAbvGr	+
                 KitchenAbvGr	+ TotRmsAbvGrd + 	Functional +	Fireplaces	+ GarageType +	
                 GarageFinish	+ GarageCars + GarageArea	+ GarageQual +	GarageCond	+ 
                 PavedDrive	+ WoodDeckSF +	OpenPorchSF	+ EnclosedPorch +	X3SsnPorch	+ 
                 ScreenPorch	+ PoolArea, New_Data_DF_v4)

summary(Model_v2)

##Removing insignificant variables using stepwise
Model_v3 <- lm(LogSalePrice ~ 1, New_Data_DF_v4)

Model_Step_Wise <- step(Model_v3, direction = 'forward', scope=formula(Model_v2))

Model_Final_v1 <- lm(LogSalePrice ~ MSSubClass +	MSZoning +	LotFrontage +	LotArea	+ 
                       Street +	LotShape + LandContour +	Utilities +	LotConfig	+ 
                       LandSlope + Neighborhood	+ Condition1 +	Condition2 +	BldgType +
                       HouseStyle +	YearBuilt	+ YearRemodAdd +	RoofStyle +	RoofMatl +	
                       Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	Foundation +
                       BsmtQual	+ BsmtCond	+ BsmtExposure	+ BsmtFinType1	+ BsmtFinSF1 +
                       BsmtFinType2	+ BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating	+ HeatingQC	+
                       CentralAir	+ Electrical	+ X1stFlrSF +	X2ndFlrSF	+ LowQualFinSF	+ GrLivArea	+
                       BsmtFullBath	+ BsmtHalfBath +	FullBath + HalfBath +	BedroomAbvGr	+
                       KitchenAbvGr	+ TotRmsAbvGrd + 	Functional +	Fireplaces	+ GarageType +	
                       GarageFinish	+ GarageCars + GarageArea	+ GarageQual +	GarageCond	+ 
                       PavedDrive	+ WoodDeckSF +	OpenPorchSF	+ EnclosedPorch +	X3SsnPorch	+ 
                       ScreenPorch	+ PoolArea, New_Data_DF_v4)

summary(Model_Final_v1)

#Using Cross validation to predict the error
install.packages("caret")
library(caret)

#Number of Subset 
Train_Folds <- trainControl(method = "cv", number = 10)

##Cross Validation
Model_Train_v1 <- train(LogSalePrice ~ ., data = New_Data_DF_v4, method = "lm", trcontrol = Train_Folds)
print(Model_Train_v1)
Train_Folds_v1 <- trainControl(method = "repeatedcv", number = 10, repeats = 4)

Model_Train_v2 <- train(LogSalePrice ~ ., data = New_Data_DF_v4, method = "lm", trcontrol = Train_Folds_v1)
print(Model_Train_v2)

##Running prediction on test data
Data.Train <- subset(New_Data_DF_v4, New_Data_DF_v4$Id <= 1460)
Data.Test <- subset(New_Data_DF_v4, New_Data_DF_v4$Id > 1460)

Model_Final_v2 <- lm(LogSalePrice ~ GrLivArea + YearRemodAdd + LotArea + PavedDrive + 
                       X3SsnPorch + TotalBsmtSF + TotRmsAbvGrd + BedroomAbvGr + 
                       X1stFlrSF + Fireplaces, Data.Train)
summary(Model_Final_v2)

Predict_SalePrice <- exp(predict(Model_Final_v2, Data.Test))
write.csv(Predict_SalePrice, file = "SalePrice_Predictions.csv")

Percent_Errors <- abs((Rev_Test_DF$LogSalePrice-Predict_SalePrice)/Rev_Test_DF$LogSalePrice)*100
mean(Percent_Errors)

plot(Model_Final_v2)
plot(fitted(Model_Final_v2),
     residuals(Model_Final_v2))
