#HOUSE PRICES: ADVANCED REGRESSION TECHNIQUES

#LOAD LIBRARIES
library(car)
library(caret)
library(tidyverse)
library(Amelia)
library(mice)
library(ggplot2)
library(lattice)
library(dplyr)

#IMPORT DATASETS
raw_train <- read.csv(file.choose(),stringsAsFactors = TRUE)
raw_test <- read.csv(file.choose(),stringsAsFactors = TRUE)

#EXPLORE DATASETS
str(raw_train)
summary(raw_train)
str(raw_test)
summary(raw_test)

#CHECK MISSING VALUES
sort(sapply(raw_train, function(x) { sum(is.na(x)) }), decreasing=TRUE)
sort(sapply(raw_test, function(x) { sum(is.na(x)) }), decreasing = TRUE)

#REPLACE 'NA' WITH 'NOT AVAILABLE', WHERE APPLICABLE AS PER DATA DESCRIPTION (TRAIN)
levels(raw_train$Alley) <- c(levels(raw_train$Alley), "Not Available")
raw_train$Alley[is.na(raw_train$Alley)] <- "Not Available"

levels(raw_train$BsmtQual) <- c(levels(raw_train$BsmtQual), "Not Available")
raw_train$BsmtQual[is.na(raw_train$BsmtQual)] <- "Not Available"

levels(raw_train$BsmtCond) <- c(levels(raw_train$BsmtCond), "Not Available")
raw_train$BsmtCond[is.na(raw_train$BsmtCond)] <- "Not Available"

levels(raw_train$BsmtExposure) <- c(levels(raw_train$BsmtExposure), "Not Available")
raw_train$BsmtExposure[is.na(raw_train$BsmtExposure)] <- "Not Available"

levels(raw_train$BsmtFinType1) <- c(levels(raw_train$BsmtFinType1), "Not Available")
raw_train$BsmtFinType1[is.na(raw_train$BsmtFinType1)] <- "Not Available"

levels(raw_train$BsmtFinType2) <- c(levels(raw_train$BsmtFinType2), "Not Available")
raw_train$BsmtFinType2[is.na(raw_train$BsmtFinType2)] <- "Not Available"

levels(raw_train$FireplaceQu) <- c(levels(raw_train$FireplaceQu), "Not Available")
raw_train$FireplaceQu[is.na(raw_train$FireplaceQu)] <- "Not Available"

levels(raw_train$GarageType) <- c(levels(raw_train$GarageType), "Not Available")
raw_train$GarageType[is.na(raw_train$GarageType)] <- "Not Available"

levels(raw_train$GarageFinish) <- c(levels(raw_train$GarageFinish), "Not Available")
raw_train$GarageFinish[is.na(raw_train$GarageFinish)] <- "Not Available"

levels(raw_train$GarageQual) <- c(levels(raw_train$GarageQual), "Not Available")
raw_train$GarageQual[is.na(raw_train$GarageQual)] <- "Not Available"

levels(raw_train$GarageCond) <- c(levels(raw_train$GarageCond), "Not Available")
raw_train$GarageCond[is.na(raw_train$GarageCond)] <- "Not Available"

levels(raw_train$PoolQC) <- c(levels(raw_train$PoolQC), "Not Available")
raw_train$PoolQC[is.na(raw_train$PoolQC)] <- "Not Available"

levels(raw_train$Fence) <- c(levels(raw_train$Fence), "Not Available")
raw_train$Fence[is.na(raw_train$Fence)] <- "Not Available"

levels(raw_train$MiscFeature) <- c(levels(raw_train$MiscFeature), "Not Available")
raw_train$MiscFeature[is.na(raw_train$MiscFeature)] <- "Not Available"

#SAME FOR TEST DATASET
levels(raw_test$Alley) <- c(levels(raw_test$Alley), "Not Available")
raw_test$Alley[is.na(raw_test$Alley)] <- "Not Available"

levels(raw_test$BsmtQual) <- c(levels(raw_test$BsmtQual), "Not Available")
raw_test$BsmtQual[is.na(raw_test$BsmtQual)] <- "Not Available"

levels(raw_test$BsmtCond) <- c(levels(raw_test$BsmtCond), "Not Available")
raw_test$BsmtCond[is.na(raw_test$BsmtCond)] <- "Not Available"

levels(raw_test$BsmtExposure) <- c(levels(raw_test$BsmtExposure), "Not Available")
raw_test$BsmtExposure[is.na(raw_test$BsmtExposure)] <- "Not Available"

levels(raw_test$BsmtFinType1) <- c(levels(raw_test$BsmtFinType1), "Not Available")
raw_test$BsmtFinType1[is.na(raw_test$BsmtFinType1)] <- "Not Available"

levels(raw_test$BsmtFinType2) <- c(levels(raw_test$BsmtFinType2), "Not Available")
raw_test$BsmtFinType2[is.na(raw_test$BsmtFinType2)] <- "Not Available"

levels(raw_test$FireplaceQu) <- c(levels(raw_test$FireplaceQu), "Not Available")
raw_test$FireplaceQu[is.na(raw_test$FireplaceQu)] <- "Not Available"

levels(raw_test$GarageType) <- c(levels(raw_test$GarageType), "Not Available")
raw_test$GarageType[is.na(raw_test$GarageType)] <- "Not Available"

levels(raw_test$GarageFinish) <- c(levels(raw_test$GarageFinish), "Not Available")
raw_test$GarageFinish[is.na(raw_test$GarageFinish)] <- "Not Available"

levels(raw_test$GarageQual) <- c(levels(raw_test$GarageQual), "Not Available")
raw_test$GarageQual[is.na(raw_test$GarageQual)] <- "Not Available"

levels(raw_test$GarageCond) <- c(levels(raw_test$GarageCond), "Not Available")
raw_test$GarageCond[is.na(raw_test$GarageCond)] <- "Not Available"

levels(raw_test$PoolQC) <- c(levels(raw_test$PoolQC), "Not Available")
raw_test$PoolQC[is.na(raw_test$PoolQC)] <- "Not Available"

levels(raw_test$Fence) <- c(levels(raw_test$Fence), "Not Available")
raw_test$Fence[is.na(raw_test$Fence)] <- "Not Available"

levels(raw_test$MiscFeature) <- c(levels(raw_test$MiscFeature), "Not Available")
raw_test$MiscFeature[is.na(raw_test$MiscFeature)] <- "Not Available"

#CHECK AGAIN FOR MISSING DATA
sort(sapply(raw_train, function(x) { sum(is.na(x)) }), decreasing=TRUE)
sort(sapply(raw_test, function(x) { sum(is.na(x)) }), decreasing = TRUE)

#IMPUTE MISSING VALUES WITH MICE (CART METHOD)
imp.raw_train <- mice(raw_train, m=1, method='cart', printFlag=FALSE)
train_complete <- complete(imp.raw_train)
sum(sapply(train_complete, function(x) { sum(is.na(x)) }))

imp.raw_test <- mice(raw_test, m=1, method='cart', printFlag=FALSE)
test_complete <- complete(imp.raw_test)
sum(sapply(test_complete, function(x) { sum(is.na(x)) }))

#CHECK AGAIN FOR MISSING DATA
sort(sapply(raw_train, function(x) { sum(is.na(x)) }), decreasing=TRUE)
sort(sapply(raw_test, function(x) { sum(is.na(x)) }), decreasing = TRUE)

#STEP WISE REGRESSION
model_all <- lm (SalePrice ~ 	MSSubClass	+MSZoning+	Utilities	+WoodDeckSF	+Exterior1st+	Exterior2nd+LowQualFinSF	+GrLivArea+	BsmtFullBath+	BsmtHalfBath	+MasVnrType	+BsmtFinType2+	BsmtFinSF2	+BsmtUnfSF+	TotalBsmtSF	+Heating	+HeatingQC	+CentralAir+	Electrical+	MasVnrArea	+ExterQual+	ExterCond	+Foundation+	BsmtQual+	BsmtCond	+BsmtExposure+	BsmtFinType1+	BsmtFinSF1+ScreenPorch	+YrSold+	SaleType+	OverallQual	+OverallCond+	YearBuilt+	YearRemodAdd+	RoofStyle	+RoofMatl+PoolArea+	PoolQC	+Fence	+MiscFeature+	MiscVal	+MoSold+OpenPorchSF+	EnclosedPorch+LotConfig+LandSlope	+FireplaceQu+	GarageType+	GarageYrBlt	+GarageFinish+	GarageCars+	GarageArea+	GarageQual	+GarageCond+	PavedDrive+FullBath+	HalfBath+	BedroomAbvGr+	KitchenAbvGr+	KitchenQual+	TotRmsAbvGrd+	Functional	+Fireplaces+Neighborhood+	Condition1+	Condition2+	BldgType	+HouseStyle+LotFrontage+	LotArea	+Street+	Alley+	LotShape	+LandContour+		+SaleCondition, train_complete)
model_0 <- lm (SalePrice~1.,data=train_complete)
model_predict <- step(model_0,direction = 'forward',scope =formula(model_all))


model_stepwise <- lm(SalePrice ~ OverallQual + GrLivArea + Neighborhood + BsmtQual + 
                       RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + KitchenQual + 
                       Condition2 + SaleCondition + OverallCond + YearBuilt + LotArea + 
                       PoolQC + ExterQual + GarageArea + TotalBsmtSF + BldgType + 
                       Functional + BedroomAbvGr + Condition1 + PoolArea + ScreenPorch + 
                       LowQualFinSF + LandContour + Street + LandSlope + KitchenAbvGr + 
                       GarageFinish + LotConfig + GarageCars + TotRmsAbvGrd + Exterior1st + 
                       MasVnrArea + MSZoning + Fireplaces + YearRemodAdd + GarageQual + 
                       GarageCond + WoodDeckSF + BsmtFullBath + MoSold + MasVnrType, train_complete)

#EVALUATING THE STEP-WISE MODEL
summary(model_stepwise)
par(mfrow=c(2,2)) 
plot(model_stepwise)
plot(density(resid(model_stepwise)))

#EXPERIMENTAL MODEL
model_exp <- lm(SalePrice ~ OverallQual + LotFrontage + CentralAir + HeatingQC + GrLivArea + Neighborhood + BsmtQual + 
                  RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + KitchenQual + 
                  Condition2 + SaleCondition + OverallCond + YearBuilt + LotArea + GarageArea +
                  ExterQual  + TotalBsmtSF + BldgType + MiscFeature + FullBath + HalfBath +
                  BedroomAbvGr + Condition1 + PoolArea + ScreenPorch + 
                  Street + LandSlope + KitchenAbvGr + 
                  LotConfig + GarageCars + Exterior1st + 
                  MasVnrArea + MSZoning + Fireplaces + YearRemodAdd  + 
                  WoodDeckSF + BsmtFullBath + MoSold + MasVnrType, train_complete)
summary(model_exp)

#LOG TRANSFORMATION
train_complete$lnSalePrice <- log(train_complete$SalePrice)
# train_complete$lnGrLivArea <- log(train_complete$GrLivArea)
# train_complete$lnLotArea <- log(train_complete$LotArea)
# 
# test_complete$lnGrLivArea <- log(test_complete$GrLivArea)
# test_complete$lnLotArea <- log(test_complete$LotArea)

#FINAL MODEL WITH LOG TRANSFORMATION
model_final <- lm(lnSalePrice ~ OverallQual + LotFrontage + CentralAir + HeatingQC + lnGrLivArea + Neighborhood + BsmtQual + 
                    RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + KitchenQual + 
                    Condition2 + SaleCondition + OverallCond + YearBuilt + lnLotArea + GarageArea +
                    ExterQual  + TotalBsmtSF + BldgType + MiscFeature + FullBath + HalfBath +
                    BedroomAbvGr + Condition1 + PoolArea + ScreenPorch + 
                    Street + LandSlope + KitchenAbvGr + 
                    LotConfig + GarageCars + Exterior1st + 
                    MasVnrArea + MSZoning + Fireplaces + YearRemodAdd  + 
                    WoodDeckSF + BsmtFullBath + MoSold + MasVnrType, train_complete)
summary(model_final)

#PREDICT SALEPRICE OF TEST DATASET WITH FINAL MODEL
test_complete$lnSalePrice <- predict(model_final,test_complete)
test_complete$SalePrice <- exp(test_complete$lnSalePrice)

#WRITE TO CSV FILE
write.csv(test_complete, file = "/Users/aemensultan/Desktop/Predictions.csv")
