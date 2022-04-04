##Install required packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("magrittr")
install.packages("caret")
install.packages("tidyverse")
install.packages("glmnet")

##Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(caret)
library(tidyverse)
library(glmnet)

##Import required files for Test & Train
Rev_Train_DF <- read.csv("/Users/aemensultan/Desktop/train.csv")
Rev_Test_DF <- read.csv("/Users/aemensultan/Desktop/test.csv")

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

##Creating factors for all categorical variables 
Data_DF <- Data_DF %>%
  mutate(MSSubClass = factor(MSSubClass), 
         MSZoning = fct_recode(MSZoning, c="c(all)"), 
         Alley = fct_explicit_na(Alley, "none"), 
         Utilities = factor(Utilities), 
         Condition2 = factor(Condition2), 
         HouseStyle = factor(HouseStyle), 
         RoofMatl = factor(RoofMatl), 
         RoofMatl = fct_recode(RoofMatl, Tar = "Tar&Grv"), 
         Exterior1st = factor(Exterior1st), 
         Exterior1st = fct_recode(Exterior1st, Wdsdng = "Wd Sdng"), 
         Exterior2nd = factor(Exterior2nd), 
         Exterior2nd = fct_recode(Exterior2nd, BrkCmn = "Brk Cmn", Wdsdng = 
                                    "Wd Sdng", WdShng = "Wd Shng"), 
         BsmtQual = fct_explicit_na(BsmtQual, "none"), 
         BsmtCond = fct_explicit_na(BsmtCond, "none"), 
         Heating = factor(Heating), 
         Electrical = factor(Electrical), 
         FireplaceQu = fct_explicit_na(FireplaceQu, "none"), 
         Fence = fct_explicit_na(Fence, "none"), 
         GarageType = fct_explicit_na(GarageType, "none"), 
         GarageFinish = fct_explicit_na(GarageFinish, "none"), 
         GarageQual = factor(GarageQual), 
         GarageCond = fct_explicit_na(GarageCond, "none"), 
         PoolQC = factor(PoolQC), 
         PoolQC = fct_explicit_na(PoolQC, "none"), 
         MiscFeature = fct_explicit_na(MiscFeature, "none"), 
         MoSold = factor(MoSold), 
         YrSold = factor(YrSold), 
         GarageYrBlt = ifelse (is.na(GarageYrBlt), YearBuilt, GarageYrBlt), 
         MasVnrArea = ifelse (is.na(MasVnrType) & is.na(MasVnrArea), 0, MasVnrArea))

##Imputation for missing values
# Categorical
Data_DF$MasVnrArea[(is.na(Data_DF$MasVnrType) & !is.na(Data_DF$MasVnrArea))] = "BrkFace"
Data_DF$SaleType[is.na(Data_DF$SaleType)] = "WD"
Data_DF$KitchenQual[is.na(Data_DF$KitchenQual)] = "TA"
Data_DF$Electrical[is.na(Data_DF$Electrical)] = "SBrkr"
Data_DF$Exterior1st[is.na(Data_DF$Exterior1st)] = "VinylSd"
Data_DF$Exterior2nd[is.na(Data_DF$Exterior2nd)] = "VinylSd"
Data_DF$Functional[is.na(Data_DF$Functional)] = "Typ"
Data_DF$Utilities[is.na(Data_DF$Utilities)] = "AllPub"
Data_DF$MSZoning[is.na(Data_DF$MSZoning)] = "RL"
Data_DF$GarageQual[is.na(Data_DF$GarageQual)] = "TA"
Data_DF$BsmtQual[is.na(Data_DF$BsmtQual)] = "TA"
Data_DF$BsmtCond[is.na(Data_DF$BsmtCond)] = "TA"
Data_DF$BsmtExposure[is.na(Data_DF$BsmtExposure)] = "No"
Data_DF$BsmtFinType1[is.na(Data_DF$BsmtFinType1)] = "Unf"
Data_DF$BsmtFinType2[is.na(Data_DF$BsmtFinType2)] = "Unf"
Data_DF$MasVnrType[is.na(Data_DF$MasVnrType)] = "None"
#Data_DF$Fence[is.na(Data_DF$Fence)] = "MnPrv"
#Data_DF$FireplaceQu[is.na(Data_DF$FireplaceQu)] = "TA"

# Numerical
Data_DF_Num = dplyr::select_if(Data_DF, is.numeric) %>% select(-SalePrice)
Imputed_Data = preProcess(Data_DF_Num, "medianImpute")
ImputedVal = predict(Imputed_Data, Data_DF_Num)

# Merging the Dataset
Data_DF_Cat = dplyr::select_if(Data_DF, negate(is.numeric))
Data_DF_v1 = bind_cols(Data_DF_Cat, ImputedVal, SalePrice = Data_DF$SalePrice)
dim(Data_DF_v1)

sum(is.na(Data_DF_v1))

##Splitting the Dataset back into Train & Test
Data.Train <- subset(Data_DF_v1, Data_DF_v1$Id <= 1460)
Data.Test <- subset(Data_DF_v1, Data_DF_v1$Id > 1460)
sum(is.na(Data.Train)) ## ensure no NAs in Train data
apply(Data.Train, 2, function(x) any(is.na(x)))

## Build model on Train data
Fitted_Data <- lm(SalePrice ~	+ MSZoning +	Street +	Alley +	LotShape + LandContour +
                    Utilities	+ LotConfig	+ LandSlope +	Neighborhood +	Condition1 +	Condition2	+
                    BldgType +	HouseStyle + RoofStyle	+ RoofMatl + Exterior1st	+ Exterior2nd +
                    MasVnrType  +	ExterQual +	ExterCond	+ Foundation +	BsmtQual +
                    BsmtCond +	BsmtExposure + BsmtFinType1 +	BsmtFinType2	+ Heating	+ HeatingQC	+
                    CentralAir	+ Electrical +	KitchenQual	+ Functional	+ FireplaceQu +	GarageType +	
                    GarageFinish +	GarageQual	+ GarageCond	+ PavedDrive +	PoolQC + 	Fence	+ 
                    MiscFeature	+ MoSold	+ YrSold + SaleType	+ SaleCondition + LotFrontage	+
                    LotArea	+ OverallQual	+ OverallCond +	YearBuilt	+ YearRemodAdd +	BsmtFinSF1 +
                    BsmtFinSF2 +	BsmtUnfSF	+ TotalBsmtSF +	X1stFlrSF +	X2ndFlrSF	+ LowQualFinSF +
                    GrLivArea	+ BsmtFullBath	+ BsmtHalfBath + FullBath +	HalfBath +	BedroomAbvGr +
                    KitchenAbvGr +	TotRmsAbvGrd +	Fireplaces +	GarageYrBlt +	GarageCars +	
                    GarageArea +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	X3SsnPorch	+
                    ScreenPorch +	PoolArea	+ MiscVal	+ Id, Data.Train)

# Predict prices for Test data
Predict.Data <- predict(Fitted_Data, Data.Test)
write.csv(Predict.Data, file="Predict 1.csv")

# Checking logarithmic model
Fitted.Log_Data <- lm(log(SalePrice) ~ MSZoning +	Street +	Alley +	LotShape + LandContour +
                        Utilities	+ LotConfig	+ LandSlope +	Neighborhood +	Condition1 +	Condition2	+
                        BldgType +	HouseStyle + RoofStyle	+ RoofMatl + Exterior1st	+ Exterior2nd +
                        MasVnrType +	ExterQual +	ExterCond	+ Foundation +	BsmtQual +
                        BsmtCond +	BsmtExposure + BsmtFinType1 +	BsmtFinType2	+ Heating	+ HeatingQC	+
                        CentralAir	+ Electrical +	KitchenQual	+ Functional	+ FireplaceQu +	GarageType +	
                        GarageFinish +	GarageQual	+ GarageCond	+ PavedDrive +	PoolQC + 	Fence	+ 
                        MiscFeature	+ MoSold	+ YrSold + SaleType	+ SaleCondition + LotFrontage	+
                        LotArea	+ OverallQual	+ OverallCond +	YearBuilt	+ YearRemodAdd +	BsmtFinSF1 +
                        BsmtFinSF2 +	BsmtUnfSF	+ TotalBsmtSF +	X1stFlrSF +	X2ndFlrSF	+ LowQualFinSF +
                        GrLivArea	+ BsmtFullBath	+ BsmtHalfBath + FullBath +	HalfBath +	BedroomAbvGr +
                        KitchenAbvGr +	TotRmsAbvGrd +	Fireplaces +	GarageYrBlt +	GarageCars +	
                        GarageArea +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	X3SsnPorch	+
                        ScreenPorch +	PoolArea	+ MiscVal	+ Id, Data.Train)

Predict.Log.Data <- exp(predict(Fitted.Log_Data,Data.Test))
write.csv(Predict.Log.Data, file="Predict 2.csv")

## Cross-fold Validation
Data.Testing <- subset(Data_DF_v1, (Id >= 1001 & Id <= 1460))
Data.Training <- subset(Data_DF_v1, (Id <= 1000))

Fitted_Data1 <- lm(SalePrice ~ MSZoning +	Street +	Alley +	LotShape + LandContour +
                     Utilities	+ LotConfig	+ LandSlope +	Neighborhood +	Condition1 +
                     BldgType +	HouseStyle +
                     MasVnrType  +	ExterQual +	ExterCond	+ Foundation +	BsmtQual +
                     BsmtCond +	BsmtExposure + BsmtFinType1 +	BsmtFinType2	+ HeatingQC	+
                     CentralAir	+ Electrical +	KitchenQual	+ Functional	+ FireplaceQu +	GarageType +	
                     GarageFinish +	GarageQual	+ GarageCond	+ PavedDrive + 	Fence	+ 
                 	   MoSold	+ YrSold + SaleType	+ SaleCondition + LotFrontage	+
                     LotArea	+ OverallQual	+ OverallCond +	YearBuilt	+ YearRemodAdd +	BsmtFinSF1 +
                     BsmtFinSF2 +	BsmtUnfSF	+ TotalBsmtSF +	X1stFlrSF +	X2ndFlrSF	+ LowQualFinSF +
                     GrLivArea	+ BsmtFullBath	+ BsmtHalfBath + FullBath +	HalfBath +	BedroomAbvGr +
                     KitchenAbvGr +	TotRmsAbvGrd +	Fireplaces +	GarageYrBlt +	GarageCars +	
                     GarageArea +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	X3SsnPorch	+
                     ScreenPorch +	PoolArea	+ MiscVal	+ Id, Data.Training)

Predict.Data1 <- predict(Fitted_Data1, Data.Testing)

PE1 <- abs((Data.Testing$SalePrice-Predict.Data1)/Data.Testing$SalePrice)*100
mean(PE1)

# Logarithmic model
Fitted_Data2 <- lm(log(SalePrice) ~ MSZoning +	Street +	Alley +	LotShape + LandContour +
                     Utilities	+ LotConfig	+ LandSlope +	Neighborhood +	Condition1 +
                     BldgType +	HouseStyle +
                     MasVnrType  +	ExterQual +	ExterCond	+ Foundation +	BsmtQual +
                     BsmtCond +	BsmtExposure + BsmtFinType1 +	BsmtFinType2	+ HeatingQC	+
                     CentralAir	+ Electrical +	KitchenQual	+ Functional	+ FireplaceQu +	GarageType +	
                     GarageFinish +	GarageQual	+ GarageCond	+ PavedDrive + 	Fence	+ 
                     MoSold	+ YrSold + SaleType	+ SaleCondition + LotFrontage	+
                     LotArea	+ OverallQual	+ OverallCond +	YearBuilt	+ YearRemodAdd +	BsmtFinSF1 +
                     BsmtFinSF2 +	BsmtUnfSF	+ TotalBsmtSF +	X1stFlrSF +	X2ndFlrSF	+ LowQualFinSF +
                     GrLivArea	+ BsmtFullBath	+ BsmtHalfBath + FullBath +	HalfBath +	BedroomAbvGr +
                     KitchenAbvGr +	TotRmsAbvGrd +	Fireplaces +	GarageYrBlt +	GarageCars +	
                     GarageArea +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	X3SsnPorch	+
                     ScreenPorch +	PoolArea	+ MiscVal	+ Id, Data.Training)

Predict.Data2 <- predict(Fitted_Data2, Data.Testing)

PE2 <- abs((Data.Testing$SalePrice-Predict.Data2)/Data.Testing$SalePrice)*100
mean(PE2)
##Non-logarithmic model is better

## Add outliers stuff + feature enginerring + etc.

## Variable Selection using Stepwise
Fit.Log.Stepwise <- step(lm(SalePrice ~ MSZoning +	Street +	Alley +	LotShape + LandContour +
                           Utilities	+ LotConfig	+ LandSlope +	Neighborhood +	Condition1 +
                           BldgType +	HouseStyle +
                           MasVnrType  +	ExterQual +	ExterCond	+ Foundation +	BsmtQual +
                           BsmtCond +	BsmtExposure + BsmtFinType1 +	BsmtFinType2	+ HeatingQC	+
                           CentralAir	+ Electrical +	KitchenQual	+ Functional	+ FireplaceQu +	GarageType +	
                           GarageFinish +	GarageQual	+ GarageCond	+ PavedDrive + 	Fence	+ 
                           MoSold	+ YrSold + SaleType	+ SaleCondition + LotFrontage	+
                           LotArea	+ OverallQual	+ OverallCond +	YearBuilt	+ YearRemodAdd +	BsmtFinSF1 +
                           BsmtFinSF2 +	BsmtUnfSF	+ TotalBsmtSF +	X1stFlrSF +	X2ndFlrSF	+ LowQualFinSF +
                           GrLivArea	+ BsmtFullBath	+ BsmtHalfBath + FullBath +	HalfBath +	BedroomAbvGr +
                           KitchenAbvGr +	TotRmsAbvGrd +	Fireplaces +	GarageYrBlt +	GarageCars +	
                           GarageArea +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	X3SsnPorch	+
                           ScreenPorch +	PoolArea	+ MiscVal	+ Id, Data.Training), direction = "both")

summary(Fit.Log.Stepwise)

## Predict prices for Test data using new model
Predict.Log.Data <- exp(predict(Fit.Log.Stepwise,Data.Testing))

Percent.Error.Log.Stepwise <- abs((Data.Testing$SalePrice-Predict.Log.Data)/Data.Testing$SalePrice)*100
mean(Percent.Error.Log.Stepwise)

##Lasso data-set
Lasso.Train <- Data_DF_v1[1:1100,]
Lasso.Test <- Data_DF_v1[1101:1460,]

##Creating y variable and x matrix for Lasso & Ridge
y <- log(Lasso.Train$SalePrice) ## use lasso train set
x <- model.matrix(Id ~ MSSubClass + MSZoning +	Street +	Alley +	LotShape + LandContour +
                    Utilities	+ LotConfig	+ LandSlope +	Neighborhood +	Condition1 +	Condition2	+
                    BldgType +	HouseStyle + RoofStyle	+ RoofMatl + Exterior1st	+ Exterior2nd +
                    MasVnrType +	ExterQual +	ExterCond	+ Foundation +	BsmtQual +
                    BsmtCond +	BsmtExposure + BsmtFinType1 +	BsmtFinType2	+ Heating	+ HeatingQC	+
                    CentralAir	+ Electrical +	KitchenQual	+ Functional	+ FireplaceQu +	GarageType +	
                    GarageFinish +	GarageQual	+ GarageCond	+ PavedDrive +	PoolQC + 	Fence	+ 
                    MiscFeature	+ MoSold	+ YrSold + SaleType	+ SaleCondition + LotFrontage	+
                    LotArea	+ OverallQual	+ OverallCond +	YearBuilt	+ YearRemodAdd +	BsmtFinSF1 +
                    BsmtFinSF2 +	BsmtUnfSF	+ TotalBsmtSF +	X1stFlrSF +	X2ndFlrSF	+ LowQualFinSF +
                    GrLivArea	+ BsmtFullBath	+ BsmtHalfBath + FullBath +	HalfBath +	BedroomAbvGr +
                    KitchenAbvGr +	TotRmsAbvGrd +	Fireplaces +	GarageYrBlt +	GarageCars +	
                    GarageArea +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	X3SsnPorch	+
                    ScreenPorch +	PoolArea	+ MiscVal	+ SalePrice, Data_DF_v1)[,-1]## add all variables with original dataset
rm(x,y)
x$Id = NULL
x <- cbind(Data_DF_v1$Id,x) ## Id bind

##Splitting into Train, Test and Prediction 
x.train<-subset(x,x[,1]<= 1100)
x.test<- subset(x,(x[,1] >= 1101 & x[,1] <= 1460))
x.predict<-subset(x,x[,1] >= 1461)

##LASSO with alpha = 1
Data.Lasso <- glmnet(x=x.train, y=y, alpha=1) 
plot(Data.Lasso, xvar="lambda")

##Penalty lamdba selection
Data.CV <- cv.glmnet(x=x.train, y=y, alpha=1)
plot(Data.CV)
Lambda.Lasso <- Data.CV$lambda.min
log(Lambda.Lasso)
plot(Data.CV, xlim=c(-8.5,-6),ylim=c(0.006,0.008))
Data.Lasso.Fit <- glmnet(x=x.train,y=y, alpha=1, lambda=Lambda.Lasso)
coef(Data.Lasso.Fit)

##Predict on Test dataset
Lasso.Test <- exp(predict(Data.Lasso.Fit, s=Lambda.Lasso, newx = x.test))
mean(abs(Lasso.Test-Data.Testing$SalePrice)/Data.Testing$SalePrice*100)

## Ridge with alpha=0
Ridge.Test <- glmnet(x=x.train,y=y,alpha=0)
plot(Ridge.Test, xvar = "lambda")

# Penalty lamdba selection
Data.CV1 <- cv.glmnet(x=x.train, y=y, alpha=0)
plot(Data.CV1)
Lambda.Ridge <- Data.CV1$lambda.min
log(Lambda.Ridge)
Data.Ridge.Fit <- glmnet(x=x.train, y=y, alpha=0, lambda=Lambda.Ridge)
coef(Data.Ridge.Fit)

##Predict on Test dataset
Ridge.Test <- exp(predict(Data.Ridge.Fit, s=Lambda.Ridge, newx = x.test))
mean(abs(Ridge.Test-Data.Testing$SalePrice)/Data.Testing$SalePrice*100)

##Using Lasso model as it gives better MAPE
Predict.Prices.Lasso <- exp(predict(Data.Lasso.Fit, s=Lambda.Lasso, newx = x.predict))
write.csv(Predict.Prices.Lasso, file="Final_V1.csv")
