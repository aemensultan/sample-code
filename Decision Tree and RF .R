####### Load Data 
Eureka <- read.csv("/Users/aemensultan/Desktop/eureka.csv", 
                   na.strings=c(""," ","NA"), header=TRUE, stringsAsFactors = TRUE)
Eureka <- Eureka[1:30000,]

####### Load Packages
# install.packages("tidyverse")
# install.packages("rpart")
# install.packages("caret")
# install.packages("mice")
# install.packages("leaps")
# install.packages("RGtk2")
# install.packages("groupdata2")


if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} 
pacman::p_load("caret","ROCR","lift","randomForest","glmnet","MASS","e1071")

####### Load Libraries
library(tidyverse)
library(readxl)
library(rpart)
library(caret)
library(mice)
library(MASS)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(partykit)
library(groupdata2)

###### Preliminary Data Inspection
# basic descriptive statistics 
summary(Eureka)
str(Eureka)

# factorization of categorical variables 
Eureka$sourceMedium <- sub(".*/","",Eureka$sourceMedium)
Eureka$sourceMedium<-as.factor(Eureka$sourceMedium)
Eureka$converted_in_7days	<- as.factor(Eureka$converted_in_7days	)
Eureka$country	<- as.factor(Eureka$country	)
Eureka$device	<- as.factor(Eureka$device	)
Eureka$fired_DemoReqPg_CallClicks_evt	<- as.factor(Eureka$fired_DemoReqPg_CallClicks_evt	)
Eureka$fired_help_me_buy_evt	<- as.factor(Eureka$fired_help_me_buy_evt	)
Eureka$fired_phone_clicks_evt	<- as.factor(Eureka$fired_phone_clicks_evt	)
Eureka$newUser	<- as.factor(Eureka$newUser	)
Eureka$paid	<- as.factor(Eureka$paid	)
Eureka$sourceMedium	<- as.factor(Eureka$sourceMedium	)
Eureka$visited_air_purifier_page	<- as.factor(Eureka$visited_air_purifier_page	)
Eureka$visited_checkout_page	<- as.factor(Eureka$visited_checkout_page	)
Eureka$visited_contactus	<- as.factor(Eureka$visited_contactus	)
Eureka$visited_customer_service_amc_login	<- as.factor(Eureka$visited_customer_service_amc_login	)
Eureka$visited_customer_service_request_login	<- as.factor(Eureka$visited_customer_service_request_login	)
Eureka$visited_demo_page	<- as.factor(Eureka$visited_demo_page	)
Eureka$visited_offer_page	<- as.factor(Eureka$visited_offer_page	)
Eureka$visited_security_solutions_page	<- as.factor(Eureka$visited_security_solutions_page	)
Eureka$visited_storelocator	<- as.factor(Eureka$visited_storelocator	)
Eureka$visited_successbookdemo	<- as.factor(Eureka$visited_successbookdemo	)
Eureka$visited_vacuum_cleaner_page	<- as.factor(Eureka$visited_vacuum_cleaner_page	)
Eureka$visited_water_purifier_page	<- as.factor(Eureka$visited_water_purifier_page	)

# impute missing values (all numerical)
# Create a custom function to fix missing values ("NAs") 
fixNAs<-function(data_frame){
  # Define reactions to NAs
  integer_reac<-0
  factor_reac<-"FIXED_NA"
  character_reac<-"FIXED_NA"
  date_reac<-as.Date("1900-01-01")
  # Loop through columns in the data frame and depending on which 
  # class the variable is, apply the defined reaction and create a surrogate
  
  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
          as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]),i]<-integer_reac
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
          
        } 
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }  
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-date_reac
            }
          }  
        }       
      }
  } 
  return(data_frame) 
}

Eureka<-fixNAs(Eureka) #Apply fixNAs function to the data to fix missing values

table(Eureka$converted_in_7days)# check for rare categories

# Create another a custom function to combine rare categories into "Other."+the name 
# of the original variable (e.g., Other.State)
# This function has two arguments: the name of the dataframe and the count of observation 
# in a category to define "rare"
combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) 
}

#Apply combinerarecategories function to the data and then split it into testing and training data.

Eureka<-combinerarecategories(Eureka,20) 
#combine categories with <10 values in STCdata into "Other"

###
### Decision Tree
###

# creating test data set 
set.seed(77850) 
inTrain <- createDataPartition(Eureka$converted_in_7days, p = .7, 
                               list = FALSE)
training <- Eureka[ inTrain,]
testing <- Eureka[ -inTrain,]

# downsample as we have imbalanced data
train_down <- downsample(training, cat_col = "converted_in_7days")
train_up <- upsample(training, cat_col = "converted_in_7days")

# # downsampling to handle imbalanced data
# ctrl <- trainControl(method = "repeatedcv", repeats = 5,
#                      classProbs = TRUE,
#                      summaryFunction = twoClassSummary,
#                      sampling = "down")
# training <- train(converted_in_7days ~ ., data = training,
#                                 method = "gbm",
#                                 preProcess = c("range"),
#                                 verbose = FALSE,
#                                 trControl = ctrl)

# # decision tree model
# Eureka_Tree <- rpart(converted_in_7days~., method = "anova", data = train_down)
# summary(Eureka_Tree)
# 
# # fine tuning parameters
# Eureka_Tune1 <- rpart(converted_in_7days~., method = "anova", data = train_down,
#                      control = list(minsplit = 10, maxdepth = 12, xval = 10))
# summary(Eureka_Tune1)

# pruning with different values for cp
CART_cp <- rpart.control(cp=0.005)
gc()
rpart_tree <- rpart(converted_in_7days~.,data=train_up, 
                    method = "class",control=CART_cp)
printcp(rpart_tree)
plot(as.party(rpart_tree), type = "extended",gp = gpar(fontsize = 7)) 

prunned_rpart_tree<-prune(rpart_tree, cp=0.00001)
plot(as.party(rpart_tree), type = "extended",gp = gpar(fontsize = 7)) 
printcp(prunned_rpart_tree)

prunned_rpart_tree1<-prune(rpart_tree, cp=0.001)
plot(as.party(prunned_rpart_tree1), type = "extended", gp = gpar(fontsize = 7))
printcp(prunned_rpart_tree)

ctree_tree<-ctree(converted_in_7days~.,data=train_up)
plot(ctree_tree,gp=gpar(fontsize=8))

# # change in xerror 
# par(mfrow=c(1,2))
# rsq.rpart(Eureka_Tree)

###
### Random Forest
###

# rf model
model_forest <- randomForest(converted_in_7days~ ., 
                             data=train_up, 
                             type="classification",
                             importance=TRUE,
                             ntree = 500,           # hyperparameter: number of trees in the forest
                             mtry = 10,             # hyperparameter: number of random columns to grow each tree
                             nodesize = 10,         # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 10,         # hyperparameter: maximum number of leafs of a tree
) 

# plotting model
plot(model_forest)  # plots error as a function of number of trees in the forest
varImpPlot(model_forest) # plots variable importance

# fine tuning parameters
model_forest1 <- randomForest(converted_in_7days~ ., data=training, 
                             type="classification",
                             importance=TRUE,
                             ntree = 10,           # hyperparameter: number of trees in the forest
                             mtry = 5,             # hyperparameter: number of random columns to grow each tree
                             nodesize = 5,         # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 5,         # hyperparameter: maximum number of leafs of a tree
) 
plot(model_forest1)  
varImpPlot(model_forest1)

# predict
forest_probabilities<-predict(model_forest,newdata=testing) 
confusionMatrix(forest_probabilities,testing$converted_in_7days, positive="1")

##########
####ROC Curve
forest_ROC_prediction <- prediction(as.numeric(forest_probabilities),as.numeric(testing$converted_in_7days))
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc")
forest_AUC <- as.numeric(AUC.tmp@y.values) 
forest_AUC 

#### Lift chart
plotLift(as.numeric(forest_probabilities),as.numeric(testing$converted_in_7days), 
         cumulative = TRUE, n.buckets = 10)

