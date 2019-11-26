library(ggplot2)
library(MASS)
library(moments)
library(caret)
setwd("/Users/Divye/Downloads/Data Mining Kaggle Competition/")
getwd()

##LoadingData===================
train <- read.csv("train.csv")
test <- read.csv("test.csv")
train <- train[,-1] 
test <- test[,-1]
combines <- rbind(train[,-80], test)
#write.csv(combines, file="combined_data.csv")

#FactorVariable_Missing======================
#str(combines)

combines$MSSubClass <- as.factor(combines$MSSubClass)
combines$OverallCond <- as.factor(combines$OverallCond)
combines$OverallQual <- as.factor(combines$OverallQual)
combines$YearBuilt <- as.factor(combines$YearBuilt)
combines$YearRemodAdd <- as.factor(combines$YearRemodAdd)
combines$YrSold <- as.factor(combines$YrSold)
combines$MoSold <- as.factor(combines$MoSold)
combines$YrSold <- as.factor(combines$YrSold)
combines$TotRmsAbvGrd <- as.factor(combines$TotRmsAbvGrd)
combines$BedroomAbvGr <- as.factor(combines$BedroomAbvGr)
combines$BsmtFullBath <- as.factor(combines$BsmtFullBath)
combines$BsmtHalfBath <- as.factor(combines$BsmtHalfBath)
combines$Fireplaces <-  as.factor(combines$Fireplaces)
combines$FullBath <- as.factor(combines$FullBath)
combines$GarageCars <-  as.factor(combines$GarageCars)
combines$HalfBath <- as.factor(combines$HalfBath)
combines$KitchenAbvGr <- as.factor(combines$KitchenAbvGr)

NA_factor_list <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1",
                    "BsmtFinType2", "FireplaceQu", "GarageType", "GarageFinish",
                    "GarageQual", "PoolQC", "Fence", "MiscFeature", "Electrical"
                    , "GarageCond", "MasVnrType") 
#as defined by the data dictionary

new_cols <- as.data.frame(lapply(combines[NA_factor_list], function(x) {
  if(anyNA(x)) {
    levels(x) <- c(levels(x), "None")
    x[is.na(x)] <- "None"
    x}
  else x
}))
combines[,colnames(new_cols)] <- new_cols
rm(new_cols)

#dropping insignificant variables(8)============================
#combines$Alley <- NULL
#combines$PoolQC <- NULL
#combines$Fence <- NULL
#combines$MiscFeature <- NULL
#combines$LowQualFinSF <- NULL
#combines$MiscVal <- NULL
#combines$PoolArea <- NULL
#combines$Utilities <- NULL

#correcting_garage_variables(7)==============================
#cols <- c("GarageType","GarageYrBlt","GarageFinish","GarageCars"
#          ,"GarageArea","GarageQual","GarageCond")
combines$GarageYrBlt[combines$GarageQual=="None"] <- "None"
combines$GarageType[combines$GarageQual=="None"] <- "None"
combines$GarageYrBlt <- as.factor(combines$GarageYrBlt) #time variable
combines$GarageCars[combines$GarageYrBlt == "None"] <- 0
combines$GarageArea[combines$GarageYrBlt == "None"] <- 0

#correcting_basement_variables(11)============================
#cols <- c("BsmtHalfBath","BsmtFullBath","BsmtQual","BsmtCond","BsmtExposure"
#    ,"BsmtFinType1","BsmtFinSF1","BsmtFinType2","BsmtFinSF2","BsmtUnfSF", "TotalBsmtSF") 
#a <- combines[combines$BsmtCond=="None" | combines$BsmtQual=="None", cols]
combines$BsmtCond[combines$BsmtCond=="None" & !(combines$BsmtQual=="None")] = NA
combines$BsmtQual[combines$BsmtQual=="None" & !(combines$BsmtCond=="None")] = NA
combines$BsmtFinSF1[combines$BsmtCond == "None"] <- 0
combines$BsmtFinSF2[combines$BsmtCond == "None"] <- 0
combines$BsmtFullBath[combines$BsmtCond == "None"] <- 0
combines$BsmtHalfBath[combines$BsmtCond=="None"] <- 0
combines$BsmtUnfSF[combines$BsmtCond=="None"] <- 0
combines$TotalBsmtSF[combines$BsmtCond == "None"] <- 0

#correcting_materials_variables(6)======================
#cols <- c("Exterior1st", "Exterior2nd","MasVnrType","MasVnrArea","ExterQual","ExterCond")
combines$MasVnrArea[combines$MasVnrType=="None" & is.na(combines$MasVnrArea)==FALSE
                    & combines$MasVnrArea != 0 & combines$MasVnrArea < 10] <- 0
combines$MasVnrArea[combines$MasVnrType=="None" & is.na(combines$MasVnrArea)==TRUE] <- 0
combines$MasVnrType[combines$MasVnrType=="None" & combines$MasVnrArea > 0] <- NA


#OutlierTreatment_Numerical_Variables(9)===============
combines$LotFrontage[combines$LotFrontage > 250] <- NA
combines$GrLivArea[combines$GrLivArea > 4000] <- NA
combines$GarageArea[combines$GarageArea > 1200] <- NA
combines$X1stFlrSF[combines$X1stFlrSF > 4000] <- NA
combines$BsmtFinSF2[combines$BsmtFinSF2 > 5000] < -NA
combines$LotArea[combines$LotArea > 60000] <- NA
combines$MasVnrArea[combines$MasVnrArea > 1400] <- NA
combines$OpenPorchSF[combines$OpenPorchSF > 400] <- NA
combines$TotalBsmtSF[combines$TotalBsmtSF > 6000] <- NA

#MissingDataReport===============================
missing <- as.data.frame(colSums(is.na(combines)))
colnames(missing) <- "number_missing"
missing <- (subset(missing, missing$number_missing >0))

#Imputingmissingvalues==========================
library(mice)
#imputedValues <- mice(data=combines, m=1, maxit = 1,method="cart", seed=2016)
load("imputedValues.Rda")
densityplot(imputedValues, layout=c(3, 1))
combines <- complete(imputedValues,1)
#save(imputedValues, file="imputedValues.Rda")

#orderingcategorical_variables=====================================
combines$KitchenQual <- ordered(combines$KitchenQual, levels = c("None","Po","Fa","TA","Gd","Ex"))
combines$GarageFinish <- ordered(combines$GarageFinish, levels = c("None","Unf","RFn","Fin"))
combines$GarageQual <- ordered(combines$GarageQual, levels = c("None","Po","Fa","TA","Gd","Ex"))
combines$GarageCond <- ordered(combines$GarageCond, levels = c("None","Po","Fa","TA","Gd","Ex"))
combines$ExterQual <- ordered(combines$ExterQual, levels = c("Po","Fa","TA","Gd","Ex"))
combines$ExterCond <- ordered(combines$ExterCond, levels = c("Po","Fa","TA","Gd","Ex"))
combines$BsmtQual <- ordered(combines$BsmtQual, levels = c("None","Po","Fa","TA","Gd","Ex"))
combines$BsmtCond <- ordered(combines$BsmtCond, levels = c("None","Po","Fa","TA","Gd","Ex"))
combines$BsmtExposure <- ordered(combines$BsmtExposure, levels = c("None","No","Mn","Av","Gd"))
combines$BsmtFinType1 <- ordered(combines$BsmtFinType1, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))
combines$BsmtFinType2 <- ordered(combines$BsmtFinType2, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))
combines$FireplaceQu <- ordered(combines$FireplaceQu, levels = c("None","Po","Fa","TA","Gd","Ex"))
combines$Fence <- ordered(combines$Fence, levels = c("None","MnWw","MnPrv","GdWo","GdPrv"))
combines$PoolQC <- ordered(combines$PoolQC, levels = c("None","Fa","Gd","Ex"))
combines$HeatingQC <- ordered(combines$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
combines$Functional <- ordered(combines$Functional, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"))

#removing_skewness==================================================
combines$LotArea <- log(combines$LotArea)
combines$LotFrontage <- log(combines$LotFrontage)
combines$GrLivArea <- log(combines$GrLivArea)
combines$MiscVal <- log(combines$MiscVal + 1)

#CreatingDummyVariables=============================================
library(caret)
dummies <- dummyVars(~ ., data = combines, sep = "_")   # create dummyes for Xs
ex <- data.frame(predict(dummies, newdata = combines))  # actually creates the dummies
#rm(dummies, missing)   

# Remove Zero & Near Zero-Variance Predictors
library(caret)
nzv <- nearZeroVar(ex[,1:ncol(ex)], uniqueCut=10) # identify columns that are "near zero"
ex <- ex[,1:ncol(ex)][, -nzv]            # remove those columns from your dataset

#dividing back the data into train and test
train_x <- ex[1:1460,]
train_y <- train$SalePrice
test_x <- ex[1461:2919,]

#transforming_dependent_variable========================
new_train <- cbind(train_y, train_x)
model <- lm(train_y~., data = new_train)
boxcox(model, plotit = TRUE, lambda = seq(-3, 3, 1/10))
train_y <- log(train_y)
new_train$train_y <- train_y
qplot(train_y, geom = "histogram", bins=40)

#linear regression
model <- lm(train_y~., data = new_train)
fw.model <- step(model, direction = "forward")
summary(fw.model)
plot(fw.model)
skewness(fw.model$residuals)

predict_lm <- predict(fw.model, train_x)
error <- train_y - predict_lm
sqrt(mean(error^2))
predict <- predict(fw.model, test_x)
predict <- exp(predict)

submission <- read.csv(file = "sample_submission.csv")
submission <- as.data.frame(cbind(submission[,1], predict))
colnames(submission) <- c("ID", "SalePrice")
write.csv(submission, file = "submission.csv", row.names = FALSE)


#lasso regression(0.14052)==============================
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = F,  
                     summaryFunction = defaultSummary,
                     allowParallel=T)

lassofit <- train(train_y ~ .,
                  data = new_train,
                  method = "lars",
                  trControl = ctrl,
                  #preProcess=c("center","scale"), # not needed; already transformed
                  tuneLength = 15,                # this specifies various s values
                  metric = "RMSE")
lassofit
#plot(lassofit$finalModel)
lassofit$bestTune[[1]]
las_predict <- exp(predict(lassofit, test_x))

submission <- read.csv(file = "sample_submission.csv")
submission <- as.data.frame(cbind(submission[,1], predict))
colnames(submission) <- c("ID", "SalePrice")
write.csv(submission, file = "submission.csv", row.names = FALSE)

#SVM(0.15076)==================================
library(e1071)
svm_model<-svm(train_y~., data = new_train, cost = 3)
svm_pred <- exp(predict(svm_model,newdata = test_x))
submission <- read.csv(file = "sample_submission.csv")
submission <- as.data.frame(cbind(submission[,1], svm_pred))
colnames(submission) <- c("ID", "SalePrice")
write.csv(submission, file = "submission_svm.csv", row.names = FALSE)

#visualising_the_plots
plot(new_train$LotFrontage, new_train$train_y)

