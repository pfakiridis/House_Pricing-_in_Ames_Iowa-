#Summary Statistics

dim(train)
dim(test)
summary(train)
summary(test)
summary(train)
str(test)
str(train)

#Checking NAs
#train

NA_count <- apply(is.na(train), 2, sum)
NA_count
# ...and the percentage of NA per variable
NA_perc <- NA_count / dim(train)[1] * 100
NA_perc

#test

NA_count <- apply(is.na(test), 2, sum)
NA_count
# ...and the percentage of NA per variable
NA_perc <- NA_count / dim(test)[1] * 100
NA_perc

#Merging the two datasets together

test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
full <- rbind(train, test)
dim(full)

#Alley column deleted as most of the entries are filled with no NA meaning houses without Allers
test <- test[,-7]
train <- train[,-7]

#NAs cleaning. There are two ways to clean the data, either manually by replacing the NAs 
#with the median values of numerical variables or with the mode value of the categorical variables

#Cleaning the data manually

#lotfrintage train
median_lotfront <- median(train$LotFrontage, na.rm = T)
train[is.na(train$LotFrontage), 'LotFrontage'] = median_lotfront

#lotfrintage test
median_lotfront <- median(test$LotFrontage, na.rm = T)
test[is.na(test$LotFrontage), 'LotFrontage'] = median_lotfront


#Masvnrarea
median_lotfront <- median(test$MasVnrArea, na.rm = T)
test[is.na(test$MasVnrArea), 'MasVnrArea'] = median_lotfront
train$MasVnrArea <- as.numeric(train$MasVnrArea)
AQ1 <- train$MasVnrArea
AQ1[is.na(AQ1 <- train$MasVnrArea)] <- 0
AQ1
train$MasVnrArea <- AQ1


#MasVnrType
table(train$MasVnrType)
AQ1 <- train$MasVnrType
AQ1[is.na(AQ1 <- train$MasVnrType)] <- 'None'
AQ1
train$MasVnrType <- AQ1

#train_BsmtQual

levels(train$BsmtQual)
train$BsmtQual = factor(train$BsmtQual, levels=c(levels(train$BsmtQual), 'None'))
train$BsmtQual[is.na(train$BsmtQual)] = 'None'

#BsmtCont

levels(train$BsmtCond)
train$BsmtCond = factor(train$BsmtCond, levels=c(levels(train$BsmtCond), 'None'))
train$BsmtCond[is.na(train$BsmtCond)] = 'None'

#BsmtExposure
#train
levels(train$BsmtExposure)
train$BsmtExposure = factor(train$BsmtExposure, levels=c(levels(train$BsmtExposure), 'No_Bas'))
train$BsmtExposure[is.na(train$BsmtExposure)] = 'No_Bas'

#Bsmtfintype1
#train
levels(train$BsmtFinType1)
train$BsmtFinType1 = factor(train$BsmtFinType1, levels=c(levels(train$BsmtFinType1), 'No_Bas'))
train$BsmtFinType1[is.na(train$BsmtFinType1)] = 'No_Bas'

#Bsmtfintype2

levels(train$BsmtFinType2)
train$BsmtFinType2 = factor(train$BsmtFinType2, levels=c(levels(train$BsmtFinType2), 'No_Bas'))
train$BsmtFinType2[is.na(train$BsmtFinType2)] = 'No_Bas'

#Elecrical
train$Electrical[is.na(train$Electrical)] = 'SBrkr'

#Bsmtfintype2

levels(train$FireplaceQu)
train$FireplaceQu = factor(train$FireplaceQu, levels=c(levels(train$FireplaceQu), 'No_Fir'))
train$FireplaceQu[is.na(train$FireplaceQu)] = 'No_Fir'

#garage_type

levels(train$GarageType)
train$GarageType = factor(train$GarageType, levels=c(levels(train$GarageType), 'No_Gar'))
train$GarageType[is.na(train$GarageType)] = 'No_Gar'


#garage_yrblt

median_lotfront <- median(train$GarageYrBlt, na.rm = T)
train[is.na(test$GarageYrBlt), 'GarageYrBlt'] = median_lotfront
train$GarageYrBlt <- as.numeric(train$GarageYrBlt)
AQ1 <- train$GarageYrBlt
AQ1[is.na(AQ1 <- train$GarageYrBlt)] <- 1980
AQ1
train$GarageYrBlt <- AQ1

#garage_finish

levels(train$GarageFinish)
train$GarageFinish = factor(train$GarageFinish, levels=c(levels(train$GarageFinish), 'No_Gar'))
train$GarageFinish[is.na(train$GarageFinish)] = 'No_Gar'

#garage_qual

levels(train$GarageQual)
train$GarageQual = factor(train$GarageQual, levels=c(levels(train$GarageQual), 'No_Gar'))
train$GarageQual[is.na(train$GarageQual)] = 'No_Gar'

#poolQC

levels(train$PoolQC)
train$PoolQC = factor(train$PoolQC, levels=c(levels(train$PoolQC), 'No_Pool'))
train$PoolQC[is.na(train$PoolQC)] = 'No_Pool'

#fence
levels(train$Fence)
train$Fence = factor(train$Fence, levels=c(levels(train$Fence), 'No_Fen'))
train$Fence[is.na(train$Fence)] = 'No_Fen'

#missfeature
levels(train$MiscFeature)
train$MiscFeature = factor(train$MiscFeature, levels=c(levels(train$MiscFeature), 'None'))
train$MiscFeature[is.na(train$MiscFeature)] = 'None'

#GarageCond  
train$GarageCond[is.na(train$GarageCond)] = 'TA'

train$Alley = factor(train$Alley, levels=c(levels(train$Alley), 'None'))
train$Alley[is.na(train$Alley)] = 'None'

test$Alley = factor(test$Alley, levels=c(levels(test$Alley), 'None'))
test$Alley[is.na(test$Alley)] = 'None'

#NAs for test set

test$Exterior1st[is.na(test$Exterior1st)] = 'VinylSd'
test$BsmtExposure = factor(test$BsmtExposure, levels=c(levels(test$BsmtExposure), 'No_Bas'))
test$BsmtExposure[is.na(test$BsmtExposure)] = 'No_Bas'

test$BsmtFinType1 = factor(test$BsmtFinType1, levels=c(levels(test$BsmtFinType1), 'No_Bas'))
test$BsmtFinType1[is.na(test$BsmtFinType1)] = 'No_Bas'

test$BsmtFinType2 = factor(test$BsmtFinType2, levels=c(levels(test$BsmtFinType2), 'No_Bas'))
test$BsmtFinType2[is.na(test$BsmtFinType2)] = 'No_Bas'

median_lotfront <- median(test$TotalBsmtSF, na.rm = T)
test[is.na(test$TotalBsmtSF), 'TotalBsmtSF'] = median_lotfront

median_lotfront <- median(test$BsmtFullBath, na.rm = T)
test[is.na(test$BsmtFullBath), 'BsmtFullBath'] = median_lotfront

test$KitchenQual[is.na(test$KitchenQual)] = 'TA'

test$Functional[is.na(test$Functional)] = 'Typ'

test$FireplaceQu = factor(test$FireplaceQu, levels=c(levels(test$FireplaceQu), 'No_Fir'))
test$FireplaceQu[is.na(test$FireplaceQu)] = 'No_Fir'

test$MSZoning[is.na(test$MSZoning)] = 'RL'

test$Utilities[is.na(test$Utilities)] = 'AllPub'
test$Exterior2nd[is.na(test$Exterior2nd)] = 'VinylSd'
test$MasVnrType[is.na(test$MasVnrType)] = 'None'

test$BsmtQual = factor(test$BsmtQual, levels=c(levels(test$BsmtQual), 'No_Bas'))
test$BsmtQual[is.na(test$BsmtQual)] = 'No_Bas'

test$BsmtCond = factor(test$BsmtCond, levels=c(levels(test$BsmtCond), 'No_Bas'))
test$BsmtCond[is.na(test$BsmtCond)] = 'No_Bas'

median_lotfront <- median(test$BsmtFinSF1, na.rm = T)
test[is.na(test$BsmtFinSF1), 'BsmtFinSF1'] = median_lotfront

median_lotfront <- median(test$BsmtFinSF2, na.rm = T)
test[is.na(test$BsmtFinSF2), 'BsmtFinSF2'] = median_lotfront

median_lotfront <- median(test$BsmtUnfSF, na.rm = T)
test[is.na(test$BsmtUnfSF), 'BsmtUnfSF'] = median_lotfront

median_lotfront <- median(test$BsmtHalfBath, na.rm = T)
test[is.na(test$BsmtHalfBath), 'BsmtHalfBath'] = median_lotfront

test$GarageType = factor(test$GarageType, levels=c(levels(test$GarageType), 'No_Gar'))
test$GarageType[is.na(test$GarageType)] = 'No_Gar'

median_lotfront <- median(test$GarageYrBlt, na.rm = T)
test[is.na(test$GarageYrBlt), 'GarageYrBlt'] = median_lotfront


test$GarageFinish = factor(test$GarageFinish, levels=c(levels(test$GarageFinish), 'No_Gar'))
test$GarageFinish[is.na(test$GarageFinish)] = 'No_Gar'

median_lotfront <- median(test$GarageCars, na.rm = T)
test[is.na(test$GarageCars), 'GarageCars'] = median_lotfront

median_lotfront <- median(test$GarageArea, na.rm = T)
test[is.na(test$GarageArea), 'GarageArea'] = median_lotfront

test$GarageQual = factor(test$GarageQual, levels=c(levels(test$GarageQual), 'No_Gar'))
test$GarageQual[is.na(test$GarageQual)] = 'No_Gar'

test$GarageCond = factor(test$GarageCond, levels=c(levels(test$GarageCond), 'No_Gar'))
test$GarageCond[is.na(test$GarageCond)] = 'No_Gar'

test$PoolQC = factor(test$PoolQC, levels=c(levels(test$PoolQC), 'No_Pool'))
test$PoolQC[is.na(test$PoolQC)] = 'No_Pool'

test$Fence = factor(test$Fence, levels=c(levels(test$Fence), 'No_Fence'))
test$Fence[is.na(test$Fence)] = 'No_Fence'

test$SaleType[is.na(test$SaleType)] = 'WD'

#Principal Component Analysis for Dimensionality reduction

names <- c(1:80)
train[,names] <- lapply(train[,names] , as.numeric)

pc_price <- prcomp(train[,], center = T, scale. = T)
pc__var <- pc_price$sdev^2
pc__var
pc__PEV <- pc__var / sum(pc__var)
pc__PEV
plot(pc_price)
plot(
  cumsum(pc__PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'Cumulative PEV',
  pch = 20,
  col = 'orange'
)
abline(h = 0.8, col = 'red', lty = 'dashed')
biplot(
  pc_price,
  scale = 0,
  col = c('grey40','orange')
)
biplot(pc_price, expand=5, xlim=c(-0.3, 0.1), ylim=c(-0.1, 0.1))
biplot(
  pc_price,
  choices = c(1,3),
  scale = 0,
  col = c('grey40','orange')
)
biplot1 <- biplot(
  pc_price,
  choices = c(2,3),
  scale = 0,
  col = c('grey40','orange')
)

biplot(pc_price, expand=7, xlim=c(-0.3, 0.2), ylim=c(-0.1, 0.1))

#Removing variables which do not affect significantly the variation of the SalePrice

train <- train[,-c(2,21,50,11,10,22,4,14)]
test <- test[,-c(2,21,50,11,10,22,4,14)]
train <- train[,-c(4,5,6,17,18,23,24,29,30,38,41,50)]
test <- test[,-c(4,5,6,17,18,23,24,29,30,38,41,50)]
train <- train[,-c(46:50)]
test <- test[,-c(46:50)]
train <- train[,-c(39:44,14,46:50)]
test <- test[,-c(39:44,14,46:50)]
train <- train[,-c(27,41,42)]
test <- test[,-c(27,41,42)]
train <- train[,-c(4,19,24)]
test <- test[,-c(4,19,24)]
train <- train[,-c(1)]
test <- test[,-c(1)]

#Dealing with outliers

train <- subset(train, LotArea < 50000)
train <- subset(train, LotArea < 30000)
train <- subset(train, LotArea < 20000)
train <- subset(train, X1stFlrSF < 2200)
train <- subset(train, GrLivArea < 2650)

#Data Engineering
#yearbulit

train$YearBuilt[train$YearBuilt >= 1810 & train$YearBuilt <= 1900] <- '00'
train$YearBuilt[train$YearBuilt >= 1901 & train$YearBuilt <= 1910] <- '10'
train$YearBuilt[train$YearBuilt >= 1911 & train$YearBuilt <= 1920] <- '20'
train$YearBuilt[train$YearBuilt >= 1921 & train$YearBuilt <= 1930] <- '30'
train$YearBuilt[train$YearBuilt >= 1931 & train$YearBuilt <= 1940] <- '40'
train$YearBuilt[train$YearBuilt >= 1941 & train$YearBuilt <= 1950] <- '50'
train$YearBuilt[train$YearBuilt >= 1951 & train$YearBuilt <= 1960] <- '60'
train$YearBuilt[train$YearBuilt >= 1961 & train$YearBuilt <= 1970] <- '70'
train$YearBuilt[train$YearBuilt >= 1971 & train$YearBuilt <= 1980] <- '80'
train$YearBuilt[train$YearBuilt >= 1981 & train$YearBuilt <= 1990] <- '90'
train$YearBuilt[train$YearBuilt >= 1991 & train$YearBuilt <= 2000] <- '000'
train$YearBuilt[train$YearBuilt >= 2001 & train$YearBuilt <= 2010] <- '010'
train$YearBuilt[train$YearBuilt >= 2001 & train$YearBuilt <= 2010] <- '010'

#yearremoveadd

train$YearRemodAdd[train$YearRemodAdd >= 1931 & train$YearRemodAdd <= 1940] <- '40'
train$YearRemodAdd[train$YearRemodAdd >= 1941 & train$YearRemodAdd <= 1950] <- '50'
train$YearRemodAdd[train$YearRemodAdd >= 1951 & train$YearRemodAdd <= 1960] <- '60'
train$YearRemodAdd[train$YearRemodAdd >= 1961 & train$YearRemodAdd <= 1970] <- '70'
train$YearRemodAdd[train$YearRemodAdd >= 1971 & train$YearRemodAdd <= 1980] <- '80'
train$YearRemodAdd[train$YearRemodAdd >= 1981 & train$YearRemodAdd <= 1990] <- '90'
train$YearRemodAdd[train$YearRemodAdd >= 1991 & train$YearRemodAdd <= 2000] <- '000'
train$YearRemodAdd[train$YearRemodAdd >= 2001 & train$YearRemodAdd <= 2010] <- '010'
train$YearRemodAdd[train$YearRemodAdd >= 2001 & train$YearRemodAdd <= 2010] <- '010'


#Regression

reg <- lm(SalePrice~. , data = train)
summary(reg)

reg1 <- lm(SalePrice~ MSZoning + LotArea + Neighborhood + Condition1 + BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd + ExterQual + BsmtExposure + TotalBsmtSF + GrLivArea  + BsmtFullBath + KitchenQual + Functional + FireplaceQu, data = train)
summary(reg1)

reg2 <- lm(SalePrice~ MSZoning + LotArea + Neighborhood + BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd + ExterQual + BsmtExposure + TotalBsmtSF + GrLivArea  + BsmtFullBath + KitchenQual + FireplaceQu, data = train)
summary(reg2)

reg3 <- lm(SalePrice~ MSZoning + LotArea + Neighborhood + Condition1 + BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd + ExterQual + BsmtExposure + TotalBsmtSF + GrLivArea  + BsmtFullBath + KitchenQual + Functional + FireplaceQu + YrSold + MoSold, data = train)
summary(reg3)

anova(reg,reg1,reg2)


#svm

mymodel <- svm(SalePrice~ MSZoning + LotArea + Neighborhood + Condition1 + BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd + ExterQual + BsmtExposure + TotalBsmtSF + GrLivArea  + BsmtFullBath + KitchenQual + Functional + FireplaceQu, data = train, kernel = "linear")
mymodel <- svm(SalePrice~ MSZoning + LotArea + Neighborhood + Condition1 + LotArea + BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd + ExterQual + BsmtExposure + TotalBsmtSF + GrLivArea  + BsmtFullBath + KitchenQual + Functional + FireplaceQu, data = train, kernel = "polynomial")

pred <- predict(mymodel, train)
tab <- table(Predicted = pred, Actual = train$SalePrice)
tab
1-sum(diag(tab)/sum(tab))
sum(diag(tab)/sum(tab))



#random forest

formula = log10(SalePrice)~ MSZoning + LotArea + Neighborhood + Condition1 + BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd + ExterQual + BsmtExposure + TotalBsmtSF + GrLivArea  + BsmtFullBath + KitchenQual + Functional + FireplaceQu
formula = SalePrice~.
formula = SalePrice~ GrLivArea + Neighborhood + OverallQual + TotalBsmtSF + X1stFlrSF + X2ndFlrSF + GarageCars + BsmtFinSF1 + GarageArea + ExterQual + GarageType + YearBuilt + YearRemodAdd + FireplaceQu + TotRmsAbvGrd + GarageFinish + OverallCond + MSSubClass + FullBath + KitchenQual + LotArea

set.seed(2018)

rf_titanic <- randomForest(formula, ntree = 500, importance = T, data = training)
rf_titanic_pred <- predict(rf_titanic, testing[,-c(79)])
rf_results_table <- table(rf = rf_titanic_pred,  actual = train$SalePrice)
rf_results_table
acc_rf <- sum(diag(rf_results_table)) / sum(rf_results_table)
acc_rf

varImpPlot(rf_titanic, type = 1)

#parameters

ctrl_parameters <- trainControl(method = 'CV', number = 5, repeats = 3)


titanic_rpart <- train(formula, data = train, method = "rpart",trControl = ctrl_parameters)
titanic_gbm <- train(formula, data = train1, method = "gbm", trControl = my_control)
titanic_svm_radial <- train(formula, data = train, method = "svmRadial",trControl = ctrl_parameters)
titanic_svm_linear <- train(formula, data = train, method = "svmLinear",trControl = ctrl_parameters)
titanic_glm<- train(formula, data = train, method = "glm", trControl = ctrl_parameters)
titanic_treebag <- train(formula, data = train, method = "treebag", trControl = ctrl_parameters)

boosting_results <- resamples(list(rpart=titanic_rpart, gmb=titanic_gbm, glm=titanic_glm, treebag=titanic_treebag, gbm_tree_tune = gbm_tree_tune))
summary(boosting_results)
dot(boosting_results)


set.seed(2018)

gbm_tree_auto <- gbm_tree_auto <- train(formula, data = train, method = "gbm", distribution = "gaussian", trControl = ctrl_parameters, verbose = FALSE)

getModelInfo()$gbm$parameters

myGrid <- expand.grid(n.trees = c(150,200,225), interaction.depth = c(5,6,7,8,9), shrinkage = c(0.075,0.1,0.125,0.15,0.2), n.minobsinnode = c(7,10,12,15))

set.seed(2018)

gbm_tree_tune <-gbm_tree_auto <- train(formula, data = train, method = "gbm", distribution = "gaussian", trControl = ctrl_parameters, verbose = FALSE, tuneGrid = myGrid)


#Prediction

pred <- train%>%
  mutate(pred.reg.train = predict(titanic_gbm))

mse <- pred %>%
  mutate(error = pred.reg.train - SalePrice,
         sq.error = error^2) %>%
  summarise(mse = mean(sq.error))

rmse <- sqrt(mse)

#prediction test set

prediction <- predict(titanic_gbm, test)

test$SalePrice <- prediction

kaggle <- test[,-c(2:78)]

write.csv(kaggle, file = "Kaggle.Submission.csv", row.names = FALSE)

















