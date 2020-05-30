library(randomForest)
library(caret)
library(tree)
library(dplyr)
library(tidyr)


data_rf = data_use %>% mutate_if(is.character, as.factor)
data_rf$SalePrice <- NULL 
data_rf$MoYrSold <- NULL
data_rf$MSSubClass <- NULL #extra level in test 
data_rf$MSZoning <- NULL
data_rf$Utilities <- NULL

#splitting into training and testing data sets 
train.index <- createDataPartition(data_rf$LogSalePrice, p=0.8, list=FALSE)
data.train <- data_rf[train.index,]
data.test <- data_rf[-train.index,]

rf.default = randomForest(LogSalePrice ~., data = data.train, importance = TRUE)

# training and testing accuracies
rf.default

# top variable
importance(rf.default)
varImpPlot(rf.default)

# MSE 
rf1.predictions = predict(rf.default, data.test)
mean((rf1.predictions - data.test$LogSalePrice)^2) #.0212

data_test$SalePrice <- NULL 
data_test$MoYrSold <- NULL
data_test$MSSubClass <- NULL # different levels
data_test$MSZoning <- NULL
data_test$Utilities <- NULL

#RF default prediction vector 
rf_default_predictions = predict(rf.default, data_test%>% mutate_if(is.character, as.factor))
