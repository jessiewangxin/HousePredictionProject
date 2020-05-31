library(randomForest)
library(caret)
library(tree)
library(dplyr)
library(tidyr)


data_rf = data_use %>% mutate_if(is.character, as.factor)
data_rf$Id <- NULL 
data_rf$SalePrice <- NULL 

#data_rf$Neighborhood <- NULL
#data_rf$Condition2 <- NULL
#data_rf$HouseStyle <- NULL
#data_rf$RoofMatl <- NULL
#data_rf$MiscFeature <- NULL
data_rf$Utilities <- NULL
#data_rf$Electrical <- NULL

#splitting into training and testing data sets 
train.index <- createDataPartition(data_rf$LogSalePrice, p=0.8, list=FALSE)
data.train <- data_rf[train.index,]
data.test <- data_rf[-train.index,]

#number of variables 

set.seed(0)
oob.err = numeric(13)
for (mtry in 1:13) {
  fit = randomForest(LogSalePrice ~ ., data = data.train, mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
  
  list_error[[i]] = boost_error
}

#Visualizing the OOB error.
plot(1:13, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")


n.trees = seq(from = 1000, to = 10000, by = 1000)

list_error = c()
for(i in depth){
  
  rf.test = randomForest(LogSalePrice ~., data = data.train, importance = TRUE, ntree = i)
  rf_prediction = predict(rf.test, data.train)
  mean((rf_predictions - data.test$LogSalePrice)^2) #.0212
  
  boost_error = mean((prediction_value$prediction_value - data.test$LogSalePrice)^2)
  
  list_error[[i]] = boost_error
  
}


rf.default = randomForest(LogSalePrice ~., data = data.train, importance = TRUE, ntree = 5000)

# training and testing accuracies
rf.default

# top variable
importance(rf.default)
varImpPlot(rf.default)

# MSE 
rf1.predictions = predict(rf.default, data.test)
mean((rf1.predictions - data.test$LogSalePrice)^2) #.0212

#####################REAL TEST 
data_rf_test = data_test%>% mutate_if(is.character, as.factor)
data_rf_test$SalePrice <- NULL 
#data_rf_test$MoYrSold <- NULL


data_rf_test$Id <- NULL 

#rf$Neighborhood <- NULL
#data_rf_test$Condition2 <- NULL
#data_rf_test$HouseStyle <- NULL
#data_rf_test$RoofMatl <- NULL
#data_rf_test$MiscFeature <- NULL
data_rf_test$Utilities <- NULL
#data_rf_test$Electrical <- NULL

# #type issue - binding train and test together, then removing first row (from train)
# data_temp = data_rf_test
# data_rf_test <- bind_rows(data_temp[1, ] , data_rf_test)
# data_rf_test <- data_rf[-1,]

#RF default prediction vector 
rf_default_predictions = predict(rf.default, data_rf_test)

submission = exp(data.frame(rf_default_predictions)) 
submission$SalePrice = submission$rf_default_predictions
submission$rf_default_predictions <- NULL

treesubmission = data.frame(c(data_test['Id'],submission %>% select(SalePrice)))

write.csv(treesubmission,'treeresults.csv')
