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

################################### CHOOSING NUMBER OF VARIABLES 

numvar_error=c()
set.seed(0)
oob.err = numeric(13)
for (mtry in 1:13) {
  fit = randomForest(LogSalePrice ~ ., data = data.train, mtry = mtry)
  rf_predictions = predict(fit, data.test)
  numvar_error[[mtry]] = mean((rf_predictions - data.test$LogSalePrice)^2) #.0212
}
numvar_error
min(numvar_error)

#Visualizing errors - looks like ideal number of trees is 6 
plot(1:13, numvar_error, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB MSE",
     main = "Random Forest OOB Error Rates\nby # of Variables")

###########################################CHOOSING NUMBER OF TREES
n.trees = seq(from = 1000, to = 10000, by = 1000)
numtree_error = c()

for(i in 1:10){
  
  rf.test = randomForest(LogSalePrice ~., data = data.train, importance = TRUE, ntree = n.trees[i],mtry = 6)
  rf_predictions = predict(rf.test, data.test)
  print(mean((rf_predictions - data.test$LogSalePrice)^2))
  numtree_error[[i]] = mean((rf_predictions - data.test$LogSalePrice)^2) #.0212

  }

plot(seq(from = 1000, to = 10000, by = 1000), numtree_error, pch = 16, type = "b",
     xlab = "Number of Trees",
     ylab = "OOB MSE",
     main = "Random Forest OOB Error Rates\nby # of Trees")


numtree_error
min(numtree_error)

#############################################FINAL MODEL 
rf.default = randomForest(LogSalePrice ~., data = data.train, importance = TRUE, ntree = 10000, mtry=6)

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

treesubmission = data.frame(c(data_test['Id'],submission))

write.csv(treesubmission,'treeresults.csv')
