library(randomForest)
library(caret)
library(tree)
library(dplyr)
library(tidyr)
library(ggplot2)


data_rf = data_use %>% mutate_if(is.character, as.factor)
data_rf$Id <- NULL 
data_rf$SalePrice <- NULL 


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
  numvar_error[[mtry]] = mean((rf_predictions - data.test$LogSalePrice)^2) 
}
numvar_error
min(numvar_error)

#Visualizing errors - looks like ideal number of trees is 6 
plot(1:13, numvar_error, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB MSE",
     main = "Random Forest OOB Error Rates\nby # of Variables")

###########################################CHOOSING NUMBER OF TREES
# n.trees = seq(from = 1000, to = 10000, by = 1000)
# numtree_error = c()
#  
# for(i in 1:10){
#   
# rf.test = randomForest(LogSalePrice ~., data = data.train, importance = TRUE, ntree = n.trees[i],mtry = 8)
#   rf_predictions = predict(rf.test, data.test)
#   print(mean((rf_predictions - data.test$LogSalePrice)^2))
#   numtree_error[[i]] = mean((rf_predictions - data.test$LogSalePrice)^2) #.0212
# 
#   }
# 
# numtree_error
# min(numtree_error)
# 
# rf.test = randomForest(LogSalePrice ~., data = data.train, importance = TRUE, ntree = 500,mtry = 8)
# rf_predictions = predict(rf.test, data.test)
# mean((rf_predictions - data.test$LogSalePrice)^2)

#############################################FINAL MODEL 


rf.default = randomForest(LogSalePrice ~., data = data.train, importance = TRUE, ntree = 500, mtry=8)

# training and testing accuracies
rf.default

# top variable
importance(rf.default)
varImpPlot(rf.default)

# MSE 
rf1.predictions = predict(rf.default, data.test)
sqrt(mean((rf1.predictions - data.test$LogSalePrice)^2)) #RMSE

residuals = rf1.predictions - data.test$LogSalePrice
plot(data.test$LogSalePrice, residuals,ylab="Residuals", xlab="Log Sale Price") 
abline(0, 0) 

tree_info=data.frame(importance(rf.default))
tree_info <- cbind(VarName = rownames(tree_info), tree_info)

tree_info %>%
  select(VarName, X.IncMSE) %>% 
  filter(X.IncMSE > 10) %>% 
  ggplot(aes(x = reorder(VarName,X.IncMSE), y = X.IncMSE)) + 
  geom_col(aes(fill = X.IncMSE), width = 0.8) + 
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +        
  xlab("") +
  ylab("Variable Importance by $IncMSE") 

#####################REAL TEST 
data_rf_test = data_test%>% mutate_if(is.character, as.factor)
data_rf_test$Id <- NULL 

#data_rf_test$Exterior2nd <- NULL
# #type issue - binding train and test together, then removing first row (from train)
# data_temp = data_rf_test
# data_rf_test <- bind_rows(data_temp[1, ] , data_rf_test)
# data_rf_test <- data_rf[-1,]

#RF default prediction vector 

data_rf_test = data_rf_test%>% mutate_if(is.character, as.factor)
rf_default_predictions = predict(rf.default, data_rf_test)

submission = exp(data.frame(rf_default_predictions)) 
submission$SalePrice = submission$rf_default_predictions
submission$rf_default_predictions <- NULL


treesubmission = data.frame(c(submission_id,submission))

write.csv(treesubmission,'treesubmission.csv')
max(test10$SalePrice - test6$SalePrice)
##############MATERIALS

materials = c('BldgType','HouseStyle','Exterior1st','MasVnrType', 'CentralAir_flag',
             'Foundation','Electrical','GarageType', 'GarageQual_flag','Fence_flag','Pool_flag', 'LogSalePrice')
constr_tree = data_rf[materials]

#splitting into training and testing data sets 
train.index <- createDataPartition(data_rf$LogSalePrice, p=0.8, list=FALSE)
data.train.con <- constr_tree [train.index,]
data.test.con <- constr_tree [-train.index,]

rf.constr = randomForest(LogSalePrice ~., data = data.train.con, importance = TRUE, ntree = 500, mtry=6)

# training and testing accuracies
rf.constr

# top variable
importance(rf.constr)
varImpPlot(rf.constr)

# MSE 
rf1.predictions = predict(rf.constr, data.test.con)
sqrt(mean((rf1.predictions - data.test.con$LogSalePrice)^2)) #RMSE .292

tree_info=data.frame(importance(rf.constr))
tree_info <- cbind(VarName = rownames(tree_info), tree_info)

tree_info %>%
  select(VarName, X.IncMSE) %>% 
  filter(X.IncMSE > 30) %>% 
  ggplot(aes(x = reorder(VarName,X.IncMSE), y = X.IncMSE)) + 
  geom_col(aes(fill = X.IncMSE), width = 0.8) + 
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +        
  xlab("") +
  ylab("Variable Importance by $IncMSE") 




