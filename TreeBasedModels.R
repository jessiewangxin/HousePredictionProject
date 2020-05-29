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

######### FIRST TREE 
initial.tree = tree(LogSalePrice ~ ., data = data.train ,split='gini')

# training accuracy
summary(initial.tree) #187 terminal nodes 

# MSE 
tree.predictions = predict(initial.tree, data.test)
mean((tree.predictions - data.test$LogSalePrice)^2) #.0332

#prediction vectors using real test 
tree.predictions = predict(initial.tree, data_test%>% mutate_if(is.character, as.factor))
submission = exp(data.frame(tree.predictions)) %>% rename(SalePrice = tree.predictions)
#class(submission)
#class(data_test)
treesubmission =data.frame(c(data_test['Id'],submission))
rownames(treesubmission) <- treesubmission$Id
treesubmission$Id <- NULL
colnames(treesubmission) = c('Id','SalePrice')
write.csv(treesubmission,'firsttree.csv')



##############RANDOM FOREST

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



###############XG BOOST
library(gbm)

#Fitting 10,000 trees with a depth of 4.
set.seed(0)
boost_tree = gbm(LogSalePrice ~ ., data = data.train,
                   distribution = "gaussian",
                   n.trees = 10000,
                   interaction.depth = 4)

#Inspecting the relative influence.
par(mfrow = c(1, 1))
summary(boost.boston)

#Let’s make a prediction on the test set. With boosting, the number of trees is
#a tuning parameter; having too many can cause overfitting. In general, we should
#use cross validation to select the number of trees. Instead, we will compute the
#test error as a function of the number of trees and make a plot for illustrative
#purposes.
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.boston, newdata = Boston[-train, ], n.trees = n.trees)

#Produces 100 different predictions for each of the 152 observations in our
#test set.
dim(predmat)

#Calculating the boosted errors.
par(mfrow = c(1, 1))
berr = with(Boston[-train, ], apply((predmat - medv)^2, 2, mean))
plot(n.trees, berr, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

#Include the best OOB error from the random forest.
abline(h = min(oob.err), col = "red")

#Increasing the shrinkage parameter; a higher proportion of the errors are
#carried over.
set.seed(0)
boost.boston2 = gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian",
                    n.trees = 10000,
                    interaction.depth = 4,
                    shrinkage = 0.1)
predmat2 = predict(boost.boston2, newdata = Boston[-train, ], n.trees = n.trees)

berr2 = with(Boston[-train, ], apply((predmat2 - medv)^2, 2, mean))
plot(n.trees, berr2, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")
