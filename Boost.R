library(randomForest)
library(caret)
library(tree)
library(dplyr)
library(tidyr)
library(gbm)

data_boost = data_use %>% mutate_if(is.character, as.factor)
data_boost$SalePrice <- NULL 
data_boost$MoYrSold <- NULL
# data_boost$MSSubClass <- NULL #extra level in test 
# data_boost$MSZoning <- NULL
# data_rf$Utilities <- NULL

#splitting into training and testing data sets 
train.index <- createDataPartition(data_boost$LogSalePrice, p=0.8, list=FALSE)
data.train <- data_boost[train.index,]
data.test <- data_boost[-train.index,]

data_test$SalePrice <- NULL 
data_test$MoYrSold <- NULL
# data_test$MSSubClass <- NULL # different levels
# data_test$MSZoning <- NULL
# 
# data_test$Utilities <- NULL

# testing different depths
depth = seq(1:8)

list_error = c()
for(i in depth){
        
        boostmodel = gbm(LogSalePrice ~ ., data = data.train,
                            distribution = "gaussian",
                            n.trees = 10000,
                            interaction.depth = i,
                            shrinkage = 0.01)
        
        prediction_value = data.frame(predict(boostmodel, newdata = data.test,n.trees = 10000))
        
        colnames(prediction_value) <- 'prediction_value'
        
        boost_error = mean((prediction_value$prediction_value - data.test$LogSalePrice)^2)
        
        list_error[[i]] = boost_error

}

list_error
min(list_error) #depth = 2

#testing different shrinkage rates 
shrink_values = c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.3)

list_error2 = c()
for(i in 1:length(shrink_values)){
        
        boostmodel = gbm(LogSalePrice ~ ., data = data.train,
                         distribution = "gaussian",
                         n.trees = 10000,
                         interaction.depth = 2,
                         shrinkage = shrink_values[i])
        
        prediction_value = data.frame(predict(boostmodel, newdata = data.test,n.trees = 10000))
        
        colnames(prediction_value) <- 'prediction_value'
        
        boost_error = mean((prediction_value$prediction_value - data.test$LogSalePrice)^2)
        
        list_error2[[i]] = boost_error
        
}

list_error2
min(list_error2)
#finding ideal number of trees

bestboostmodel = gbm(LogSalePrice ~ ., data = data.train,
                     distribution = "gaussian",
                     n.trees = 10000,
                     interaction.depth = 2,
                     shrinkage = 0.01)

n.trees = seq(from = 100, to = 10000, by = 100)

prediction_matrix2 = predict(bestboostmodel, newdata = data.test, n.trees = n.trees)

boost_error2 = with(data.test, apply((prediction_matrix2 - LogSalePrice)^2, 2, mean))

plot(n.trees, boost_error2, pch = 16,
      ylab = "Mean Squared Error",
      xlab = "# Trees",
      main = "Boosting Test Error")
abline(h = min(boost_error2), col = "red") 

#test 2200 trees
bestboostmodel2 = gbm(LogSalePrice ~ ., data = data.train,
                     distribution = "gaussian",
                     n.trees = 2200,
                     interaction.depth = 2,
                     shrinkage = 0.01)

prediction_matrix2 = data.frame(predict(bestboostmodel, newdata = data.test, n.trees = 2200))
colnames(prediction_value) <- 'prediction_value'

boost_error = mean((prediction_value$prediction_value - data.test$LogSalePrice)^2)


min(boost_error2)
boost_error2
#prediction vectors using real test 
boost.predictions = predict(boostmodel, data_test%>% mutate_if(is.character, as.factor), n.trees = 2200)
submission = exp(data.frame(boost.predictions)) 
submission$SalePrice = submission$boost.predictions
submission$boost.predictions <- NULL

boostsubmission =data.frame(c(data_test['Id'],submission %>% select(SalePrice)))

write.csv(boostsubmission,'boostresults.csv')
