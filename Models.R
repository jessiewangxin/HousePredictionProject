library(glmnet)
library(tree)
library(randomForest)
library(caret)

#####################################RIDGE 

num_columns = c('LotFrontage', 'MasVnrArea', 'LotArea', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 
                'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'GrLivArea', 'BsmtFullBath', 'BsmtHalfBath', 
                'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces',
                'GarageCars', 'GarageArea', 'TotalPorchSF', 'PoolArea', 'MiscVal','LogSalePrice')

x = model.matrix(LogSalePrice ~ ., data_use[num_columns])[, -1] #Dropping the intercept column.
y = data_use$LogSalePrice

#splitting into training and testing data sets 
train.index <- createDataPartition(data_use$SalePrice, p=0.8, list=FALSE)
#data.train <- data_use[train.index,]
#data.test <- data_use[-train.index,]

y.test = data.test$LogSalePrice


grid = 10^seq(5, -2, length = 100)
ridge.models = glmnet(x[train.index, ], y[train.index], alpha = 0, lambda = grid)
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

cv.ridge.out = cv.glmnet(x[train.index, ], y[train.index], alpha = 0, nfolds = 10, lambda = grid)
plot(cv.ridge.out, main = "Ridge Regression\n")

bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge       #.02656088
log(bestlambda.ridge)  #3.628316

#refit RIDGE
ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x[-train.index, ])
mean((ridge.bestlambdatrain - y.test)^2)  # 0.04019026

ridge.best_refit = glmnet(x, y, alpha = 0, lambda = bestlambda.ridge) #.05546

#Coefficients
predict(ridge.best_refit, s = bestlambda.ridge, type = "coefficients")

# MSE
ridge.bestlambda = predict(ridge.best_refit, s = bestlambda.ridge, newx = x)
mean((ridge.bestlambda - y)^2)  # 0.03641061

##############################################LASSO

lasso.models = glmnet(x[train.index, ], y[train.index], alpha = 1, lambda = grid)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

set.seed(0)
cv.lasso.out = cv.glmnet(x[train.index, ], y[train.index], alpha = 1, nfolds = 10, lambda = grid)

plot(cv.lasso.out, main = "Lasso Regression\n")

#7 Results
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso       # 0.01
log(bestlambda.lasso)  # -4.60517
#MSE
lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[-train.index, ])
mean((lasso.bestlambdatrain - y.test)^2) #.05188425


#Refit a model & Results
lasso.best_refit = glmnet(x, y, alpha = 1)

# Coefficients
predict(lasso.best_refit, type = "coefficients", s = bestlambda.lasso)

# MSE
lasso.bestlambda = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)  # 0.03635368

##################################COMPARE LASSO AND RIDGE
predict(ridge.best_refit, type = "coefficients", s = bestlambda.ridge)
predict(lasso.best_refit, type = "coefficients", s = bestlambda.lasso)
mean((ridge.bestlambda - y)^2)  # 0.03641061 
mean((lasso.bestlambda - y)^2)  # 0.03635368 - lasso is lower and less complex model



#################################FITTING TO ACTUAL TEST DATA 
num_columns = c('Id','LotFrontage', 'MasVnrArea', 'LotArea', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 
                'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'GrLivArea', 'BsmtFullBath', 'BsmtHalfBath', 
                'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces',
                'GarageCars', 'GarageArea', 'TotalPorchSF', 'PoolArea', 'MiscVal','LogSalePrice')

data_test$LogSalePrice <-0

x = model.matrix(LogSalePrice ~ ., data_test[num_columns])[, -1] #Dropping the intercept column
index = data.frame(x)['Id']
x$Id <- NULL
y = data_test$LogSalePrice

lasso.official_refit = glmnet(x, y, alpha = 1)
lasso_official = predict(lasso.official_refit, s = bestlambda.lasso, newx = x)
submission = exp(data.frame(lasso_official)) %>% rename(SalePrice = X1)
class(submission)
class(data_test)
test =c(data_test['Id'],submission)

#TREE


tree.one = tree(LogSalePrice ~ . , data = data_use[num_columns])
summary(tree.one)
plot(tree.one)
text(tree.one, pretty = 0) 


#RANDOMFOREST

#XGBOOST