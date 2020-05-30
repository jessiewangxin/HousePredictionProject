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
data.train <- data_use[train.index,]
data.test <- data_use[-train.index,]

y.test = data.test$LogSalePrice


grid = 10^seq(5, -2, length = 100)
ridge.models = glmnet(x[train.index, ], y[train.index], alpha = 0, lambda = grid)
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

cv.ridge.out = cv.glmnet(x[train.index, ], y[train.index], alpha = 0, nfolds = 5, lambda = grid)
plot(cv.ridge.out, main = "Ridge Regression\n")

bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge      #.1353048
log(bestlambda.ridge) 

#refit RIDGE
ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x[-train.index, ])
mean((ridge.bestlambdatrain - y.test)^2)  # 0.062675

ridge.best_refit = glmnet(x, y, alpha = 0, lambda = bestlambda.ridge) #.05546

#Coefficients
predict(ridge.best_refit, s = bestlambda.ridge, type = "coefficients")

# MSE
ridge.bestlambda = predict(ridge.best_refit, s = bestlambda.ridge, newx = x)
mean((ridge.bestlambda - y)^2)  # 0.03628087


##############################################LASSO

lasso.models = glmnet(x[train.index, ], y[train.index], alpha = 1, lambda = grid)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

set.seed(0)
cv.lasso.out = cv.glmnet(x[train.index, ], y[train.index], alpha = 1, nfolds = 5, lambda = grid)

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
mean((lasso.bestlambda - y)^2)  # 0.03733635

#################################FITTING TO ACTUAL TEST DATA 

#use data_test cleaned from HousingPredictionCleanCode.R 

num_columns = c('LotFrontage', 'MasVnrArea', 'LotArea', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 
                'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'GrLivArea', 'BsmtFullBath', 'BsmtHalfBath', 
                'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces',
                'GarageCars', 'GarageArea', 'TotalPorchSF', 'PoolArea', 'MiscVal')

x = model.matrix(~ ., data_test[num_columns])[, -1] #Dropping the intercept column




#####################################ELASTIC NET






####################################################OUTPUT FILES 

#LASSO
lasso_official = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)

submission = exp(data.frame(lasso_official)) %>% rename(SalePrice = X1)
# class(submission)
# class(data_test)
lassosubmission =c(data_test['Id'],submission)

write.csv(lassosubmission,'lasso.csv')

#RIDGE
ridge_official = predict(ridge.best_refit, s = bestlambda.ridge, newx = x)

submission = exp(data.frame(ridge_official)) %>% rename(SalePrice = X1)
# class(submission)
# class(data_test)
ridgesubmission = data.frame(c(data_test['Id'],submission['SalePrice']))

write.csv(ridgesubmission,'ridge.csv')
