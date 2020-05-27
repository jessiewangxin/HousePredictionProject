library(glmnet)
library(tree)
library(randomForest)


#RIDGE 
num_columns = c('LotFrontage', 'MasVnrArea', 'LotArea', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 
                'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'GrLivArea', 'BsmtFullBath', 'BsmtHalfBath', 
                'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces',
                'GarageCars', 'GarageArea', 'TotalPorchSF', 'PoolArea', 'MiscVal','LogSalePrice')

x = model.matrix(LogSalePrice ~ ., data_use[num_columns])[, -1] #Dropping the intercept column.
y = data_use$LogSalePrice

set.seed(0)
train = sample(1:nrow(x), nrow(x)*0.8)
test = (-train)
y.test = y[test]

length(train)/nrow(x)  # 0.7938144
length(y.test)/nrow(x) # 0.2061856

grid = 10^seq(5, -2, length = 100)
ridge.models = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

cv.ridge.out = cv.glmnet(x[train, ], y[train], alpha = 0, nfolds = 10, lambda = grid)
plot(cv.ridge.out, main = "Ridge Regression\n")

bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge       #.08302176
log(bestlambda.ridge)  #-2.488653

#refit RIDGE
ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)  # 0.04019026

ridge.best_refit = glmnet(x, y, alpha = 0, lambda = bestlambda.ridge)

#Coefficients
predict(ridge.best_refit, s = bestlambda.ridge, type = "coefficients")

# MSE
ridge.bestlambda = predict(ridge.best_refit, s = bestlambda.ridge, newx = x)
mean((ridge.bestlambda - y)^2)  # 0.03794408

#LASSO

lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)

plot(cv.lasso.out, main = "Lasso Regression\n")

#7 Results
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso       # 0.01
log(bestlambda.lasso)  # -4.60517
#MSE
lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2) #.04146037

#9 Refit a model & Results
lasso.best_refit = glmnet(x, y, alpha = 1)

# Coefficients
predict(lasso.best_refit, type = "coefficients", s = bestlambda.lasso)

# MSE
lasso.bestlambda = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)  # 0.03830363

####COMPARE LASSO AND RIDGE
predict(ridge.best_refit, type = "coefficients", s = bestlambda.ridge)
predict(lasso.best_refit, type = "coefficients", s = bestlambda.lasso)
mean((ridge.bestlambda - y)^2)  # 0.03784408 - ridge is lower 
mean((lasso.bestlambda - y)^2)  # 0.03830363 - less complex model using fewer components

#ELASTIC NET


#TREE


tree.one = tree(LogSalePrice ~ . , data = data_use[num_columns])
summary(tree.one)
plot(tree.one)
text(tree.one, pretty = 0) 


#RANDOMFOREST

#XGBOOST