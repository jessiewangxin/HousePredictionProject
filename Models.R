library(glmnet)
library(tree)
library(randomForest)

# #MULTIVARIABLE MODEL  
num_columns = c('LotFrontage', 'MasVnrArea', 'LotArea', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 
                 'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'GrLivArea', 'BsmtFullBath', 'BsmtHalfBath', 
                 'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces',
                 'GarageCars', 'GarageArea', 'TotalPorchSF', 'PoolArea', 'MiscVal','SalePrice')
 
full_linear_model = lm(SalePrice  ~ ., data_use[num_columns])
vif(full_linear_model)
influencePlot(full_linear_model)
avPlots(full_linear_model)
summary(full_linear_model)


#RIDGE 

x = model.matrix(LogSalePrice ~ ., data_use)[, -1] #Dropping the intercept column.
y = data_use$LogSalePrice

grid = 10^seq(5, -2, length = 100)
ridge.models = glmnet(x, y, alpha = 0, lambda = grid)

lasso.models = glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")
dim(coef(ridge.models))
coef(ridge.models)

#LASSO

#ELASTIC NET

#TREE


tree.one = tree(LogSalePrice ~ . , data = data_use[num_columns])
summary(tree.one)
plot(tree.one)
text(tree.one, pretty = 0) 


#RANDOMFOREST

#XGBOOST