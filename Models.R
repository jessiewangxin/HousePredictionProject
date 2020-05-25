library(glmnet)
library(tree)
library(randomForest)

#MULTIVARIABLE MODEL  
num_columns = c('LotFrontage', 'MasVnrArea', 'LotArea', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 
                'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'GrLivArea', 'BsmtFullBath', 'BsmtHalfBath', 
                'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces',
                'GarageCars', 'GarageArea', 'TotalPorchSF', 'PoolArea', 'MiscVal','SalePrice')

full_linear_model = lm(SalePrice  ~ ., data_use[num_columns])
vif(full_linear_model)
library(car)
influencePlot(full_linear_model)

vif(full_linear_model)

avPlots(full_linear_model)


#LASSO RIDGE MODELS 

x = model.matrix(SalePrice ~ ., data_use[num_columns])[, -1] #Dropping the intercept column.
y = data_use$SalePrice

ridge.models = glmnet(x, y, alpha = 0, lambda = grid)

lasso.models = glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")


#TREE
tree.one = tree(LogSalePrice ~ . , data = data_use[num_columns])
summary(tree.one)
plot(tree.one)
text(tree.one, pretty = 0) 


#RANDOMFOREST

#XGBOOST