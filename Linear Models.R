library(glmnet)
library(caret)

#####################################RIDGE 

data_use = Filter(is.numeric, data_use)
data_use$SalePrice <-NULL
x = model.matrix(LogSalePrice ~ ., data_use)[, -1] #Dropping the intercept column.
y = data_use$LogSalePrice

#splitting into training and testing data sets 
train.index <- createDataPartition(data_use$LogSalePrice, p=0.8, list=FALSE)
data.train <- data_use[train.index,]
data.test <- data_use[-train.index,]

y.test = data.test$LogSalePrice


grid = 10^seq(5, -2, length = 100)
ridge.models = glmnet(x[train.index, ], y[train.index], alpha = 0, lambda = grid)
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

cv.ridge.out = cv.glmnet(x[train.index, ], y[train.index], alpha = 0, nfolds = 5, lambda = grid)
plot(cv.ridge.out, main = "Ridge Regression\n")

bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge      #.01
log(bestlambda.ridge) 

#refit RIDGE
ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x[-train.index, ])
mean((ridge.bestlambdatrain - y.test)^2)  # 0.062675

ridge.best_refit = glmnet(x, y, alpha = 0, lambda = bestlambda.ridge) 

#Coefficients
predict(ridge.best_refit, s = bestlambda.ridge, type = "coefficients")

# MSE
ridge.bestlambda = predict(ridge.best_refit, s = bestlambda.ridge, newx = x)
mean((ridge.bestlambda - y)^2)  # 0.00876


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
mean((lasso.bestlambdatrain - y.test)^2) #.01573

#Refit a model & Results
lasso.best_refit = glmnet(x, y, alpha = 1)

# Coefficients
predict(lasso.best_refit, type = "coefficients", s = bestlambda.lasso)

# MSE
lasso.bestlambda = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)  # 0.00894039
mean((lasso.bestlambda - data_use$LogSalePrice)^2)  # 0.021423


#################################FITTING TO ACTUAL TEST DATA #####################OUTPUT FILES


data_test = Filter(is.numeric, data_test)
x = model.matrix(~ ., data_test)[, -1] #Dropping the intercept column

#LASSO
lasso_official = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)

submission = exp(data.frame(lasso_official)) %>% rename(SalePrice = X1)
# class(submission)
# class(data_test)
lassosubmission =data.frame(c(data_test['Id'],submission))

write.csv(lassosubmission,'lasso.csv')

#RIDGE
ridge_official = predict(ridge.best_refit, s = bestlambda.ridge, newx = x)

submission = exp(data.frame(ridge_official)) %>% rename(SalePrice = X1)
# class(submission)
# class(data_test)
ridgesubmission = data.frame(c(data_test['Id'],submission['SalePrice']))

write.csv(ridgesubmission,'ridge.csv')
