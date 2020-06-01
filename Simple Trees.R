#Simple Trees
library(dplyr)
library(caret)
library(tree)

material_columns = c('MasVnrType', 'Exterior2nd', 'Exterior1st', 'StandardRoof', 'RoofStyle','LogSalePrice')

data_tree = data_use[material_columns] %>% mutate_if(is.character, as.factor)


#splitting into training and testing data sets 
train.index <- createDataPartition(data_tree$LogSalePrice, p=0.8, list=FALSE)
data.train <- data_tree[train.index,]
data.test <- data_tree[-train.index,]

######### FIRST TREE 
initial.tree = tree(LogSalePrice ~ ., data = data.train ,split='gini')

# training accuracy
summary(initial.tree) #42 terminal nodes 

plot(initial.tree)
text(initial.tree, pretty = 0)
# MSE 
tree.predictions = predict(initial.tree, data.test)
mean((tree.predictions - data.test$LogSalePrice)^2) 

###############################################

#MasVnrType
material_columns = c('MasVnrType','LogSalePrice')

data_tree = data_use[material_columns] %>% mutate_if(is.character, as.factor)

train.index <- createDataPartition(data_tree$LogSalePrice, p=0.8, list=FALSE)
data.train <- data_tree[train.index,]
data.test <- data_tree[-train.index,]

initial.tree = tree(LogSalePrice ~ ., data = data.train,split='gini')
summary(initial.tree) 
plot(initial.tree)
text(initial.tree, pretty = 0)

# Exterior2nd and Exterior1st
material_columns = c('Exterior2nd','LogSalePrice')

data_tree = data_use[material_columns] %>% mutate_if(is.character, as.factor)

train.index <- createDataPartition(data_tree$LogSalePrice, p=0.8, list=FALSE)
data.train <- data_tree[train.index,]
data.test <- data_tree[-train.index,]

initial.tree = tree(LogSalePrice ~ ., data = data.train,split='gini')
summary(initial.tree) 
plot(initial.tree)
text(initial.tree, pretty = 0)

#RoofStyle
material_columns = c('RoofStyle','LogSalePrice')

data_tree = data_use[material_columns] %>% mutate_if(is.character, as.factor)

train.index <- createDataPartition(data_tree$LogSalePrice, p=0.8, list=FALSE)
data.train <- data_tree[train.index,]
data.test <- data_tree[-train.index,]

initial.tree = tree(LogSalePrice ~ ., data = data.train,split='gini')
summary(initial.tree) 
plot(initial.tree)
text(initial.tree, pretty = 0)

# MSE 
tree.predictions = predict(initial.tree, data.test)
mean((tree.predictions - data.test$LogSalePrice)^2) 


