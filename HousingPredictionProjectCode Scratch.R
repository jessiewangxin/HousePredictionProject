library(caret)
library(ggplot2)
library(dplyr)
library(corrplot)
library(Hmisc)
library(kknn)
library(car)
library(glmnet)
library(tree)
library(randomForest)

data_use <- read.csv("train.csv", header = T, stringsAsFactors = F)

#splitting into training and testing data sets 
train.index <- createDataPartition(data_use$SalePrice, p=0.8, list=FALSE)
data.train <- data_use[train.index,]
data.test <- data_use[-train.index,]

data_use = data.train
#MISSING VALUES 
#checking missing values
missing_df <- data.frame(colname = names(data_use),colSums_missing=colSums(is.na(data_use)))
#19 columns with missing values 
plot_missing = missing_df %>% filter(colSums_missing!=0) %>% arrange(desc(colSums_missing))
ggplot(plot_missing, aes(x = reorder(colname,colSums_missing), y = colSums_missing)) + 
  geom_col(aes(fill = colname), width = 0.7) + 
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +        
  xlab("") +
  ylab("Number of Missing Observations") 

#Redefining data and notes 

#PoolQ - NA means no pool
#Pool Quality
data_use$PoolQC<-ifelse(is.na(data_use$PoolQC),'NoPool',data_use$PoolQC)
unique(data_use$PoolQC)

#MiscFeature - NA means None
#Miscellaneous Feature 
data_use$MiscFeature<-ifelse(is.na(data_use$MiscFeature),'None',data_use$MiscFeature)
unique(data_use$MiscFeature)

#Alley - NA means No Alley Access 
data_use$Alley<-ifelse(is.na(data_use$Alley),'NoAlley',data_use$Alley)
unique(data_use$Alley)

#Fence - NA means No Fence
data_use$Fence<-ifelse(is.na(data_use$Fence),'NoFence',data_use$Fence)
unique(data_use$Fence)

#FireplaceQu - NA means No Fireplace
data_use$FireplaceQu <- ifelse(is.na(data_use$FireplaceQu),'NoFireplace',data_use$FireplaceQu)
unique(data_use$FireplaceQu)

#NA means No Garage ####NOT SURE IF THIS IS THE BEST!!!! 
data_use$GarageYrBlt <- ifelse(is.na(data_use$GarageYrBlt),0,data_use$GarageYrBlt)
unique(data_use$GarageYrBlt)

#NA means No Garage 
data_use$GarageType <- ifelse(is.na(data_use$GarageType),'NoGarage',data_use$GarageType)
unique(data_use$GarageType)

data_use$GarageQual <- ifelse(is.na(data_use$GarageQual),'NoGarage',data_use$GarageQual)
unique(data_use$GarageQual)

data_use$GarageFinish <- ifelse(is.na(data_use$GarageFinish),'NoGarage',data_use$GarageFinish)
unique(data_use$GarageFinish)

data_use$GarageCond <- ifelse(is.na(data_use$GarageCond),'NoGarage',data_use$GarageCond)
unique(data_use$GarageCond)

#NA means No Basement 
data_use$BsmtFinType2 <- ifelse(is.na(data_use$BsmtFinType2),'NoBasement',data_use$BsmtFinType2)
unique(data_use$BsmtFinType2)

data_use$BsmtExposure <- ifelse(is.na(data_use$BsmtExposure),'NoBasement',data_use$BsmtExposure)
unique(data_use$BsmtExposure)

data_use$BsmtQual <- ifelse(is.na(data_use$BsmtQual),'NoBasement',data_use$BsmtQual)
unique(data_use$BsmtQual)

data_use$BsmtExposure <- ifelse(is.na(data_use$BsmtExposure),'NoBasement',data_use$BsmtExposure)
unique(data_use$BsmtExposure)

data_use$BsmtFinType1 <- ifelse(is.na(data_use$BsmtFinType1),'NoBasement',data_use$BsmtFinType1)
unique(data_use$BsmtFinType1)

data_use$BsmtCond <- ifelse(is.na(data_use$BsmtCond),'NoBasement',data_use$BsmtCond)
unique(data_use$BsmtCond)

#ACTUAL MISSING
#LotFrontage 
#MasVnrType
#MasVnrArea
#Electrical

#NEW CHECK MISSING PLOT 
missing_df <- data.frame(colname = names(data_use),colSums_missing=colSums(is.na(data_use)))
plot_missing = missing_df %>% filter(colSums_missing!=0) %>% arrange(desc(colSums_missing))
ggplot(plot_missing, aes(x = reorder(colname,colSums_missing), y = colSums_missing)) + 
  geom_col(aes(fill = colname), width = 0.7) + 
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +        
  xlab("") +
  ylab("Number of Missing Observations") 


#LotFrontage 
#related to lotarea
#LotArea
#LotShape
#LotConfig
lotdata = data_use %>% select(LotFrontage,LotArea,LotShape,LotConfig)
test = lotdata %>% filter(!is.na(LotFrontage))

ggplot(test, aes(x=LotArea, y=LotFrontage,group=LotShape)) +
  geom_point(aes(color=LotShape)) +
  xlab('Lot Area') + 
  ylab('Lot Frontage') +
  theme_bw()

model = lm(LotFrontage ~ LotArea, data = test)
summary(model)

# 44.067376 + 0.002688 * LotArea 
ggplot(test, aes(x = LotShape, y = LotFrontage, fill = LotShape)) + geom_boxplot() +
  facet_wrap(~ LotConfig, ncol = 5)

#MasVnrType 
#MasVnrArea 
# MasVnr_summary = data_use %>% 
#   filter(!is.na(MasVnrType), !is.na(MasVnrArea)) %>% 
#   mutate(count=1) %>% 
#   group_by(MasVnrType) %>% 
#   summarise(sum_area=sum(MasVnrArea),tot_count=sum(count))
# MasVnr_summary$average <- MasVnr_summary$sum_area/MasVnr_summary$tot_count

#MASVNR imputation 
#mode imputation 
data_use$MasVnrType.impute <- Hmisc::impute(data_use$MasVnrType, "random")
data_use$MasVnrArea.impute <- Hmisc::impute(data_use$MasVnrArea, median)
data_use$MasVnrType <- NULL
data_use$MasVnrArea <- NULL
data_use$MasVnrType <- data_use$MasVnrType.impute
data_use$MasVnrArea <- data_use$MasVnrArea.impute
data_use$MasVnrArea <- ifelse(MasVnrType=='None',0,data_use$MasVnrArea )

#Electrical - randomly assign missing Electrical value based on distribution 
electric_summ = data_use %>% 
  filter(!is.na(Electrical)) %>% 
  mutate(count=1) %>% 
  group_by(Electrical) %>% 
  summarise(tot_count=sum(count))
#mode imputation 
data_use$Electrical.impute <- Hmisc::impute(data_use$Electrical, mode)
data_use$Electrical <- NULL
data_use$Electrical <- data_use$Electrical.impute

############################################################################################################
#Feature Engineering 

#TotalPorchSF - total porch square footage 
data_use$TotalPorchSF <- data_use$WoodDeckSF +data_use$OpenPorchSF + data_use$EnclosedPorch + data_use$X3SsnPorch + data_use$ScreenPorch
data_use$WoodDeckSF <- NULL 
data_use$OpenPorchSF <- NULL 
data_use$EnclosedPorch <- NULL 
data_use$X3SsnPorch <- NULL 
data_use$ScreenPorch <- NULL

#TotalSF - sum of 1stFlrSF, 2ndFlrSF
#data_use$TotalSF <- data_use$X1stFlrSF + data_use$X2ndFlrSF

#CentralAir_flag as a binary variable
data_use$CentralAir_flag <- ifelse(data_use$CentralAir=="Y",1,0)
data_use$CentralAir <- NULL

data_use$LogSalePrice <- log(data_use$SalePrice)
############################################################################################################
#DATA EXPLORATION 

numeric_data = data_use %>% select(
  LotFrontage, MasVnrArea,
  LotArea, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, LowQualFinSF, GrLivArea,
  BsmtFullBath, BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces,
  GarageCars, GarageArea, TotalPorchSF, PoolArea, MiscVal)

cc = cor(numeric_data, method = "spearman")
corrplot(cc, tl.col = "black", order = "hclust", hclust.method = "average",addrect = 1, tl.cex = 0.5)

ggplot(data_use, aes(x=YearRemodAdd, y=SalePrice)) + 
  geom_line() +
  xlab("Time") + 
  ylab("SalePrice") +
  theme_bw() +
  geom_smooth()

ggplot(data_use, aes(x=YearBuilt, y=SalePrice)) + 
  geom_line() +
  xlab("Time") + 
  ylab("SalePrice") +
  theme_bw() +
  geom_smooth()

ggplot(data_use, aes(x=GarageYrBlt, y=SalePrice)) + 
  geom_line() +
  xlab("Time") + 
  ylab("SalePrice") +
  theme_classic() +
  geom_smooth()


hist(log(data_use$SalePrice), breaks=50, col="#66ffcc")
hist(data_use$SalePrice, breaks=50, col="#66ffcc")

g <- ggplot(data_use, aes(x=SalePrice)) + 
  geom_histogram(bins=200, fill="#66ffcc") + 
  xlab('Sale Price') + 
  ylab('') +
  theme_classic() +
  stat_function(fun=dnorm,
                color="black",
                args=list(mean=mean(data_use$SalePrice), 
                          sd=sd(data_use$SalePrice)))
g

glog <- ggplot(data_use, aes(x=log(SalePrice))) + 
  geom_histogram(bins=200, fill="#66ffcc") + 
  xlab('Log of Sale Price') + 
  ylab('') +
  theme_classic() +
  stat_function(fun=dnorm,
                color="black",
                args=list(mean=mean(log(data_use$SalePrice)), 
                          sd=sd(log(data_use$SalePrice))))
glog




#####################################################################################


#simple linear model 
# num_columns = c('LotFrontage', 'MasVnrArea', 'LotArea', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 
#                 'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'GrLivArea', 'BsmtFullBath', 'BsmtHalfBath', 
#                 'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces',
#                 'GarageCars', 'GarageArea', 'TotalPorchSF', 'PoolArea', 'MiscVal','SalePrice')
# 
# full_linear_model = lm(SalePrice  ~ ., data_use[num_columns])
# vif(full_linear_model)
# library(car)
# influencePlot(full_linear_model)
# 
# vif(full_linear_model)
# 
# avPlots(full_linear_model)
# 
# 
# #Lasso, Ridge - or some combo
# 
# x = model.matrix(SalePrice ~ ., data_use[num_columns])[, -1] #Dropping the intercept column.
# y = data_use$SalePrice
# 
# ridge.models = glmnet(x, y, alpha = 0, lambda = grid)
# 
# lasso.models = glmnet(x, y, alpha = 1, lambda = grid)
# plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")
# 
# 
# #Tree 
# tree.one = tree(LogSalePrice ~ . , data = data_use[num_columns])
# summary(tree.one)
# plot(tree.one)
# text(tree.one, pretty = 0) 
# 
# 
# #RandomForest

#XGBoost 

#Question about overall process
#Question about imputation
#Question about data issues
#Question about multi-collinearity 


