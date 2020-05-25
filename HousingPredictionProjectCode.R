library(caret)
library(car)
library(ggplot2)
library(dplyr)
library(corrplot)
library(Hmisc)

data_use <- read.csv("train.csv", header = T, stringsAsFactors = F)

#splitting into training and testing data sets 
train.index <- createDataPartition(data_use$SalePrice, p=0.8, list=FALSE)
data.train <- data_use[train.index,]
data.test <- data_use[-train.index,]

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

#MasVnrType - randomly assign Masonry Veneer Type based on distribution
#MasVnrArea - randonly assign Masonry Veneer Area based on average area of type 
MasVnr_summary = data_use %>% 
  filter(!is.na(MasVnrType), !is.na(MasVnrArea)) %>% 
  mutate(count=1) %>% 
  group_by(MasVnrType) %>% 
  summarise(sum_area=sum(MasVnrArea),tot_count=sum(count))
MasVnr_summary$average <- MasVnr_summary$sum_area/MasVnr_summary$tot_count

#the following have data issues, should only be zero
#this suggests there are other data issues as well, however, due to time restraints I will leave as is
test=data_use %>% 
  filter(MasVnrType=='None', MasVnrArea!=0)

#randomly assign based on distribution 
MasVnr_summary2 = data_use %>% 
  filter(!is.na(MasVnrType), !is.na(MasVnrArea)) %>% 
  mutate(count=1) %>% 
  group_by(MasVnrType) %>% 
  summarise(sum_area=sum(MasVnrArea),tot_count=sum(count))

MasVnr_summary2$average <- MasVnr_summary2$sum_area/MasVnr_summary2$tot_count

###TO DO 

library(Hmisc)
set.seed(0)
age.meanimpute = Hmisc::impute(titanic3$age, mean)
age.randomimpute = Hmisc::impute(titanic3$age, "random")


#Electrical - randomly assign missing Electrical value based on distribution 
electric_summ = data_use %>% 
  filter(!is.na(Electrical)) %>% 
  mutate(count=1) %>% 
  group_by(Electrical) %>% 
  summarise(tot_count=sum(count))
#mode imputation 
data_use$Electrical <-ifelse(data_use$Electrical==NA,'SBrKr',data_use$Electrical )


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

############################################################################################################
#DATA EXPLORATION 
plot(data_use)

numeric_data = data_use %>% select(
  #LotFrontage, MasVnrArea,
  LotArea, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, LowQualFinSF, GrLivArea,
  BsmtFullBath, BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces,
  GarageCars, GarageArea, TotalPorchSF, PoolArea, MiscVal)

cc = cor(numeric_data, method = "spearman")
corrplot(cc, tl.col = "black", order = "hclust", hclust.method = "average",addrect = 1, tl.cex = 0.5)




#cross-validation vs bootstrapping
#boostrapping decreases the variance 
#it is fairly well-known that the naive bootstrap produces biased estimates.
#http://appliedpredictivemodeling.com/blog/2014/11/27/08ks7leh0zof45zpf5vqe56d1sahb0

#compare k-fold cross validation to bootstrapping


#missingness and imputation

#model trials
#Lasso, Ridge - or some combo
#RandomForest 


#Question about overall process
#Question about imputation
#Question about data issues
#Question about multi-collinearity 


