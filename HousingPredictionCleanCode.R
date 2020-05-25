library(Hmisc)
library(kknn)
library(caret)

data_use = read.csv("train.csv", header = T, stringsAsFactors = F)

#splitting into training and testing data sets 
train.index <- createDataPartition(data_use$SalePrice, p=0.8, list=FALSE)
data.train <- data_use[train.index,]
data.test <- data_use[-train.index,]

data_use = data.train

  #imputes NA values that are not actually missing 
changeNA <- function(data_use){
  #PoolQC NA means No Pool
  data_use$PoolQC<-ifelse(is.na(data_use$PoolQC),'NoPool',data_use$PoolQC)
  
  #MiscFeature - NA means None
  data_use$MiscFeature<-ifelse(is.na(data_use$MiscFeature),'None',data_use$MiscFeature)
  
  #Alley - NA means No Alley Access 
  data_use$Alley<-ifelse(is.na(data_use$Alley),'NoAlley',data_use$Alley)
  
  #Fence - NA means No Fence
  data_use$Fence<-ifelse(is.na(data_use$Fence),'NoFence',data_use$Fence)
  
  #FireplaceQu - NA means No Fireplace
  data_use$FireplaceQu <- ifelse(is.na(data_use$FireplaceQu),'NoFireplace',data_use$FireplaceQu)
  
  #NA means No Garage ####NOT SURE IF THIS IS THE BEST!!!! 
  data_use$GarageYrBlt <- ifelse(is.na(data_use$GarageYrBlt),0,data_use$GarageYrBlt)
  
  #NA means No Garage 
  data_use$GarageType <- ifelse(is.na(data_use$GarageType),'NoGarage',data_use$GarageType)
  
  data_use$GarageQual <- ifelse(is.na(data_use$GarageQual),'NoGarage',data_use$GarageQual)
  
  data_use$GarageFinish <- ifelse(is.na(data_use$GarageFinish),'NoGarage',data_use$GarageFinish)

  data_use$GarageCond <- ifelse(is.na(data_use$GarageCond),'NoGarage',data_use$GarageCond)
  
  #NA means No Basement 
  data_use$BsmtFinType2 <- ifelse(is.na(data_use$BsmtFinType2),'NoBasement',data_use$BsmtFinType2)
  
  data_use$BsmtExposure <- ifelse(is.na(data_use$BsmtExposure),'NoBasement',data_use$BsmtExposure)
  
  data_use$BsmtQual <- ifelse(is.na(data_use$BsmtQual),'NoBasement',data_use$BsmtQual)
  
  data_use$BsmtExposure <- ifelse(is.na(data_use$BsmtExposure),'NoBasement',data_use$BsmtExposure)
  
  data_use$BsmtFinType1 <- ifelse(is.na(data_use$BsmtFinType1),'NoBasement',data_use$BsmtFinType1)
  
  data_use$BsmtCond <- ifelse(is.na(data_use$BsmtCond),'NoBasement',data_use$BsmtCond)
  
  return(data_use)
}
  #imputes missing values
imputevalues<-function(data_use){
  
  #LotFrontage imputation - TEMPORARY
  data_use$LotFrontage.impute <- ifelse(is.na(data_use$LotFrontage),
                                 round(44.067376+0.002688*data_use$LotArea,0),data_use$LotFrontage) 
  data_use$LotFrontage <- NULL
  data_use$LotFrontage <- data_use$LotFrontage.impute
  
  #MasVnrType and MasVnrArea imputation
  data_use$MasVnrType.impute <- Hmisc::impute(data_use$MasVnrType, "random")
  data_use$MasVnrArea.impute <- Hmisc::impute(data_use$MasVnrArea, median)
  data_use$MasVnrType <- NULL
  data_use$MasVnrArea <- NULL
  data_use$MasVnrType <- data_use$MasVnrType.impute
  data_use$MasVnrArea <- data_use$MasVnrArea.impute
    
  #Eletrical imputation - mode 
  data_use$Electrical.impute <- Hmisc::impute(data_use$Electrical, mode)
  data_use$Electrical <- NULL
  data_use$Electrical <- data_use$Electrical.impute

  return(data_use)
  
}
  #cleans data 
cleandata<-function(data_use){
  #fix if any Type = None values were imputed with Area
  data_use$MasVnrArea <- ifelse(data_use$MasVnrType=='None',0,data_use$MasVnrArea )
  
  return(data_use)
}
  #creates new features
newfeatures<-function(data_use){
  
  #TotalPorch information
  data_use$TotalPorchSF <- data_use$WoodDeckSF +data_use$OpenPorchSF + data_use$EnclosedPorch + data_use$X3SsnPorch + data_use$ScreenPorch
  data_use$WoodDeckSF <- NULL 
  data_use$OpenPorchSF <- NULL 
  data_use$EnclosedPorch <- NULL 
  data_use$X3SsnPorch <- NULL 
  data_use$ScreenPorch <- NULL
  
  #CentralAir_flag as a binary variable
  data_use$CentralAir_flag <- ifelse(data_use$CentralAir=="Y",1,0)
  data_use$CentralAir <- NULL
  
  #log of SalePrice 
  data_use$LogSalePrice <- log(data_use$SalePrice)
  
  return(data_use)
  
}

data_use = changeNA(data_use)
data_use = imputevalues(data_use)
data_use = cleandata(data_use)
data_use = newfeatures(data_use)


