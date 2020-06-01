library(Hmisc)
library(kknn)
library(caret)

data_use = read.csv("train.csv", header = T, stringsAsFactors = F)
data_test = read.csv("test.csv", header = T, stringsAsFactors = F)

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
  data_use$LotFrontage.impute <- NULL
  
  #MasVnrType and MasVnrArea imputation
  data_use$MasVnrType.impute <- Hmisc::impute(data_use$MasVnrType, "random")
  data_use$MasVnrArea.impute <- Hmisc::impute(data_use$MasVnrArea, median)
  data_use$MasVnrType <- NULL
  data_use$MasVnrArea <- NULL
  data_use$MasVnrType <- data_use$MasVnrType.impute
  data_use$MasVnrArea <- data_use$MasVnrArea.impute
  data_use$MasVnrType.impute <- NULL
  data_use$MasVnrArea.impute <- NULL
    
  #Eletrical imputation - mode 
  data_use$Electrical.impute <- Hmisc::impute(data_use$Electrical, mode)
  data_use$Electrical <- NULL
  data_use$Electrical <- data_use$Electrical.impute
  data_use$Electrical.impute <- NULL

  return(data_use)
  
}
  #cleans data 
cleandata<-function(data_use){
  #fix if any Type = None values were imputed with Area
  data_use$MasVnrArea <- ifelse(data_use$MasVnrType=='None',0,data_use$MasVnrArea )
  
  data_use$HouseStyle <- ifelse(data_use$HouseStyle=='2.5Unf','2.5Story', data_use$HouseStyle)
  data_use$HouseStyle <- ifelse(data_use$HouseStyle=='2.5Fin','2.5Story', data_use$HouseStyle)
  
  data_use$Electrical <- ifelse(data_use$Electrical == 'FuseP','Other', data_use$Electrical)
  data_use$Electrical <- ifelse(data_use$Electrical == 'Mix','Other', data_use$Electrical)  
  
  #too much overlap with Garage Area
  data_use$GarageCars <- NULL
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
  
  #Fence_flag as a binary variable
  data_use$Fence_flag <- ifelse(data_use$Fence=="NoFence",0,1)
  data_use$Fence <- NULL
  
  #Pool_flag as a binary variable
  data_use$Pool_flag <- ifelse(data_use$PoolQC=="NoPool",0,1)
  data_use$PoolQC <- NULL
  
  #Paved_flag as a binary variable
  data_use$Paved_flag <- ifelse(data_use$PavedDrive=="N",0,1)
  data_use$PavedDrive <- NULL
  
  #Cond2Norm_flag as a binary variable
  data_use$Cond2Norm_flag <- ifelse(data_use$Condition2!="Norm",0,1)
  data_use$Condition2 <- NULL
  
  #MiscFeatures flag
  data_use$MiscFeatures <- ifelse(data_use$MiscFeature=="None",'N','Y')
  data_use$MiscFeature <- NULL
  
  #StandardRoof
  data_use$StandardRoof <- ifelse(data_use$RoofMatl!="CompShg",'N','Y')
  data_use$RoofMatl <- NULL
  
  #GarageCond_num as numeric variable
  data_use$GarageCond_flag <- ifelse(data_use$GarageCond!="NoGarage",
                                     ifelse(data_use$GarageCond!="Po",
                                            ifelse(data_use$GarageCond!="Fa",
                                                   ifelse(data_use$GarageCond!='TA',
                                                          ifelse(data_use$GarageCond!='Gd',5,4),3),2),1),0)
  data_use$GarageCond <- NULL
  
  #GarageQual_num as numeric variable
  data_use$GarageQual_flag <- ifelse(data_use$GarageQual!="NoGarage",
                                     ifelse(data_use$GarageQual!="Po",
                                            ifelse(data_use$GarageQual!="Fa",
                                                   ifelse(data_use$GarageQual!='TA',
                                                          ifelse(data_use$GarageQual!='Gd',5,4),3),2),1),0)
  data_use$GarageQual <- NULL
  
  #GarageQual_num as numeric variable
  data_use$FireplaceQu_flag <- ifelse(data_use$FireplaceQu!="NoFireplace",
                                     ifelse(data_use$FireplaceQu!="Po",
                                            ifelse(data_use$FireplaceQu!="Fa",
                                                   ifelse(data_use$FireplaceQu!='TA',
                                                          ifelse(data_use$FireplaceQu!='Gd',5,4),3),2),1),0)
  data_use$FireplaceQu <- NULL
  
  #KitchenQual_flag as numeric variable
  data_use$KitchenQual_flag <- ifelse(data_use$KitchenQual!="Po",
                                              ifelse(data_use$KitchenQual!="Fa",
                                                     ifelse(data_use$KitchenQual!='TA',
                                                            ifelse(data_use$KitchenQual!='Gd', 5,4),3),2),1)
  data_use$KitchenQual <- NULL
  
  #HeatingQC_flag as numeric variable
  data_use$HeatingQC_flag <- ifelse(data_use$HeatingQC!="Po",
                                      ifelse(data_use$HeatingQC!="Fa",
                                             ifelse(data_use$HeatingQC!='TA',
                                                    ifelse(data_use$HeatingQC!='Gd', 5,4),3),2),1)
  data_use$HeatingQC <- NULL
  
  #HeatingGASA - binary flag, Gas forced warm air furnace 
  data_use$HeatingGASA_flag <- ifelse(data_use$Heating!="GasA",1,0)
  data_use$Heating <- NULL
  
  #BsmtExposure_flag as numeric variable
  data_use$BsmtExposure_flag <- ifelse(data_use$BsmtExposure!="NoBasement",
                                    ifelse(data_use$BsmtExposure!="No",
                                           ifelse(data_use$BsmtExposure!='Mn',
                                                  ifelse(data_use$BsmtExposure!='Av', 4,3),2),1),0)
  data_use$BsmtExposure <- NULL
  
  #BsmtCond_num as numeric variable
  data_use$BsmtCond_flag <- ifelse(data_use$BsmtCond!="NoBasement",
                                      ifelse(data_use$BsmtCond!="Po",
                                             ifelse(data_use$BsmtCond!="Fa",
                                                    ifelse(data_use$BsmtCond!='TA',
                                                           ifelse(data_use$BsmtCond!='Gd',5,4),3),2),1),0)
  data_use$BsmtCond <- NULL
  
  #BsmtQual_num as numeric variable (evaluates height of basement)
  data_use$BsmtQual_flag <- ifelse(data_use$BsmtQual!="NoBasement",
                                   ifelse(data_use$BsmtQual!="Po",
                                          ifelse(data_use$BsmtQual!="Fa",
                                                 ifelse(data_use$BsmtQual!='TA',
                                                        ifelse(data_use$BsmtQual!='Gd',5,4),3),2),1),0)
  data_use$BsmtQual <- NULL
  
  #ExterCond_flag as numeric variable
  data_use$ExterCond_flag <- ifelse(data_use$ExterCond!="Po",
                                    ifelse(data_use$ExterCond!="Fa",
                                           ifelse(data_use$ExterCond!='TA',
                                                  ifelse(data_use$ExterCond!='Gd', 5,4),3),2),1)
  data_use$ExterCond <- NULL
  
  #ExterQual_flag as numeric variable
  data_use$ExterQual_flag <- ifelse(data_use$ExterQual!="Po",
                                    ifelse(data_use$ExterQual!="Fa",
                                           ifelse(data_use$ExterQual!='TA',
                                                  ifelse(data_use$ExterQual!='Gd', 5,4),3),2),1)
  data_use$ExterQual <- NULL
  
  
  #MonthYear sold variable 
  #data_use$MoYrSold <- paste(data_use$YrSold,"-",data_use$MoSold,sep='')
  #data_use$MoSold <- NULL
  #data_use$YrSold <- NULL
  
  #Total number of baths
  data_use$TotalBath <- data_use$FullBath + .5* data_use$HalfBath + data_use$BsmtFullBath + .5*data_use$BsmtHalfBath
  data_use$FullBath <- NULL
  data_use$HalfBath <- NULL
  data_use$BsmtFullBath <- NULL
  data_use$BsmtHalfBath <- NULL 
  
  data_use$MSSubClass <- NULL
  data_use$TotalSF <- data_use$X1stFlrSF + data_use$X2ndFlrSF
  data_use$NumFloors <- ifelse(data_use$X2ndFlrSF==0,1,2)
  data_use$X1stFlrSF <- NULL
  data_use$X2ndFlrSF <- NULL
  
  return(data_use)
  
}
  #adding logsaleprice 
logsaleprice <- function(data_use){
  
  #log of SalePrice 
  data_use$LogSalePrice <- log(data_use$SalePrice)
  
  return(data_use)
}
  #imputing values missing on test data 
imputetest <- function(data){
   
  data$MSZoning.impute <- Hmisc::impute(data$MSZoning, mode)
  data$MSZoning <- NULL
  data$MSZoning <- data$MSZoning.impute
  data$MSZoning.impute <- NULL
  data$MSZoning <- ifelse(data$MSZoning=="character",'RL',data$MSZoning)
  
  data$Utilities.impute <- Hmisc::impute(data$Utilities, mode)
  data$Utilities <- NULL
  data$Utilities <- data$Utilities.impute
  data$Utilities.impute <- NULL
  
  data$Functional.impute <- Hmisc::impute(data$Functional, mode)
  data$Functional <- NULL
  data$Functional <- data$Functional.impute
  data$Functional.impute <- NULL
  
  data$BsmtHalfBath.impute <- Hmisc::impute(data$BsmtHalfBath, median)
  data$BsmtHalfBath <- NULL
  data$BsmtHalfBath <- data$BsmtHalfBath.impute
  data$BsmtHalfBath.impute <- NULL
  
  data$BsmtFullBath.impute <- Hmisc::impute(data$BsmtFullBath, median)
  data$BsmtFullBath <- NULL
  data$BsmtFullBath <- data$BsmtFullBath.impute
  data$BsmtFullBath.impute <- NULL
  
  data$TotalBsmtSF.impute <- Hmisc::impute(data$TotalBsmtSF, median)
  data$TotalBsmtSF <- NULL
  data$TotalBsmtSF <- data$TotalBsmtSF.impute
  data$TotalBsmtSF.impute <- NULL
  
  data$SaleType.impute <- Hmisc::impute(data$SaleType, mode)
  data$SaleType <- NULL
  data$SaleType <- data$SaleType.impute
  data$SaleType.impute <- NULL
  
  data$KitchenQual.impute <- Hmisc::impute(data$KitchenQual, mode)
  data$KitchenQual <- NULL
  data$KitchenQual <- data$KitchenQual.impute
  data$KitchenQual.impute <- NULL
  
  data$GarageCars.impute <- Hmisc::impute(data$GarageCars, median)
  data$GarageCars <- NULL
  data$GarageCars <- data$GarageCars.impute
  data$GarageCars.impute <- NULL
  
  data$GarageArea.impute <- Hmisc::impute(data$GarageArea, median)
  data$GarageArea <- NULL
  data$GarageArea <- data$GarageArea.impute
  data$GarageArea.impute <- NULL
  
  data$Exterior2nd.impute <- Hmisc::impute(data$Exterior2nd, mode)
  data$Exterior2nd <- NULL
  data$Exterior2nd <- data$Exterior2nd.impute
  data$Exterior2nd.impute <- NULL
  
  data$Exterior1st.impute <- Hmisc::impute(data$Exterior1st, mode)
  data$Exterior1st <- NULL
  data$Exterior1st <- data$Exterior1st.impute
  data$Exterior1st.impute <- NULL
  
  data$BsmtUnfSF.impute <- Hmisc::impute(data$BsmtUnfSF, median)
  data$BsmtUnfSF <- NULL
  data$BsmtUnfSF <- data$BsmtUnfSF.impute
  data$BsmtUnfSF.impute <- NULL
  
  data$BsmtFinSF2.impute <- Hmisc::impute(data$BsmtFinSF2, median)
  data$BsmtFinSF2 <- NULL
  data$BsmtFinSF2 <- data$BsmtFinSF2.impute
  data$BsmtFinSF2.impute <- NULL
  
  data$BsmtFinSF1.impute <- Hmisc::impute(data$BsmtFinSF1, median)
  data$BsmtFinSF1 <- NULL
  data$BsmtFinSF1 <- data$BsmtFinSF1.impute
  data$BsmtFinSF1.impute <- NULL
  
  return(data)
}

#fix issues on imputation
fixdata <- function(data){
  data$Functional <- ifelse(data$Functional=="character",'Typ',data$Functional)
  data$SaleType <- ifelse(data$SaleType=="character",'WD',data$SaleType)
  data$Electrical <- ifelse(data$Electrical=="character",'SBrkr',data$Electrical)
  
  data$Exterior1st <- ifelse(data$Exterior1st=="character",'VinylSd',data$Exterior1st)
  
  data$Exterior2nd <- ifelse(data$Exterior2nd=="character",'VinylSd',data$Exterior2nd)
  data$Exterior2nd <- ifelse(data$Exterior2nd=="Other",'VinylSd',data$Exterior2nd)
  
  data$Utilities <- ifelse(data$Utilities=="character",'AllPub',data$Utilities)
  #data$KitchenQual <- ifelse(is.na(data$KithenQual), 'TA', data$KitchenQual)
  return(data)
}

removeobs <- function(data){
  #BEST TO REMOVE THESE 
  data$Exterior1st = ifelse(data$Exterior1st=="ImStucc",'VinylSd',data$Exterior1st)
  data$Exterior1st = ifelse(data$Exterior1st=="Stone",'VinylSd',data$Exterior1st) 
  
  data = data %>% filter(Id!=524,Id!=1299)
  
  return(data)
}

data_use = changeNA(data_use)
data_use = imputevalues(data_use)
data_use = fixdata(data_use)
data_use = cleandata(data_use)
data_use = newfeatures(data_use)
data_use = logsaleprice(data_use)
data_use = removeobs(data_use)

#data_test = fixtest(data_test)
data_test = changeNA(data_test)
data_test = imputetest(data_test)
data_test = fixdata(data_test)
data_test = imputevalues(data_test)
data_test = cleandata(data_test)
data_test = newfeatures(data_test)
#data_test = imputetest(data_test)


