library(ggplot2)
library(dplyr)
library(corrplot)

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

#run changeNA function

#RE-CHECK MISSING VALUES  
missing_df <- data.frame(colname = names(data_use),colSums_missing=colSums(is.na(data_use)))

#4 columns missing
plot_missing = missing_df %>% filter(colSums_missing!=0) %>% arrange(desc(colSums_missing))
ggplot(plot_missing, aes(x = reorder(colname,colSums_missing), y = colSums_missing)) + 
  geom_col(aes(fill = colname), width = 0.7) + 
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +        
  xlab("") +
  ylab("Number of Missing Observations") 

#run imputevalues function
#run cleandata function 
#run newfeatures function 

###########################################################################################################

#IMPUTING LOT FRONTAGE DATA EXPLORATION

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


############################################################################################################
#DATA EXPLORATION 

numeric_data = data_use %>% select(
  LotFrontage, MasVnrArea,
  LotArea, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, LowQualFinSF, GrLivArea,
  BsmtFullBath, BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces,
  GarageCars, GarageArea, TotalPorchSF, PoolArea, MiscVal)

cc = cor(numeric_data, method = "spearman")
corrplot(cc, tl.col = "black", order = "hclust", hclust.method = "average",addrect = 1, tl.cex = 0.5)


#Relationship between time and SalePrice
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



g <- ggplot(data_use, aes(x=SalePrice)) + 
  geom_histogram(bins=200, fill="#66ffcc") + 
  xlab('Sale Price') + 
  ylab('') +
  theme_classic() 
#   +
#   stat_function(fun=dnorm,
#                 color="black",
#                 args=list(mean=mean(data_use$SalePrice), 
#                           sd=sd(data_use$SalePrice)))
g

glog <- ggplot(data_use, aes(x=log(SalePrice))) + 
  geom_histogram(bins=200, fill="#66ffcc") + 
  xlab('Log of Sale Price') + 
  ylab('') +
  theme_classic() 
  # +
  # stat_function(fun=dnorm,
  #               color="black",
  #               args=list(mean=mean(log(data_use$SalePrice)), 
  #                         sd=sd(log(data_use$SalePrice))))
glog




#####################################################################################



