library(ggplot2)
library(dplyr)
library(corrplot)
library(purrr)
library(tidyr)
library(car)

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
  GarageCars, GarageArea, TotalPorchSF, PoolArea, MiscVal, SalePrice)

cc = cor(numeric_data, method = "spearman")
corrplot(cc, tl.col = "black", order = "hclust", hclust.method = "average",addrect = 1, tl.cex = 0.5)

set_1 = data_use %>% select(X2ndFlrSF, HalfBath, BedroomAbvGr, FullBath, GrLivArea, TotRmsAbvGrd, SalePrice)
set_2 = data_use %>% select(GarageCars, GarageArea)

#ScatterPlotMatrix
scatterplotMatrix(set_1)
scatterplotMatrix(set_2)


#Relationship between year built or remodeled and SalePrice
ggplot(data_use, aes(x=YearBuilt, y=SalePrice)) + 
  geom_line() +
  xlab("Time") + 
  ylab("SalePrice") +
  theme_bw() +
  geom_smooth()

ggplot(data_use, aes(x=YearRemodAdd, y=SalePrice)) + 
  geom_line() +
  xlab("Time") + 
  ylab("SalePrice") +
  theme_bw() +
  geom_smooth()

#numeric variables 
data_use %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value, fill='pink')) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() + 
  xlab('') + 
  ylab('') +
  theme_bw()

#character variables
data_use %>%
  keep(is.character) %>% 
  gather() %>% 
  ggplot(aes(value, fill='pink')) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() + 
  xlab('') + 
  ylab('') +
  theme_bw()

#MoSold, YrSold
data_use %>%
  select(MoSold, YrSold) %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value, fill='pink')) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram() + 
  xlab('') + 
  ylab('') +
  theme_bw()

data_use %>%
  select(MoSold,SalePrice) %>% 
  group_by(MoSold) %>% 
  mutate(count=1) %>% 
  summarise(SalePrice = sum(SalePrice)/sum(count)) %>% 
  ggplot(aes(x=MoSold,y=SalePrice)) +
  geom_bar()
  xlab('') + 
  ylab('') 

#Price by Neighborhood
data_use %>%
  select(Neighborhood, LogSalePrice) %>% 
  group_by(Neighborhood) %>% 
  summarise(avg_price=mean(exp(LogSalePrice))) %>% 
  ggplot(aes(x = reorder(Neighborhood,avg_price), y = avg_price)) + 
  geom_col(aes(fill = avg_price), width = 0.7) + 
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +        
  xlab("") +
  ylab("Mean Price by Neighborhoods") 

#Price by HouseStyle
data_use %>%
  select(HouseStyle, SalePrice) %>% 
  group_by(HouseStyle) %>% 
  summarise(avg_price=mean(SalePrice)) %>% 
  ggplot(aes(x = reorder(HouseStyle,avg_price), y = avg_price)) + 
  geom_col(aes(fill = avg_price), width = 0.7) + 
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +        
  xlab("") +
  ylab("Mean Price by House Style") 


#Checking distribution of SalePrice 
g <- ggplot(data_use, aes(x=SalePrice)) + 
  geom_histogram(bins=200, fill="#66ffcc") + 
  xlab('Sale Price') + 
  ylab('') +
  theme_classic() 

g

#checking distribution of log transformation of SalePrice
glog <- ggplot(data_use, aes(x=log(SalePrice))) + 
  geom_histogram(bins=200, fill="#66ffcc") + 
  xlab('Log of Sale Price') + 
  ylab('') +
  theme_classic() 

glog




#####################################################################################

#RF visualization

ggplot(data_use, aes(x=OverallQual, y=SalePrice)) +
  geom_boxplot(aes(group=OverallQual), outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE) + 
  theme_classic()

ggplot(data_use, aes(x=Neighborhood, y=SalePrice)) +
  geom_boxplot(aes(group=OverallQual), outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE) + 
  theme_classic()

ggplot(data_use, aes(x=TotalSF, y=SalePrice)) +
  geom_point() + 
  theme_classic()

# outlier - 

outlierTotalSF = data_use %>% filter(TotalSF>4000,SalePrice<250000)

ggplot(data_use, aes(x=TotalBsmtSF, y=SalePrice)) +
  geom_point() + 
  theme_classic()

ggplot(data_use, aes(x=GrLivArea, y=SalePrice)) +
  geom_point() + 
  theme_classic()


ggplot(data_use, aes(x=YearRemodAdd, y=SalePrice)) +
  geom_boxplot(aes(group=OverallQual), outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE) + 
  theme_classic()


ggplot(data_use, aes(x=OverallQual, y=SalePrice)) +
  geom_point() + 
  theme_classic()



ggplot(data_use, aes(x=Exterior1st, y=Exterior2nd)) +
  geom_point() + 
  theme_classic()
