#cleaning the data enviroment
rm(list=ls())

#loading the data
setwd("C:/Users/Omer Gralnik/Desktop/Business Analysis/Datasets")
getwd() #verifying that the work directory is correct
list.files()
kc <- read.csv("kc_house.csv", header = T)

#checking the data variables and a summary of the data
names(kc)
summary(kc)
str(kc)


#performing summary statistics
quantile(kc$bathrooms)
prop.table(table(kc$floors))
by(kc$price,kc$bedrooms, mean)
prop.table(table(kc$condition))
quantile(kc$grade,probs = seq(0,1,0.1))

#converting the relevant numeric variables to categorial ones
kc$waterfront <- as.factor(kc$waterfront)
kc$floors <- as.factor(kc$floors)
kc$condition <- as.factor(kc$condition)

str(kc)

#install.packages("stargazer")
library(stargazer)

#generating a summary stat table
#stargazer(kc, out = 'housesSummary.html') #for the first time using the code take of the '#' mark



#the variables with missing values are - sqft_living,condition,yr_built

#Replacing missing values in sqft_living with their predicted value
options(digits = 4)
kc$sqft_living <- ifelse(is.na(kc$sqft_living),predict(lm(sqft_living~sqft_above,data = kc)),kc$sqft_living)

#creating a mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Replace missing values in condition with the mode of this variable 
kc$condition <- ifelse(is.na(kc$condition),Mode(kc$condition),kc$condition)

#Replace missing values in yr_bulit with the mode of yr_built
kc$yr_built <- ifelse(is.na(kc$yr_built), Mode(kc$yr_built),kc$yr_built)

#checking the new data's summary
summary(kc)


# Splitting the data to 70% Training and 30% Testing
set.seed(123)

sampleSize <-  floor( 0.7 * nrow(kc) )


train.index = sample(nrow(kc), size = sampleSize, replace = F )  

train <-  kc[train.index, ]
test <-  kc[-train.index, ]

#loading relevant packages
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#creating the decision tree for price 
tree <-  rpart(price ~ bathrooms+bedrooms+sqft_living+grade+sqft_basement+yr_built+floors+condition,
             data = train,
             parms = list(split = "information"),
             control = rpart.control(maxdepth = 5))
fancyRpartPlot(tree)

#generate a predict variable to the test dataset
test$price_pred <-  predict(tree, newdata = test,type = 'vector')

#calculating mae
mae_rpart <- abs(sum(test$price-test$price_pred))/nrow(test)
#mae = 3667

#prediction using linear model
model1 <- lm(price ~ bathrooms+bedrooms+sqft_living+grade+sqft_basement+yr_built+floors+condition,
             data = train)
summary(model1)
#stargazer(model1, out = 'lm_prediction.html',title = 'Linear regression Summary',
#          no.space = T,single.row = T,digits = 2)

#generating a predict variable to the test data
test$price_pred_lm <- predict(model1, newdata = test)

#calculating mae
mae_lm <- abs(sum(test$price-test$price_pred_lm))/nrow(test)
#mae = 3277

#Cross validation
Shuffle = kc[sample(nrow(kc),replace=F),  ]

#creating 20 folds vector
folds = cut( 1:nrow(Shuffle), 
             breaks = 20, labels=F)
summary(factor(folds))

MAELm = rep(0,20)
MAETree = rep(0,20)

#performing 20 folds cross validation
for (i in 1:20) {
  #i = 1
  testIndexes = which(folds == i)
  
  testData = Shuffle[testIndexes, ]
  trainData = Shuffle[-testIndexes, ]
  
  #linear model
  linear_model = lm(price ~ bathrooms+bedrooms+sqft_living+grade+
                      sqft_basement+yr_built+floors+condition,data = trainData)
  testData$lmPredictions = predict(linear_model, newdata = testData)
  MAELm[i] = abs(sum(testData$price-testData$lmPredictions))/nrow(testData)
  
  #tree
  tree1 = rpart(price ~ bathrooms+bedrooms+sqft_living+grade+sqft_basement+yr_built+floors+condition,
                data = trainData,
                parms = list(split = "information"),
                control = rpart.control(maxdepth = 5))
  testData$treePrediction = predict(tree, newdata = testData,type = 'vector')
  MAETree[i] = abs(sum(testData$price-testData$treePrediction))/nrow(testData)
}

#checking summary and sd for each MAE
summary(MAELm)
sd(MAELm)
summary(MAETree)
sd(MAETree)















