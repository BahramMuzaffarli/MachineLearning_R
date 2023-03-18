#Regression Analsis
install.packages("datarium")
data("marketing",package = "datarium")

head(marketing)

str(marketing)

#for checking null value
install.packages('Amelia')
library(Amelia)

missmap(marketing) #no missing data

library(corrplot)
cor = cor(marketing)
corrplot(cor,method='circle')

#Data Preparing
library(caret)
set.seed(100)
trainIndex = createDataPartition(marketing$sales, p = .8,
                                 list=FALSE)

trainData = marketing[trainIndex,]
testData = marketing[-trainIndex,]

nrow(trainIndex)
nrow(testData)

#Simple linear regression
model = lm(sales~facebook, data = marketing)
summary(model)
summary(model)$coef


new_data = data.frame(facebook=c(0,1000))
predict(model,new_data)

#Linear Regression
model = lm(sales~youtube+facebook+newspaper,
           data = trainData)

summary(model)$coef
summary(model)

model2 = lm(sales~youtube+facebook,data = trainData)

#model evaluation
1.922/mean(marketing$sales) #Residual standard error = 1.922


predict = predict(model2,testData)

head(predict)

RMSE(predict,testData$sales)

2.38/mean(testData$sales) #modelin hata orani

R2(predict, testData$sales)


