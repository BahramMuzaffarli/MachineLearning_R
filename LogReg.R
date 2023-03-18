getwd()
data = read_csv("Downloads/heart.csv")
setwd("C:/Users/FX505DT")
library(readr)
View(data)

head(data)

#Covert binary types data to characterical type
data$sex[data$sex==1] = 'F'
data$sex[data$sex==2] = 'M'

data$target[data$target==0] = "Healthy"
data$target[data$target==1] = "Unhealthy"


str(data)

#Convert Data Type
data$sex = as.factor(data$sex)
data$target = as.factor(data$target)
data$fbs = as.factor(data$fbs)
data$restecg = as.factor(data$restecg)
data$exang = as.factor(data$exang)
data$slope = as.factor(data$slope)

#Count of NA 
sapply(data,function(x) sum(is.na(x)))

nrow(data)


xtabs(~target+sex,data = data)

library(corrplot)
cor = cor(data[,c(1,4,5,8,10)])
corrplot(cor,method='circle')


pairs(data,col=data$target)


#Logistic Regression Analysis

install.packages("caret")
library(caret)

set.seed(100)

#datani test ve train datalara ayirmaq 20/80
trainIndex = createDataPartition(y=data$target,p=0.80,
                                 list=FALSE)
train_data = data[trainIndex,]
test_data = data[-trainIndex,]


nrow(train_data)
nrow(test_data)


#logistic regression
lr_model = glm(target~.,data = data, family = binomial)
summary(lr_model)


lr_probability = predict(lr_model,newdata = test_data,
                     type="response")
head(lr_probability)

lr_predict = ifelse(lr_probability>0.5,"Unhealthy","Healthy")
head(lr_predict)

mean(lr_predict == test_data$target)



#train
lr_probability_train = predict(lr_model,newdata = train_data,
                     type="response")
head(lr_probability_train)

lr_predict_train = ifelse(lr_probability_train>0.5,"Unhealthy","Healthy")
head(lr_predict_train)

mean(lr_predict_train == train_data$target)

