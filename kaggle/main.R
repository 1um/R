#Задачи про тип почвы на kaggle
#В этом файле - делится тестовый и треин датасет. 
#Подготовливаются данные
#Делается оценка
#Сохраняется предикт.

library(dplyr)
library(tidyr)
library(randomForest)
 library(caret)
options( java.parameters = "-Xmx7g")
library(extraTrees)
library(e1071)
library(gbm)
set.seed(123)
setwd("~/Documents/prog/coursera/course3/forest")

data.train = tbl_df(read.csv('train.csv'))

# data.train = data.train %>% filter( Cover_Type == 1 | Cover_Type == 2) %>% mutate(Cover_Type = Cover_Type-1)
             
data.train$Cover_Type = as.factor(data.train$Cover_Type)

data.test = tbl_df(read.csv('test.csv'))

train.ratio <- 0.8
train.rows <- sample.int(nrow(data.train), floor(nrow(data.train)*train.ratio))
data.test <- data.train[-train.rows,]
data.train <- data.train[train.rows,]

data.test.answers <- select(data.test, Cover_Type)
data.test <- select(data.test, -Cover_Type)



data.train.clear = clear_d(data.train)
data.test.clear = clear_d(data.test)

x = select(data.train.clear, -Cover_Type)
y = data.train.clear$Cover_Type
x1 = clear_d(data.test)



result = mycross(x,y,my_gbm,5)
mean(result)
sd(result)

cover.predict = my_et_gbm(x,y,data.test.clear)
predicted = mutate( data.test,Cover_Type = cover.predict)
predicted = select(predicted, Id, Cover_Type)
write.csv(predicted,'predicted.csv',row.names = F)
tbl_df(read.csv('predicted.csv'))
 
