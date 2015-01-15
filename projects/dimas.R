#Писал для одногрупника, что бы он понял принцип предсказывания временных рядов и было с чего начать в дипломе.


require(dplyr) #для работы с tbl
require(signal) #для сглаживания
require(ggplot2) #для графиков

#считывание данных
table = tbl_df(read.table('/Users/onum/Documents/Универ/4\ курс/1\ семестр/КСКС/log.txt'))
table = table %>% mutate(time =paste(V1,V2),get=V3,out=V6) %>% select(time,get,out) %>% filter(get>=0,out>=0)
table$time = strptime(table$time,"%Y-%m-%d %H:%M:%S")

qplot(table$time,table$get) #до сглаживание

table$get = sgolayfilt(table$get,p=3,n=101) #чем больше n - тем сильнее будет сглаживать, тем больше потеряется.
table$out = sgolayfilt(table$out,p=3,n=101) #но тем меньше будет скачов, которые мешают обучению.

qplot(table$time,table$get) #после сглаживание

#для предстказания нам нужны фичи и предсказуемое значение. Так, как нам нужно предсказать
#кол-во трафика в i момент времени, зная колво в 0..i-1 моменты, мы вибраем в качестве фич
#кол-во трафика в прошлые допустим 3 секунды. Т.е. у нас такая таблица 
# 3 сек назад  |  2 сек назад  | 1 сек назад | ТЕКУЩЕЕ(это нужно будет предказать)
#
#и заполняем эту таблицу историческеми данным. Т.е. в каждой следущей записи значение с 2_сек_назад
# будет прыгать на 3_сек_назад, 1 сек назад на 2, текущее на 1 сек
#Вообщем скользящее окно это.
window.size = 100 # размер окна - 100 секунд. Т.е. по 99 секундам будем предсказывать 1.
n = nrow(table)

features = data.frame(matrix(ncol=0, nrow=n-window.size+1))
for(i in 1:window.size) {
  name = paste(window.size-i,"ago",sep = '_')
  if(i==window.size){name = 'current'}
  features[name]= table$out[i:(n-window.size+i)]
}
features = tbl_df(features)
features # сделали наш список наблюдений(фичи+то, что нужно предсказать)


#делим наши данные на 2 части - 80% для создания модели и 20% для ее проверки
train.ratio <- 0.8
train.rows <- sample.int(nrow(features), floor(nrow(features)*train.ratio))

data.train <- features[train.rows,]
data.train.answers <- data.train$current
data.train <- select(data.train, -current)

data.test <- features[-train.rows,]
data.test.answers <- data.test$current
data.test <- select(data.test, -current)


#создаем модель, которая учится на train(может быть долго)
model.et = extraTrees(data.train,data.train.answers,mtry = 30,ntree=1000, numRandomCuts = 1, numThreads = 2)
predict.et = predict(model.et, data.test) #предсказываем ею данные с тестовой выборке

accuraccy = sum(abs(predict.et - data.test.answers)<1000)/nrow(data.test) #проверяем сколько значение у нас из всех с точностью то 1000 угадались.
#16% у меня. Очень слабо.
#Что можно улучшить?
#Нужно подбирать параметры сглаживания. Взрять среднее по минутам, например.
#Нужно подбирать размер окна.
#Нужно подбирать метод для предскзаания и настраивать его.
#Есть такие подходы RandomForest, ExtraTrees, GBM, SVM, DeepLearning, LASSO и т.д.
#Для настройке есть пакет caret с  методом train, который поможет подобрать параметры для многих из этих.
#Рекомендую пока использовать extraTrees и тюнить параметр mtry




