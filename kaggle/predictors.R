#методы, которыми я пробовал делать предсказания и тюнить.
my_gbm <- function(x,y,x1){
  data.train = mutate(x,wanted=y)
  gbm.cover.form <- as.formula(paste("wanted ~", paste(names(x), collapse=" + ")))
  model = gbm(formula=gbm.cover.form, data = data.train, n.trees=200, distribution="gaussian", cv.folds=5,shrinkage = 0.1,interaction.depth = 5,n.minobsinnode = 10)
  best.cv.iter <- gbm.perf(model, method="cv")
  y1 <- predict(model, newdata = x1, best.cv.iter)
  rez = round(y1)
  rez[rez==8]=7
  rez[rez==0]=1
  confusionMatrix(as.factor(rez),data.test.answers$Cover_Type)
  as.factor(rez)
}

my_et_gbm<- function(x,y,x1){
  cover.et = extraTrees(x, y,mtry = 5,ntree=1000, numRandomCuts = 1, numThreads = 2)
  cover.predict.et = predict(cover.et, x1)
  
  data.train = x %>% mutate(wanted=as.integer(y)-1) %>% filter(wanted == 0 | wanted == 1)
  gbm.form <- as.formula(paste("wanted ~", paste(names(x), collapse=" + ")))
  
  fitControl <- trainControl(method = "cv", number=5, verboseIter=T)
  gbmGrid <-  expand.grid(interaction.depth = c(5,10,40),n.trees = c(1000,2000,3000), shrinkage = 0.01)
  gbmFit1 <- train(gbm.form, data = data.train,method = "gbm",trControl = fitControl, verbose = FALSE, tuneGrid = gbmGrid)
  
  
  model = gbm(formula=gbm.cover.form, data = data.train, n.trees=2000, distribution="bernoulli", cv.folds=5,shrinkage = 0.01,interaction.depth = 20,n.minobsinnode = 10)
  best.cv.iter <- gbm.perf(model, method="cv")
  
  data.test = x1[cover.predict.et %in% c(1,2),]
  y1 <- predict(model, newdata = data.test, best.cv.iter,type="response")
  
#   real12 = as.integer(data.test.answers$Cover_Type[cover.predict.et %in% c(1,2)])
#   comparable = real12 %in% c(1,2)
#   real12 = real12[comparable]
#   et.pred12 = as.integer(cover.predict.et[cover.predict.et %in% c(1,2)])[comparable]
#   gbm.pred12 = (round(y1)+1)[comparable]
#   sum(et.pred12==real12)/length(real12)
#   sum(gbm.pred12==real12)/length(real12)
  
  cover.predict.et[cover.predict.et %in% c(1,2)]=round(y1)+1
  
  cover.predict.et
}

my_et <- function(x,y,x1){
  cover.et = extraTrees(x, y,mtry = 5,ntree=1000, numRandomCuts = 1, numThreads = 2)
  cover.predict.et = predict(cover.et, x1)
}

# tuned <- tune.svm(x,y, gamma = 10^(-1:3), cost = 10^(1:3))
my_svm <- function(x,y,x1){
  model  <- svm(x,y, gamma = 1, cost = 1.6)
  predict(model, x1)
}



my_svm_et <- function(x,y,x1){
  model.et = extraTrees(x, y,mtry = 5,ntree=1000, numRandomCuts = 2, numThreads = 2)
  cover.predict.et = predict(model.et, x)
  new_x <- mutate(x,cover.predict.et = as.integer(cover.predict.et))
  #   tuned <- tune.svm(new_x,y, gamma = c(0.001,0.1,0.3), cost = c(0.001,0.1,0.3))
  model.svm  <- svm(new_x,y, gamma = 0.05, cost = 0.05)
  
  
  cover.predict.et.test = predict(model.et, x1)
  new_x1 = mutate(x1,cover.predict.et = as.integer(cover.predict.et.test))
  y1 = predict(model.svm, new_x1)
}

my_rf <- function(x,y,x1){
  #bestmtry = tuneRF(select(tbl,-Cover_Type),tbl$Cover_Type, ntreeTry=100,stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
  # rf3 <- train(formula(paste("Cover_Type~.")), data = tbl, method = "rf", 
  #             trControl = trainControl(method = "cv", number=5, verboseIter=T),
  #             importance = TRUE, verbose = TRUE, tuneGrid = data.frame(mtry = 20:25),
  #             ntree=500)
  
  # cover.rf = randomForest(Cover_Type~.,data=data.train, mtry=20, ntree=1000,do.trace = 50)
  # cover.predict.rf = predict(cover.rf, data.test.features)
  # 
  # confusionMatrix(cover.predict.rf,data.test.answers$Cover_Type)
}

library(h2o)
my_dl <-function(x,y,x1){
  localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, min_mem_size = '2g',max_mem_size='3g',nthreads = -1)
  data.train = mutate(x,wanted=as.integer(y))
  data.train_h2o <- as.h2o(localH2O, data.train)
  data.test_h2o <- as.h2o(localH2O, x1)
  
  model <- h2o.deeplearning(x = names(x),
                     y = 'wanted',
                     data = data.train_h2o, # data in H2O format
                     activation = "Tanh", # or 'Tanh'
                     input_dropout_ratio = 0.3, # % of inputs dropout
                     hidden = c(20,10),
                     epochs = 2)
  model <- h2o.deeplearning(x = names(x),
                     y = 'wanted',
                     data = data.train_h2o,
#                      nfolds = 3,
                     autoencoder=T,
                     #validation = data.val_h2o,
                     classification = T,
                     activation="RectifierWithDropout",
                     hidden = c(90,90,90),
                     #c(100,100),c(200),c(50,50), c(100)
                     #hidden_dropout_ratios = c(0.0,0.0),
                     input_dropout_ratio = 0,
                     epochs = 100,
                     l1 = 0,
                     l2 = 0, 
                     rho = 0.99, 
                     epsilon = 1e-8, 
#                      train_samples_per_iteration = -2
                  )
#   cvmodel
#   p <- cvmodel@sumtable[[1]]
#   p$prediction_error
#   p$hidden

  predict_h2o <- h2o.predict(model, data.test_h2o)
  predict <- tbl_df(as.data.frame(predict_h2o))
  levels(as.factor(round(predict$reconstr_wanted)))
  y1 = as.factor(predict$predict)
  confusionMatrix(as.factor(y1),data.test.answers$Cover_Type)
  y1
}

