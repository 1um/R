mycross <- function(x,y,predict_fun,k){
  nums = sample(rep.int(1:k,ceiling(nrow(x)/k)),nrow(x))
  rezult = vector("integer",length=k)
  for(i in 1:k){
    train.features = x[!nums==i,]
    train.answer = y[!nums==i]
    test.features = x[nums==i,]
    test.answer = y[nums==i]
    print(i)
    predict.answer = predict_fun(train.features,train.answer,test.features)
    accuracy = sum(predict.answer==test.answer)/length(test.answer)
    print(accuracy)
    rezult[i] = accuracy
    print(confusionMatrix(predict.answer, test.answer))
  }
  rezult
}