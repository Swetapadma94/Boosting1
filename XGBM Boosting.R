require(xgboost)
require(methods)
library(xgboost)

data(iris)

datagbm<-iris

mean<-c()
for(i in 1:25){
  
  inTraining <- createDataPartition(datagbm$Species, p = .8, list = FALSE)
  training <- datagbm[ inTraining,]
  testing  <- datagbm[-inTraining,]
  
  
  labeltraining = as.numeric(training[[5]])
  datatraining = as.matrix(training[1:4])
  
  labeltesting  = as.numeric(testing [[5]])
  datatesting  = as.matrix(testing [1:4])
  
  xgtraining <- xgb.DMatrix(data=datatraining, label = labeltraining)
  
  xgtesting <- xgb.DMatrix(data=datatesting, label = labeltesting)
  
  param = list("objective" = "multi:softmax", "bst:eta" = 0.005,"bst:max_depth" = 4,"num_class"=4,"nthread" = 4,"gamma" =0.5,"min_child_weight" = 3)
  ?xgboost
  
  model = xgboost(params = param, data = xgtraining, nround = 1000)#,subsample = 0.8,colsample_bytree = 0.8)
  
  
  ypred = predict(model, xgtesting)
  
  #mean<-mean(labeltesting==ypred)
  
  mean <- c(mean,mean(labeltesting==ypred) )
  
}

summary(mean)
