set.seed(10)
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-(c(1:1000)*runif(1000,min=0,max=2))^2
x3<-log(c(1:1000)*runif(1000,min=0,max=2))

lm_fit<-lm(y~x1+x2+x3)
summary(lm_fit)

set.seed(10)
all_data<-data.frame(y,x1,x2,x3)
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))
train<- all_data[positions,]
test<- all_data[-positions,]

lm_fit<-lm(y~x1+x2+x3,data=train)
predictions<-predict(lm_fit,newdata=test)
error<-sqrt((sum((test$y-predictions)^2))/nrow(test))
error

# single ensemble
# overfitting can still happen with random forest when data is very noisy...
library(randomForest)
rf_fit<-randomForest(y~x1+x2+x3,data=train,ntree=500)
rf_predictions<-predict(rf_fit,newdata=test)
error<-sqrt((sum((test$y-rf_predictions)^2))/nrow(test))
error

# idea of bagging already in randomforest, so...
# try combine models (e.g. randomforest, svm) results with different ratios
library(e1071)
svm_fit<-svm(y~x1+x2+x3,data=train)
svm_predictions<-predict(svm_fit,newdata=test)
error<-sqrt((sum((test$y-svm_predictions)^2))/nrow(test))
error

library(foreach)
bagging_svm<-function(train, test, length_divisor=4, iterations=1000){
  predictions<-foreach(m=1:iterations, .combine=cbind) %do% {
    train_postions<-sample(nrow(train), size=floor((nrow(train)/length_divisor)))
    train_pos<-1:nrow(train) %in% train_positions
    svm_fit<-svm(y~x1+x2+x3, data=train[train_pos,])
    predict(svm_fit,newdata=test)
  }
  rowMeans(predictions)
}
svm2_predictions<-bagging_svm(train,test,length_divisor=6, iterations=5000)
# though result not improved in this case...
# stacking svm could potentially be powerful
error<-sqrt((sum((test$y-svm2_predictions)^2))/nrow(test))
error

# combine svm and randomforests with different rations
predictions<-(svm_predictions+rf_predictions)/2
error<-sqrt((sum((test$y-predictions)^2))/nrow(test))
error

predictions<-(svm_predictions*2+rf_predictions)/3
error<-sqrt((sum((test$y-predictions)^2))/nrow(test))
error
