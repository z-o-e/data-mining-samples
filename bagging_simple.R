set.seed(10)
# y: a sequence of values 1~1000
# x1, x2, x3: permutations of y with random errors added
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-c(1:1000)*runif(1000,min=0,max=2)
x3<-c(1:1000)*runif(1000,min=0,max=2)

lm_fit<-lm(y~x1+x2+x3)
# single model Rsquared .7042 
summary(lm_fit)

set.seed(10)
all_data<-data.frame(y,x1,x2,x3)
# randomly pick out ~3/4 of all data indexes
positions<-sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))
train<-all_data[positions,]
test<-all_data[-positions,]

lm_fit<-lm(y~x1+x2+x3,data=train)
predictions<-predict(lm_fit, newdata=test)
error<-sqrt((sum((test$y-predictions)^2))/nrow(test))

library(foreach)

# an example: taking 1/4 of the training set in each iteration,
# generate predictions for testing set based on sample
length_divisor<-4
iterations<-1000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  train_positions<-sample(nrow(train), size=floor((nrow(train)/length_divisor)))
  train_pos<-1:nrow(train) %in% train_positions
  lm_fit<-lm(y~x1+x2+x3, data=train[train_pos,])
  predict(lm_fit,newdata=test)
}
predictions<-rowMeans(predictions)
error<-sqrt((sum((test$y-predictions)^2))/nrow(test))

# wrap up as a function
bagging<-function(train, test, length_divisor=4, iterations=1000){
  predictions<-foreach(m=1:iterations, .combine=cbind) %do% {
    train_postions<-sample(nrow(train), size=floor((nrow(train)/length_divisor)))
    train_pos<-1:nrow(train) %in% train_positions
    lm_fit<-lm(y~x1+x2+x3, data=train[train_pos,])
    predict(lm_fit,newdata=test)
  }
  rowMeans(predictions)
}

