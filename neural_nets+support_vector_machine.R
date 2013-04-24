

#**************************************************************************************
#
#			CHD Data Set
#
#**************************************************************************************

# upload the chd data

chd <- read.table("CHDdata.csv", sep = ",", header =T)

summary(chd)

# Create a test set with 1/3 the observervations (default)


source("TestSet.R")

CHD <- test.set(chd)




#******************************
#
#  Neural Nets
#
# *****************************

#Load the nnets package

library(nnet)


#First we do 5 hidden nodes

chd.nn1 = nnet(chd ~., data = chd.train, size = 5, decay=1e-3,  maxit = 1000)

#The weights on the neural network

summary(chd.nn1)


# Plot of training set performance

plot(chd.nn1$fit~as.factor(chd.train$chd), pch = 21, main = "NN 5 Training Results for CHD Data", xlab = "Class", ylab = "Posterior", col = "steelblue" )

# Test set predictions

chd.nn1.pred <- predict(chd.nn1, newdata = chd.test, type = "raw")

plot(chd.nn1.pred~as.factor(chd.test$chd), pch = 21, main = "NN 5 Test Set Results for CHD Data", xlab = "Class", ylab = "Posterior", col = "steelblue" )

# Confusion matrix for test set

score.table(chd.nn1.pred, chd.test$chd)

     
# 20 nodes in the hidden layer

chd.nn2 = nnet(chd ~., data = chd.train, size = 20, decay=1e-3,  maxit = 1000)





#**********************************************************
#
#			SVM
#
#*********************************************************

#  Two packages for SVM

library(e1071)
library(svmpath)

# We will use e1071

#		Training set model




# Radial Basis - the default

chd.svm1 <- svm(as.factor(chd)~., data = chd.train,  cost = 100, gamma = 1, probability = TRUE)

# Plot of training set performance

chd.svm1.fit <- predict(chd.svm1, chd.train, probability = T)

chd.svm.fit <- attr(chd.svm1.fit, "probabilities")


plot(chd.svm.fit[,1]~as.factor(chd.train$chd), pch = 21, main = "SVM RBF Training Results for CHD Data", xlab = "Class", ylab = "Posterior", col = "steelblue" )


# Test set fit

svm1.pred <- predict(chd.svm1, newdata = chd.test, decision.values = T, probability = TRUE)

svm1.pr <- attr(svm1.pred, "probabilities")

plot(svm1.pr[,1]~as.factor(chd.test$chd), pch = 21, main = "SVM RBF Test Set Results for CHD Data", xlab = "Class", ylab = "Posterior", col = "steelblue" )

# Confusion matrix for test set

score.table(svm1.pr[,1], chd.test$chd)


# Linear degree 1 

chd.svml <- svm(as.factor(chd)~., kernel = "linear", data = chd.train, cost = 100, gamma = 1, probability = TRUE)

# Polynomial degree 3 

chd.svmp <- svm(as.factor(chd)~., kernel = "polynomial", degree = 3,  data = chd.train, cost = 10, gamma = 1, probability = TRUE)

# Sigmoid

chd.svmsig <- svm(as.factor(chd)~., data = chd.train, probability = TRUE,  cost = 100, gamma = 1, kernal = "sigmoid")







