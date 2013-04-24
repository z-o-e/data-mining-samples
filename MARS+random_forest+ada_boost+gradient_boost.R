

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
#  MARS (earth) 
#
# *****************************

# MARS is in the earth package

library(earth)

# MARS with chd data 

# specifying glm

chd.earth <- earth(chd~., data = CHD$train, glm = list(family = binomial))

# default with a factor predictor 

chd.earth <- earth(as.factor(chd)~., data = CHD$train)

# Results

summary(chd.earth)

# Plots of MARS model selection and fit

plot(chd.earth)

plot.earth.models(chd.earth)


# With CV

summary(earth(as.factor(chd)~., data = CHD$train, nfold = 10))

# Measuring variable importance

chd.mars.imp<- evimp(chd.earth)

plot(chd.mars.imp)

# MARS Plots

plotmo(chd.earth)

plotd(chd.earth, xlim = c(0, 1), main = "Distribution Plot")

# prediction with test data 

chd.earth.pred <- predict(chd.earth, newdata = CHD$test, type = "response")

summary(chd.earth.pred)



#********************************************************
#
#			Random Forests
#
#********************************************************

# Two packages for RF: randomForest and Party

library(randomForest)
 
chd.rf = randomForest(as.factor(chd) ~., data = CHD$train, ntree = 1000, importance = T, proximity =T)       

# list of outputs

summary(chd.rf)

 # Trace
 
 plot(chd.rf, main = "CHD Random Forest") 
 
 # MDS plot 
 
 MDSplot(chd.rf, factor(CHD$train[,10]), main = "MDS Plot CHD")
 
 # Variable importance
 
 varImpPlot(chd.rf, main = "Variable Importance for CHD RF")
 
 importance(chd.rf)
 
  # Prediction
 
  chd.rf.pred = predict(chd.rf, newdata = CHD$test, type = "prob")


#**************************************
#
#		Party RF
#
#**************************************

library(party)

cforest_control()

chd.cf <- cforest(as.factor(chd)~., data = CHD$train)

# Variable importance

varimp(chd.cf)

barplot(varimp(chd.cf), main = "Variable Importance from Party RF")

# prediction

chd.cf.p <- treeresponse(chd.cf, newdata = CHD$test)

chd.cf.pred <- sapply(chd.cf.p, "[", i = 1)


#**************************************
#
#		Adaboost
#
#**************************************

# In ada package

# Note: you cannot run ada with mvpart. You will get an error about na.action.
# If you have it loaded, us detach("package:mvpart", unload = T)
# This will let you continue working withou restarting.

library(ada)


dc <- rpart.control()
chd.ada <-ada(chd~.,data=CHD$train,iter=50,loss="ada",type="discrete", control= dc)

# Results

chd.ada

summary(chd.ada)

# Boosting trace

plot(chd.ada, TRUE, TRUE)

# Variable importance

varplot(chd.ada)

# Prediction 

chd.ada.pred <- predict(chd.ada, newdata = CHD$test, type = "probs")
 
 
#**************************************
#
#		Gradient Boosting
#
#**************************************

# Two packages for gradient boosting: gbm and mboost

library(gbm)

chd.gbm <- gbm(chd~., data = CHD$train, n.trees = 100, shrinkage = 1)

# Variable importance

summary(chd.gbm)

# 2 variable plot
plot(chd.gbm, i.var = c(2,9))


# Prediction

chd.gbm.pred <- predict(chd.gbm, newdata = chd.test, n.trees = 100, type = "response")

summary(chd.gbm.pred)


#**************************************
#
#		Gradient Boosting with mboost
#
#**************************************


library(mboost)

chd.mboost <- mboost(chd~., data = chd.train,  baselearner = "btree")

# Results

summary(chd.mboost)

# Var Imp

dotchart((summary(chd.mboost))$selprob, main = "Mboost Selection Probabilities, 100 Iterations")

# Plot
par(mfrow = c(3,3))
plot(chd.mboost)
par(mfrow = c(1,1))

# Prediction

chd.mboost.pred <- predict(chd.mboost, newdata = chd.test, type = "response")



