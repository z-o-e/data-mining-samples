

#**************************************************************************************
#
#			Linear Models, Discriminants, and Additive Models 
#
#**************************************************************************************

# Code for evaluation

source("ROC.R")

# Code for test sets

source("TestSet.R")


#**************************************************************************************
#
#			CHD Data Set
#
#**************************************************************************************


chd <- read.table("CHDdata.csv", sep = ",", header =T)

summary(chd)


#**************************************
#
#		TEST SET
#
#**************************************


# Default with test.set() is to select 1/3 for testing

CHD <- test.set(chd)

# The training set is CHD$train and the test set is CHD$test

# comparing quantitative variables for the 3 data sets

par(mfrow = c(3,3))
for(i in c(1:4, 6:9))
{
	boxplot(chd[,i], at = 1, xaxt = "n", xlim = c(0, 4), main = colnames(chd)[i])
	boxplot(CHD$test[,i], at = 2, xaxt = "n", add = TRUE)
	boxplot(CHD$train[,i], at = 3, xaxt = "n", add = TRUE)
	axis(1, at = 1:3, labels = c("Original", "Test", "Train"), tick = TRUE)
}
par(mfrow = c(1,1))


# For discriminants we need to remove the qualitative predictor, famhist

CHD.train <- CHD$train[,-5]
CHD.test <- CHD$test[,-5]


#**************************************************************************************
#
#			Linear Discrimants
#
#**************************************************************************************

# Load the MASS library 

library(MASS)

chd.lda <- lda(chd~., data = CHD.train)

# Training set fit

chd.lda.fit <- predict(chd.lda)

# Confusion matrix for training set (uses ROC code)

score.table(chd.lda.fit$posterior[,2], CHD.train$chd)


# Plot of training set performance

plot(chd.lda.fit$poster[,2], CHD.train$chd, pch = 21, main = "LDA Results for CHD Data", xlab = "Posterior", ylab = "Actual" )

# Test set fit

chd.lda.pred <- predict(chd.lda, newdata = CHD.test)

# Confusion matrix for test set

score.table(chd.lda.pred$posterior[,2], CHD.test$chd)

# Plot of test set performance

plot(chd.lda.pred$post[,2], CHD.test$chd, pch = 21, main = "LDA Results for CHD Data", xlab = "Posterior", ylab = "Actual" )

#**************************************************************************************
#
#			Quadratic Discrimants
#
#**************************************************************************************

chd.qda <- qda(chd~., data = CHD.train)

# Training set fit

chd.qda.fit <- predict(chd.qda)

# Confusion matrix for training set

score.table(chd.qda.fit$posterior[,2], CHD.train$chd)


# Plot of training set performance

plot(chd.qda.fit$poster[,2], CHD.train$chd, pch = 21, main = "QDA Results for CHD Data", xlab = "Posterior", ylab = "Actual" )

# Test set fit

chd.qda.pred <- predict(chd.qda, newdata = CHD.test)

# Confusion matrix for training set

score.table(chd.qda.pred$posterior[,2], CHD.test$chd)


# Plot of training set performance

plot(chd.qda.pred$poster[,2], CHD.test$chd, pch = 21, main = "QDA Results for CHD Data", xlab = "Posterior", ylab = "Actual" )

#**************************************************************************************
#
#			Mixture Model Discrimants
#
#**************************************************************************************


library(mda)

chd.mda <- mda(chd~., data = CHD.train)

# Training set fit

chd.mda.fit <- predict(chd.mda, type = "post")

# Confusion matrix for training set

score.table(chd.mda.fit[,2], CHD.train$chd)


# Plot of training set performance

plot(chd.mda.fit[,2], CHD.train$chd, pch = 21, main = "MDA Results for CHD Data", xlab = "Posterior", ylab = "Actual" )

# Test set fit

chd.mda.pred <- predict(chd.mda, newdata = CHD.test, type = "post")


# Confusion matrix for training set

score.table(chd.mda.pred[,2], CHD.test$chd)

# Plot of training set performance

plot(chd.mda.pred[,2], CHD.test$chd, pch = 21, main = "MDA Results for CHD Data", xlab = "Posterior", ylab = "Actual" )


#**************************************************************************************
#
#			GLM - Logistic Regression
#
#**************************************************************************************

# For logistic regression we can use the qualitative predictor.

chd.glm <- glm(chd~., data = CHD$train, family = binomial)

# Training set fit

chd.glm.fit <- predict(chd.glm, type = "response")

# Confusion matrix for training set

score.table(chd.glm.fit, CHD$train$chd)


# Plot of training set performance

plot(chd.glm.fit, CHD$train$chd, pch = 21, main = "GLM Results for CHD Data", xlab = "Posterior", ylab = "Actual" )

# Test set fit

chd.glm.pred <- predict(chd.glm, newdata = CHD$test, type = "response")


# Confusion matrix for training set

score.table(chd.glm.pred, CHD$test$chd)


# Plot of training set performance

plot(chd.glm.pred, CHD$test$chd, pch = 21, main = "GLM Results for CHD Data", xlab = "Posterior", ylab = "Actual" )



#****************************************************************************************
#
#		GAM
#****************************************************************************************


#*******************************************************************
# Function to create the gam forumla with splines for the predictors 
#*******************************************************************

gam.form <- function(response, data, K = 4, nsvar = NULL)
{
	Names <- names(data)
	PredV <- setdiff(Names, response)
	PV <- paste("s(", PredV, ", ", K, ")", sep = "")
	if(is.null(nsvar)){
	as.formula(paste(paste(response, "~", sep = ""), paste(PV, collapse = "+")))
	}
	else as.formula(paste(paste(response, "~", sep = ""), paste(PV, collapse = "+"), "+", paste(nsvar, collapse = "+")))
	}
#*********************************************************************


library(gam)


chd.gam <- gam(gam.form("chd", chd[,-5], K = 5, nsvar = "famhist"), data = CHD$train, family = binomial)

summary(chd.gam)

par(mfrow = c(3,3))
plot(chd.gam)
par(mfrow = c(1,1))

# Training set fit

chd.gam.fit <- predict(chd.gam, type = "response")

# Confusion matrix for training set

score.table(chd.gam.fit, CHD$train$chd)


# Plot of training set performance

plot(chd.gam.fit, CHD$train$chd, pch = 21, main = "GAM Results for CHD Data", xlab = "Posterior", ylab = "Actual" )

# Test set fit

chd.gam.pred <- predict(chd.gam, newdata = CHD$test, type = "response")


# Confusion matrix for training set

score.table(chd.gam.pred, CHD$test$chd)


# Plot of training set performance

plot(chd.gam.pred, CHD$test$chd, pch = 21, main = "GAM Results for CHD Data", xlab = "Posterior", ylab = "Actual" )


#****************************
# GAM Stepwise


chd.gam.step <- step.gam(chd.gam, scope=list("sbp"=~1+ sbp +s(sbp,4)+s(sbp,5)+s(sbp,6),"tobacco"=~1+tobacco+s(tobacco,4), "ldl"=~1+ sbp +s(sbp,4)+s(sbp,5)+s(sbp,6)), "age"=~1+ sbp +s(sbp,4)+s(sbp,5)+s(sbp,6))

# Training set

chd.gam.step.fit <- predict(chd.gam.step, type = "response")

# Confusion matrix for training set

score.table(chd.gam.step.fit, CHD$train$chd)


# Plot of training set performance

plot(chd.gam.step.fit, CHD$train$chd, pch = 21, main = "GAM Stepwise Results for CHD Data", xlab = "Posterior", ylab = "Actual" )

# Test set 

chd.gam.step.pred <- predict(chd.gam.step, newdata = CHD$test, type = "response")

# Confusion matrix for training set

score.table(chd.gam.step.pred, CHD$test$chd)


# Plot of training set performance

plot(chd.gam.step.pred, CHD$test$chd, pch = 21, main = "GAM Stepwise Results for CHD Data", xlab = "Posterior", ylab = "Actual" )


#**************************************
#
#		ROC for Discriminants, GLM, and GAM
#
#**************************************

source("/Users/deb/Dropbox/department/Classes/Rcode/ROC.R")


# Just Discriminants


plot.roc(chd.lda.pred$post[,2], CHD$test[,10])

lines.roc(chd.qda.pred$post[,2], CHD$test[,10], col = "red2")

lines.roc(chd.mda.pred[,2], CHD$test[,10], col = "green")

legend(.8, .3, legend = c("LDA", "QDA", "MDA"), lwd = 2, col = c("blue", "red2", "green"))


# GLM and GAM


plot.roc(chd.glm.pred, CHD$test[,10])


lines.roc(chd.gam.pred, CHD$test[,10], col = "green")

lines.roc(chd.gam.step.pred, CHD$test[,10], col = "purple")

%lines.roc(chd.int.p, CHD$test[,10], col = "orange")

legend(.8, .3, legend = c("GLM",  "GAM", "GAM Step"), lwd = 2, col = c("blue",  "green", "purple"))




#*********************************************************
#
#			Trees
#
#*********************************************************

#
#  There are several tree packages in R: tree, party, rpart, and mvpart
#  All implement versions of the recursive partitioning algorithm
#  Here we use rpart but the syntax is similar for the others.


library(rpart)


# Rpart tree

#***********************************************************
#Trees with recursive partitioning

# Gini

chd.rpart <- rpart(chd~., data = CHD$train, method = "class")

# Entropy

chd.rparti <- rpart(chd~., data = CHD$train, method = "class", parms = list(split = "information"))

# Plots

chd.rpart #the gini tree

chd.rparti # entropy tree

# Tree Trace
par(mfrow = c(2,1))
rsq.rpart(chd.rpart)
par(mfrow = c(1,1))

par(mfrow = c(2,1))
rsq.rpart(chd.rparti)
par(mfrow = c(1,1))

# Summary

summary(chd.rpart)

summary(chd.rparti)

# Tree plots

plot(chd.rpart)
text(chd.rpart, use.n=TRUE, pretty = 3)

post(chd.rpart, filename = "")


plot(chd.rparti)
text(chd.rparti, use.n=TRUE, pretty = 3)


# plot of cross-validation relative error

plotcp(chd.rpart)

plotcp(chd.rparti)



# Selecting the best tree by cv error

best.rpart <- function(rpart.obj)
{
	cp <- rpart.obj$cptable[which.min(rpart.obj$cptable[,4]),1]
	prune.rpart(rpart.obj, cp = cp)
	}

chd.rpart.b1 <- best.rpart(chd.rpart)

chd.rparti.b1<- best.rpart(chd.rparti)

# selecting your own based on cp value

chd.rpart.b2 <- prune.rpart(chd.rpart, cp = 0.056)

plot(chd.rpart.b1)
text(chd.rpart.b1, use.n=TRUE)

plot(chd.rparti.b1)
text(chd.rparti.b1, use.n=TRUE)

plot(chd.rpart.b2)
text(chd.rpart.b2, use.n=TRUE)


# predict tree results

chd.rpart.pred <- predict(chd.rpart.b2, newdata = CHD$test, type = "prob")

chd.rparti.pred <- predict(chd.rparti.b1, newdata = CHD$test, type = "prob")

summary(chd.rpart.pred)

#*********************************************************
#
#			ROC for Trees
#
#*********************************************************

# Just trees


plot.roc(chd.rpart.pred[,2], CHD$test[,10])

lines.roc(chd.rparti.pred[,2], CHD$test[,10], col = "red2")

lines.roc(chd.ctree.pred[[2]], CHD$test[,10], col = "blue4")

lines.roc(chd.mvpart.pred[,2], CHD$test[,10], col = "cyan")

legend(.8, .3, legend = c("Tree Gini", "Tree Entropy", "Party Tree", "MV Tree"), lwd = 2, col = c("blue", "red2", "blue4", "cyan"))


