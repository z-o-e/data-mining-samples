
#*******************************************************************************
#
#				Shrinkage Methods: Prostate Data Example
#
#*******************************************************************************


prostate <- read.table("prostate.data.csv", sep = ",", header = T)

summary(prostate)

#***********************
# Data cleaning
#***********************

# Remove  the index variable

pros <- prostate[,-c(1)]

summary(pros)

# Center and scale the predictor variables

pros[,1:9] <- scale(pros[,1:9])


#Note that the one excluded from the training set or test is
# the extreme value of 5.58 lpsa

pros[which(is.na(pros[,"train"]) ),]

# Remove it

pros <-	pros[-which(is.na(pros[,"train"]) ),]


# Make it a data frame

pros <- as.data.frame(pros)

# Training & test sets

pros.train <- pros[which(pros$train == T),-10]
pros.test <- pros[which(pros$train == F),-10]




#**************************************
#
#		Evaluation Function
#
#**************************************

# RMSE

rmse <- function(yp,ya)
{
	sqrt(mean((ya-yp)^2))
	
}


#**************************************
#
#		Scatter Plot Matrix
#
#**************************************

source("SPM_Panel.R")

uva.pairs(pros[,1:9])



#**************************************
#
#		OLS Regression
#
#**************************************

pros.lm <- lm(lpsa~., data = pros.train)

summary(pros.lm)

# Diagnostics

par(mfrow = c(2,2))
plot(pros.lm)
par(mfrow = c(1,1))

# RMSE for test data

lm.fit <- predict(pros.lm, newdata = pros.test)


rmse(lm.fit, pros.test[,9])



#**************************************
#
#		Principal Compoents Regression
#
#**************************************

library(pls)

pros.pca <- prcomp(pros.train[,-9])

# Plot of the principal components


op <- par("mar")
par(xpd = T, mar = op+c(0,0,0,6))

matplot(1:8, pros.pca$rot, type = "l", xlab = "Variables", ylab = "Loadings", main = "PCA Prostate Data")

legend(8.5, .72, legend = 1:8, lwd = 1, col = 1:8, lty =1:8, title = "PC#")

par(mar = op)
par(xpd = F)

# scree plot

plot(1:8, pros.pca$sdev, xlab = "PC Number", ylab = "Standard Deviation", type = "h", main = "Prostate PCA Scree Plot")



# Regression on training data

pros.pcr <- pcr(lpsa~., data = pros.train, validation = "CV", y =T)

validationplot(pros.pcr, legendpos = "topright")

# choose 3

coefplot(pros.pcr, ncomp = 3, xlab = "Variables", main = "Coefficients for PCR with Prostate Data", type = "h")

# RMSE for test data 

# 3 components

pcr.fit <- predict(pros.pcr, newdata = pros.test[,-9], ncomp = 3, type = "response")

rmse(pcr.fit, pros.test[,9])



#**************************************
#
#		Partial Least Squares Regression
#
#**************************************


library(pls)


pros.pls  <- plsr(lpsa ~ ., data = pros.train,  ncomp = 8, validation = "CV")

coefplot(pros.pls, ncomp = 4, xlab = "Variables", main = "Coefficients for PLS with Prostate Data with 3 PC", type = "h")

validationplot(pros.pls, legendpos = "topright", main = "CV for PLS with Prostate Data")

# RMSE for test data 

pls.fit <- predict(pros.pls, newdata = pros.test[,-9], ncomp = 4, type = "response")

rmse(pls.fit, pros.test[,9])




#**************************************
#
#		Ridge Regression
#
#**************************************

library(MASS)

# Get the ridge trace for lambda from 0 - 500
	
pros.ridge <- lm.ridge(lpsa ~ .,lambda = seq(0,500), data = as.data.frame(pros.train))

# Plot the ridge trace

colnames(t(pros.ridge$coef))

matplot(pros.ridge$lambda, t(pros.ridge$coef),type="l",lty=1,xlab=expression(lambda),ylab=expression(hat(beta)), main = "Ridge Trace of Prostate Data", col = c("red", "orange", "tan","seagreen", "green", "blue", "purple", "violet"))


matplot(pros.ridge$lambda, t(pros.ridge$coef),type="l",lty=1,xlab=expression(lambda),ylab=expression(hat(beta)), main = "Ridge Trace of Prostate Data", col = 1:nrow(pros.ridge$coef))


legend(380,.6, legend = colnames(t(pros.ridge$coef)), lwd = 2, col = 1:nrow(pros.ridge$coef)) 

select(pros.ridge)

abline(v=which.min(pros.ridge$GCV)-1)

# CV results

plot(1:length(pros.ridge$GCV), pros.ridge$GCV, ylab = "GCV", xlab = "Lambda", type = "l", main = "Ridge CV Results for Prostate Data")

abline(v=which.min(pros.ridge$GCV))


# RMSE for the predicted value from Ridge

ridge.pred <- as.matrix(pros.test[,-9]) %*% pros.ridge$coef[,which.min(pros.ridge$GCV)] + mean(pros.train[,9])

rmse(ridge.pred, pros.test[,9])


#**************************************
#
#		Lasso Regression
#
#**************************************


 #  Load library lars
  
 library(lars)
 
 # the lasso model
 
pros.l1  <- lars(as.matrix(pros.train[,-9]), pros.train[,9], type = "lasso")
 
 # Coefficient plot
 
 plot(pros.l1)
 
 # CV 
 
cv.l1  <- cv.lars(as.matrix(pros.train[,-9]), pros.train[,9], type = "lasso")
 
plotCVLars(cv.l1)


# RMSE for the predicted value from lasso

lasso.cv <- cv.l1$cv[which.min(cv.l1$cv > min(cv.l1$cv) + cv.l1$cv.error[which.min(cv.l1$cv)])]


l1.pred <- predict(pros.l1, pros.test[,-9], s = lasso.cv, type = "fit", mode = "fraction")

rmse(l1.pred$fit, pros.test[,9])


 #**************************************
#
#		LAR
#
#**************************************
  
 library(lars)
 
 # the lar model
 
pros.lar  <- lars(as.matrix(pros.train[,-9]), pros.train[,9], type = "lar")
 
 # Coefficient plot
 
 plot(pros.lar)
 
 
# CV 
 
cv.lar  <- cv.lars(as.matrix(pros.train[,-9]), pros.train[,9], type = "lar")
 
plotCVLars(cv.l1)
 
 
# RMSE for the predicted value from lasso

lar.cv <- cv.lar$cv[which.min(cv.lar$cv > min(cv.lar$cv) + cv.lar$cv.error[which.min(cv.lar$cv)])]

lar.pred <- predict(pros.lar, pros.test[,-9], s = lar.cv, type = "fit", mode = "fraction")

