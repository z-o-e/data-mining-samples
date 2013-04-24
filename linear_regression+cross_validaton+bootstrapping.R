
#*****************************************************
#
#			Algae Data
#
#*****************************************************


# Data are in the DMwR package


install.packages("DMwR", dependencies = T)

library(DMwR)

summary(algae)


# Data Cleaning

# which observations are missing values for 20% or more of the variables?

manyNAs(algae, .2)

# Removing extreme cases, see text for discussion

algae.clean <- algae[-manyNAs(algae, .2),]

# Imputing values for others using knn, see text.

algae.clean <- knnImputation(algae.clean, k=10, meth = "median")

summary(algae.clean)

#***************************************************************
#
#		Linear Models
#
#***************************************************************

# main effects model

algae.lm1 <- lm(a1~., data = algae.clean[,1:12])

summary(algae.lm1)

# stepwise regression model

algae.step <- step(algae.lm1)

summary(algae.step)


# All subsets models

library(leaps)

algae.sub <- regsubsets(a1~., data = algae.clean[,1:12], nbest = 10)

summary(algae.sub)

plot(algae.sub, scale = "adjr2")


# *****************************

#  CV 

# *****************************

# Library DAAG

library(DAAG)


# 10 fold cross validation of the main effects model

CVlm(df = algae.clean[,1:12], form.lm = formula(algae.lm1), m = 10)





# *****************************

#  Bootstrapping

# *****************************

install.packages("relaimpo", dependencies = T)
library(relaimpo)

# bootstrapping with b = 100. Use larger b and wait but get more confidence.

algae.boot <- boot.relimp(algae.lm1, b =100, type = c("lmg", "last", "first"))

#Results

booteval.relimp(algae.boot) # print result

# plot
plot(booteval.relimp(algae.boot,sort=TRUE))

