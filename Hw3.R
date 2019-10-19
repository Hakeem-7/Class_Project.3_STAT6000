library(MASS)
install.packages("ISLR")
library(ISLR)
fix(Boston)
names(Boston)
?Boston

detach(Boston)#Linear Regression model w/ one predictor
lmfit = lm(medv~lstat, data = Boston)
attach(Boston)
lmfit= lm(medv~Boston)
lmfit
summary(lmfit) #Provides a summmary of the model output
names(lmfit) #For determining the other pieces of info stored in the Variable
coef(lmfit) #To extract the coefficient of the fit
confint(lmfit) #Computes the CI of the coefficient estimates.
predict(lmfit, data.frame(lstat=c(5,10,15)), interval = "confidence")
