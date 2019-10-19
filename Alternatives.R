library(MASS)
install.packages("ISLR")
library(ISLR)
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

## Linear regresssion (TBC) ---------

# Logistic Regression, LDA, QDA, and KNN


library(ISLR)
library(MASS)
library(class)
set.seed(107)
train = (year %% 2 == 0) ##modular division which seperates the dataset into even and odds.
Auto.train = Auto[train, ]
Auto.test = Auto[!train, ]
mpg01.test <- mpg01[!train]

# Q11(d) - LDA

lda.fit2 = lda(mpg01 ~ weight+acceleration+horsepower+displacement+year, 
               data = Auto, subset = train)
lda.fit2
lda.pred2 = predict(lda.fit2,Auto.test)
table(lda.pred2$class,mpg01.test)
mean(lda.pred2$class!=mpg01.test)
## Test error is approx.12.1%


# Alternative method 1
install.packages("caTools")
?mtcars

## Splitting sample size evenly

smp.size <- floor(.50*nrow(Auto))
set.seed(107) #To make the partitioning reproducible
train.ind <- sample(seq_len(nrow(Auto)), size = smp.size)
Auto.train1 <- Auto[train.ind, ]
Auto.test1 <- Auto[-train.ind, ]
mpg01.test1 <- mpg01[-train.ind]

lda.fit2.1 = lda(mpg01 ~ weight+acceleration+horsepower+displacement+year, 
                 data = Auto, subset = train.ind)
lda.fit2.1
lda.pred2.1 = predict(lda.fit2.1, Auto.test1)
table(lda.pred2.1$class,mpg01.test1)
mean(lda.pred2.1$class!=mpg01.test1)
## Test error is approx.9.2%

# The reason for the disparity in model accuracy can be traced to the differences in the data frame used.
# Alternative 1 is reasonable if the entire data is to be splitted into 2.


fix(Auto)


# Alternative method 2

require(caTools)
set.seed(107)
train2 = sample.split(Auto$year, SplitRatio = .50) #Spltting the dat w/ diff. col. yields dif. test error estimate?? 
Auto.train2 = subset(Auto, train2 == TRUE)
Auto.test2 = subset(Auto, train2 == FALSE) #instead of using the "==" sign
mpg01.test2 = mpg01[!train2]

lda.fit2.2 = lda(mpg01 ~ weight+acceleration+horsepower+displacement+year, 
                 data = Auto, subset = train2)
lda.fit2.2
lda.pred2.2 = predict(lda.fit2.2, Auto.test2)
table(lda.pred2.2$class,mpg01.test2)
mean(lda.pred2.2$class!=mpg01.test2)
## Test error is approx.11.2%.

# Alternative 2 is reasonable if the splitting is desirable in a particular column.
