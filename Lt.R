library(MASS)
library(ISLR)
a = Boston$medv
b = Boston$lstat

first.fit = lm(a~b)
summary(Boston)

first.fit
summary(first.fit)
names(first.fit)
coef(first.fit)
confint(first.fit)
predict(first.fit, data.frame(b<-c(5,10,15)), interval = "confidence")

names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9]) #Quick correlation examination
plot(Smarket$Volume)

## logistic regression
attach(Smarket)
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
              data = Smarket, 
              family = binomial)  # the argument "family = binomial is for logistic regressions."
summary(glm.fit)
coef(glm.fit) #For accessing the coefficients of the model
summary(glm.fit)$coef
summary(glm.fit)$coef[,4] #Yields the coeficient of just the 4th column
glm.prob = predict(glm.fit, type = "response") #the arg tells R to predict the prob of occurence and not logit
glm.prob[1:12] #Predicts P(Y=1|X), where 1 is the dummy variable for a rise in the stock market.
contrasts(Direction) #Affirms the previous comment.
glm.pred = rep("Down", 1250)
glm.pred[glm.prob>0.5]= "Up" #This simple logic is what makes you a good programmer.
table(glm.pred, Direction)
model.precision = (145+507)/1250 #remember, the model was trained and tested on the same data set.
round(model.precision, 2)
mean(glm.pred==Direction) #Alternative way of extracting the model precision.

### Training the data set, and testing on held out data set.

train = (Year<2005) #Boolean vector: extracting a submatrix from a matrix
Smarket.2005 = Smarket[!train,] #the exclamation is used to reverse the Boolean vector
dim(Smarket.2005)
Direction.2005 = Direction[!train]
# contrasts(Direction.2005)
# summary(Direction.2005)

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, 
               family = binomial, subset = train) #subset arg for fitting on a subset of the data
glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.preds = rep("Down", 252)
glm.preds[glm.probs>.5]="Up"
table(glm.preds, Direction.2005)
mean(glm.preds==Direction.2005) #test error rate
mean(glm.preds!= Direction.2005) # ! = "not equal to" predicts the training error rate

glm.fits12 = glm(Direction~Lag1+Lag2, data = Smarket, 
                 family = binomial, subset = train) #Trying out the Predictors w/ strongest correlation w/ the response.
glm.probs12 = predict(glm.fits12, Smarket.2005, type = "response")
glm.preds12 = rep("Down", 252)
glm.preds12[glm.probs12>.5] = "Up"
table(glm.preds12, Direction.2005)
mean(glm.preds12==Direction.2005) #Test error rate
mean(glm.preds12!=Direction.2005) #Training error rate

predict(glm.fits12, newdata = data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type = "response")

