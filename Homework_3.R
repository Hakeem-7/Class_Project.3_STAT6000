library(ISLR)
names(Weekly)
dim(Weekly)

# Q10 (a)
## Numerical Summary
dim(Weekly)
str(Weekly)
summary(Weekly)
## Graphical Summary
pairs(Weekly)
cor(Weekly[,1:8]) #There is a strong positive relationship between "Year" and "Volume". lag2 and Lag5 showed very slight correlation with "Today."
library(corrplot)
corrplot(cor(Weekly[,1:8]))
plot(Weekly$Volume) #Corroborates the previous deduction about the "Year" and "Volume" correlation.
library(ggcorrplot)
ggcorrplot(cor(Weekly[,1:8])) #Another approach to the dataset visualization
attach(Weekly)


# Q10(b)
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly, 
              family = binomial)
summary(glm.fit) #Based on the p-value, only Lag2 is statistically signigicant.
coef(glm.fit)

# Q10(c)
glm.prob = predict(glm.fit, type = "response")
glm.prob[1:12] #Predicts P(Y=1|X), where 2 is the dummy variable for a rise in the stock market.
contrasts(Direction)
glm.pred = rep("Down", 1089)
glm.pred[glm.prob>0.5]= "Up" 
table(glm.pred, Direction)
mean(glm.pred==Direction) #Model precision
#the confusion matrix presents the wrongly classified data off the "left to right" diagonal matrix. 
#In this scenario, the misclassified observations are 48 and 430, which sums up to 478 observations.
#However, the model accuracy (i.e., ~56.1%) does a slightly better job than a random guess.

# Q10(d)
### Training the data set, and testing on held out data set.
train = (Year<2009)
Weekly.9.10 = Weekly[!train,]
dim(Weekly.9.10)
Direction.9.10 = Direction[!train]

glm.fits = glm(Direction~Lag2, data = Weekly, 
               family = binomial, subset = train)
glm.probs = predict(glm.fits, Weekly.9.10, type = "response")
glm.preds = rep("Down", 104)
glm.preds[glm.probs>.5]="Up"
table(glm.preds, Direction.9.10)
mean(glm.preds==Direction.9.10) #The overall model accuracy is 62.5%, and the test error rate is 37.5%.
#By utilizing the only significant predictor (i.e., Lag2) in the logistic regression model, model accuracy increased by approx. 6.5%.
#The confusion matrix suggests that accuracy of prediction of upward and downward trend in the stock market 
#are approx. 92% and 21%, respectively.
mean(glm.preds!= Direction.9.10) 


# Q10(e) - LDA

library(MASS)

lda.fit = lda(Direction~Lag2, data = Weekly, subset=train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, Weekly.9.10)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.9.10)
mean(lda.class==Direction.9.10) #Model accuracy is 62.5%, which is identical w/ the result obtained in the Logistic Regression that considered only Lag2 as the predictor.

# Q10(f) - QDA

qda.fit = qda(Direction~Lag2, data = Weekly, subset = train)
qda.fit
qda.pred = predict(qda.fit, Weekly.9.10)
qda.class = qda.pred$class
table(qda.class,Direction.9.10)
#The confusion matrix suggest that the model is 100% accurate in predicting market rise, but completely inaccurate (i.e., 0%) when predicting market fall.
mean(qda.class==Direction.9.10) #Model accuracy is approx 58.7%. 

# Q10(g) - KNN: K = 1

library(class)
train.X= as.matrix(Lag2[train])
test.X= as.matrix(Lag2[!train])
train.Direction=Direction[train]
set.seed(7)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.9.10)
#The confusion matrix suggest that the model is approx 50.8% and 48.8% accurate in predicting the market rise and fall.
mean(knn.pred==Direction.9.10) #Model accuracy is approx. 50%

# Q10(h) 
# With respect to overall model accuracy, Logistic regression and LDA provided the best results, followed by QDA and KNN-1.

#Q10h(i) - Freestyle

# Logistic regression with Lag2 and Lag2~Lag1 synergy effect.
fit.glm1 = glm(Direction ~ Lag2+Lag2:Lag1, data = Weekly, family = binomial, subset = train)
glm.prob1 = predict(fit.glm1, Weekly.9.10, type = "response")
glm.pred1 = rep("Down", length(glm.prob1))
glm.pred1[glm.prob1>.5] = "Up"
table(glm.pred1, Direction.9.10)
mean(glm.pred1==Direction.9.10)

# LDA with Lag2 and Lag2~Lag1 synergy effect.
lda.fit1 = lda(Direction ~ Lag2+Lag2:Lag1, data = Weekly, subset = train)
lda.pred1 = predict(lda.fit1, Weekly.9.10)
table(lda.pred1$class, Direction.9.10)
mean(lda.pred1$class == Direction.9.10) #Model accuracy is equivalent to the logistic regression model accuracy (i.e., 58.65%).

# QDA with Lag2 and square of (abs(Lag2))
qda.fit1 = qda(Direction ~ Lag2 + (abs(Lag2))^2, data = Weekly, subset = train)
qda.pred1 = predict(qda.fit1, Weekly.9.10)
table(qda.pred1$class, Direction.9.10)
mean(qda.pred1$class==Direction.9.10) #model accuracy is 57.69%.

# KNN k = 5
knn.pred1 = knn(train.X, test.X, train.Direction, k = 5)
table(knn.pred1, Direction.9.10)
mean(knn.pred1==Direction.9.10) #Model accuracy is  53.85%.

# KNN k = 10
knn.pred2 = knn(train.X, test.X, train.Direction, k = 10)
table(knn.pred2, Direction.9.10)
mean(knn.pred2==Direction.9.10) #Model accuracy is  57.69%.

#Logistic regression and LDA outperformed the other analysis methods, based on model accuracy.



# Question 11(a) - attach mpg01
attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[median(mpg)<mpg] = 1
Auto = data.frame(Auto, mpg01)

# Q11(b) - Explore the data
dim(Auto)
str(Auto)
summary(Auto)
pairs(Auto)
cor(Auto[,-9])
par(mfrow = c(1,2))
boxplot(weight~mpg01, data = Auto, main = "Weight vs mpg01", col = "light blue")
boxplot(acceleration~mpg01, data = Auto, main = "Acceleration vs mpg01", col = "red")
par(mfrow = c(1,2))
boxplot(year~mpg01, data = Auto, main = "Year vs mpg01", col = "purple")
boxplot(horsepower~mpg01, data = Auto, main = "Horsepower vs mpg01", col = "green")
par(mfrow = c(1,3))
boxplot(cylinders~mpg01, data = Auto, main = "Cylinders vs mpg01", col = "Blue")
boxplot(displacement~mpg01, data = Auto, main = "Displacement vs mpg01", col = "green")
boxplot(origin~mpg01, data = Auto, main = "Origin vs mpg01", col = "Violet")

#From the boxplots, we can conclude that some relationships exist between "mpg01" and "weight," 
# "horsepower," "year," "acceleration," and "displacement."

# Q11(c) - split the data into training and testing sets

train = (year %% 2 == 0)
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
#Test error is approx.12.1%

# Q11(e) - QDA

qda.fit2 = qda(mpg01 ~ weight+acceleration+horsepower+displacement+year, 
               data = Auto, subset = train)
qda.fit2
qda.pred2 = predict(qda.fit2,Auto.test)
table(qda.pred2$class,mpg01.test)
mean(qda.pred2$class!=mpg01.test)
#Test error is approx.10.4%

# Q11(f) - Logistic Regression

glm.fit2 = glm(mpg01 ~ weight+acceleration+horsepower+displacement+year,
               data = Auto, family = binomial, subset = train)
summary(glm.fit2)
glm.prob2 = predict(glm.fit2, Auto.test, type = "response")
glm.pred2 = rep(0, length(glm.prob2))
glm.pred2[glm.prob2 > 0.5] = 1
table(glm.pred2, mpg01.test)
mean(glm.pred2 != mpg01.test)
#Test error is approx. 11.0%

# Q11(g) - KNN
# K = 1
train.X1 = cbind(weight,acceleration,horsepower,displacement,year)[train,]
test.X1 = cbind(weight,acceleration,horsepower,displacement,year)[!train,]
train.mpg01 = mpg01[train]
set.seed(7)
knn.pred3 = knn(train.X1, test.X1, train.mpg01, k = 1)
table(knn.pred3, mpg01.test)
mean(knn.pred3 != mpg01.test)
#Test error is approx. 15.4%

# K = 10
train.X1 = cbind(weight,acceleration,horsepower,displacement,year)[train,]
test.X1 = cbind(weight,acceleration,horsepower,displacement,year)[!train,]
train.mpg01 = mpg01[train]
set.seed(7)
knn.pred4 = knn(train.X1, test.X1, train.mpg01, k = 10)
table(knn.pred4, mpg01.test)
mean(knn.pred4 != mpg01.test)
#Test error is approx. 15.9%

# K = 100
train.X1 = cbind(weight,acceleration,horsepower,displacement,year)[train,]
test.X1 = cbind(weight,acceleration,horsepower,displacement,year)[!train,]
train.mpg01 = mpg01[train]
set.seed(7)
knn.pred5 = knn(train.X1, test.X1, train.mpg01, k = 100)
table(knn.pred5, mpg01.test)
mean(knn.pred5 != mpg01.test)
#Test error is approx. 14.3%. A K value of 100 seems to yield the lowest test error.

