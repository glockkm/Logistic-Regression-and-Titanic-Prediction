#https://www.r-bloggers.com/evaluating-logistic-regression-models/
#http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#log

library(dplyr)
library(stargazer)
library(caret)
library(readr)

titanic = read.csv(file="tit-train.csv", header = TRUE, sep=",")

class(titanic)
summary(titanic)
View(titanic)
titanic = titanic[ -c(1) ] #take out id column
titanic = titanic[ -c(3) ] #take out name column
titanic = titanic[ -c(9) ] #take out cabin column
#over 60% missing in a column can remove that column for example cabin

#factor all categoricals
titanic$Survived = factor(titanic$Survived)
titanic$Pclass = factor(titanic$Pclass)
titanic$Sex = factor(titanic$Sex)
titanic$Embarked = factor(titanic$Embarked)
#look for any nas
anyNA(titanic)

#omit any rows with nas
titanic = na.omit(titanic) 
View(titanic)

tb_gender = table(titanic$Survived, titanic$Sex)
tb_gender
#0 is no did not survive and 1 is yes survived

chisq.test(tb_gender, correct=FALSE) #INDEPENDENT TEST, ARE THESE ASSOCIATED?
#p is smaller than .05 so reject null hypothesis but don't accept alternative yet

pairs(titanic) #to see if any input variables are highly correlated. If so, take out.

#calculate odds of surviving shipwreck
titanic$Survived
tb = table(titanic$Survived) #2x2 table
tb

290/424
#0.68 odds of surviving
#female odds of survival are 197/64 = 3.078
#male odds of survival are 93/360 = .258

#create model to see how much lower are the odds of survival for men relative to women
gender_model = glm(Survived ~ Sex, data = titanic, family = "binomial")
#To interpret the risk factor, we use odds ratio, but not log odds ratio. 
#The default setting for coefficent is log odds ratio, you might want to transform it to odds ratio, then interpret.
  
summary(gender_model)
#negative MALE GENDER b1 coeff so surv chance decreases if male and gender is significant p-value wise
#so male gender has lower chance of surviving
#people who are male gender= coeff is -2.4778% (b1 gender coeff)
#if male is negative coeff then female will be positive
exp(coef(gender_model))
#OR
#b1 = - 2.4778 so take exp of that
#.08 is the risk factor by which odds of survival decrease if male so 8% compared to female
#So male gender survival odds is 0.08 compared to the female gender
#So males have a 92% less survival odds than female


#model to look and see if age is statistically significant for odds of surv and if so by what magnitude
age_gender_model = glm(Survived ~ Sex + Age, data = titanic, family = "binomial")
summary(age_gender_model)
#p-value for age is 0.39 which is greater than 0.05 and is not statistically significant for the model
#b2 age coeff is -0.005426 so as age increases by 1, survival chances go down for male gender
#coeff for each independent variable is the effect of holding all other independent variables constant

exp(coef(age_gender_model))
#OR
# b2 age coeff = -0.005426 
#.99 is the risk factor by which odds of survival decrease if male and have a 1 year age increase. So older males have less chance of survival
#males with an increase in one year of age, the odds of survival decrease by 0.005
#So for every 1 year in age increase the survival odds is .99 compared to the year before
#So 1 year older has a 1% less survival odds than 1 year younger


age_gender_model$fitted.values

#using cross validation and caret train( )
#ctrl = trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

#mod_fit_again = train(Survived ~ .,  data=tit_train, 
#method="glm", family="binomial",
#trControl = ctrl, tuneLength = 5)
#summary(mod_fit_again)
#same results
#pred = predict(mod_fit_again, newdata=testing)
#confusionMatrix(data=pred, testing$Survived)
