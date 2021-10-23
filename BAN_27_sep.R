setwd("/Users/Sabrina/Downloads/")
wine = read.csv("wine.csv")

#structure of dataset
str(wine)

#we want to predict the variable Price

#statistical summary of data

summary(wine)

#lm - linear model
model1 = lm(Price ~ AGST,data=wine)
summary(model1) #residuals are the errors over each point
#under the estimate column we find the values of b0 and b1
#you can also find these by doing model$coefficients

#adjusted R-squared is a good way to know if an additional var
#should be added to the model

plot(wine$Price ~ wine$AGST)
#fit
abline(model1, col="blue")
#baseline
abline(a=mean(wine$Price),b=0)
#abline(a,b) where a is the intercept (or b0 in our eqn) and b is the slope

#https://www.statology.org/sst-ssr-sse-in-r/
#SSE = sum(model1$residuals^2) residuals give us the error for each data point
SSE = sum((fitted(model1) - wine$Price)^2)
SSR = sum((fitted(model1) - mean(wine$Price))^2)
SST = SSE + SSR
R2 = SSR/SST

#with the addition of this new variable we can see that 
#
model2 = lm(Price ~ AGST  + HarvestRain, data = wine )
summary(model2)

SSE = sum(model2$residuals^2)
SSE
#The SSE is higher with the added variable

#now lets use all our variables as predictors
model3 = lm(Price ~ AGST  + HarvestRain +WinterRain + Age + FrancePop, data = wine )
summary(model3)
#look at the estimate column
#if the coefficient for a var is near 0 we can remove the term
#because e.g. if we b2*AGST and b2 is near 0 it means we will extract
#almost no information from AGST so we can just remove AGST
#so we want coefficients that are far from 0, both negative and positive

#*** next to the variable it means the var is very significant and we should 
#keep it. You can look at the signif codes underneath to see how significant 
#it is. Next to it we will see the probs of how different the coeff is from 0
#usually we pick any variable with at least 1 star given to it. So our
#threshold is a probability of 0.05 of it being 0

SSE = sum(model3$residuals^2)
SSE

#so if we remove FrancePop let's see what happens
model4 = lm(Price ~ AGST  + HarvestRain +WinterRain + Age, data = wine )
summary(model4)
#now we can see that WinterRain has 1 star and Age has 2 stars compared 
#to before where they got quite bad ratings. 
#so from this we learn that we should not remove the badly rated
#variables at once. Because there might be correlations in the dataset
#if two variables are correlated we can plot the first one a function
#of the second one. WE WANT the dependent variable to be correlated to the
#independent variables/predictors, but we want to AVOID that the 
#independent variables have correlations between each other so we 
#only want to include one of them. If we include both the model
#will not make sense

#CORRELATION 
# 1 - perfect positive linear relationship
# 0 - no linear relationship between vars
# -1 - perfect negative linear relationship

#highly correlated if abs(correlation) is close to 1
cor(wine$WinterRain,wine$Price)
cor(wine$Age,wine$FrancePop) #highly correlated

#this gives us a correlation matrix for all the variables in the dataset
cor(wine)

#out of sample accuracy - how our model performs on test data
#we also want to see the R^2 on the test data
wine_test = read.csv("wine_test.csv")
str(wine_test)
predict_test = predict(model4, newdata = wine_test)
#predicted price
predict_test
#actual price
wine_test$Price

SSE = sum((wine_test$Price - predict_test)^2)
#we want the mean of our training set for SST
SST = sum((wine_test$Price - mean(wine$Price))^2)
#this is a pretty good out of sample R^2
#but to be more sure about our out of sample accuracy
#we should use more data points than just 2
R2 = 1 - SSE/SST

#our model can't do worse on the train data than the baseline model
#so the R^2 is over 0, but our model can do worse on the test data
#than the baseline model so it is possible for the R^2
#to be negative



