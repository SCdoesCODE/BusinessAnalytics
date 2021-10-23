setwd("/Users/Sabrina/Downloads/")
cc = read.csv("climate_change.csv")

#Problem 1.1 - Creating Our First Model

#split into train and test
train <- subset(cc, Year <= 2006)
test <- subset(cc, Year >= 2007)

#build a linear regression model to predict the dependent variable 
#Temp, using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols as 
#independent variables. Use the training data

model <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)
summary(model)

#Enter the model R2 (the "Multiple R-squared" value): 0.7509

#Problem 1.2 - Creating Our First Model

#Which variables are significant in the model? 
#We will consider a variable significant only if the p-value is 
#below 0.05. (Select all that apply.)

#ANSWER : All except for CH4 and N2O

#Problem 2.1 - Understanding the Model

#All of the gas concentration variables reflect human development - N2O and
#CFC.11 are correlated with other variables in the data set.
#COMMMENT - we can see in the correlation matrix that they have high 
#correlation with other variables

#Problem 2.2 - Understanding the Model

#Compute the correlations between all the variables in the training set. 
#Which of the following independent variables is N2O highly correlated 
#with (absolute correlation greater than 0.7)? Select all that apply.

cor(train)

#ANSWER : Year, CO2, CH4, CFC.12,Temp

#Which of the following independent variables is CFC.11 highly correlated 
#with? Select all that apply.

#ANSWER : CH4, CFC.12, 

#Problem 3 - Simplifying the Model

model2 <- lm(Temp ~ MEI + N2O +TSI + Aerosols, data = train)
summary(model2)

#Enter the coefficient of N2O in this reduced model: 2.532e-02

#(How does this compare to the coefficient in the previous model 
#with all of the variables?)
summary(model)
#ANSWER : In the previous model N20 has a coefficient of -1.653e-02
#N20 fits better in this new model, has more significance 

#Enter the model R2 : 0.7261
summary(model2)

#Problem 4 - Automatically Building the Model

step_model <- step(model)

#Enter the R2 value of the model produced by the step function:0.7508
summary(step_model)

#Which of the following variable(s) were eliminated from the full model 
#by the step function?
#ANSWER : CH4

#Problem 5 - Testing on Unseen Data

#Using the model produced from the step function, calculate temperature 
#predictions for the testing data set, using the predict function.

predict_test = predict(step_model, newdata = test)

#Enter the testing set R2

SSE = sum((test$Temp - predict_test)^2)
SST = sum((test$Temp - mean(train$Temp))^2)
R2 = 1 - SSE/SST


