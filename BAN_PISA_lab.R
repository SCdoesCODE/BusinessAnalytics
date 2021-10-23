#Load the training and testing sets using the read.csv() function, and 
#save them as variables with the names pisaTrain and pisaTest.
setwd("/Users/Sabrina/Downloads/")
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

#1.1 How many students are there in the training set?
#ANSWER : 3663 observations

#Using tapply() on pisaTrain, what is the average reading 
#test score of males?

tapply(pisaTrain$readingScore,pisaTrain$male == 1, mean)[2]

#ANSWER = 483.5325

#Of females?

tapply(pisaTrain$readingScore,pisaTrain$male == 1, mean)[1]

#ANSWER = 512.9406 

#1.3 Which variables are missing data in at least one observation in 
#the training set? Select all that apply.

colnames(pisaTrain)[colSums(is.na(pisaTrain)) > 0]

#ANSWER: run above code

#1.4 Type the following commands into your R console to remove observations 
#with any missing value from pisaTrain and pisaTest:

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

#How many observations are now in the training set?

#ANSWER : 2414

#How many observations are now in the testing set?

#ANSWER : 990

#2.1 Which of the following variables is an unordered factor with at 
#least 3 levels? (Select all that apply.)

#ANSWER : male, raceeth

#Which of the following variables is an ordered factor with at least 
#3 levels? 

#ANSWER : grade

#2.2 Which binary variables will be included in the regression model? 

table(pisaTrain$raceeth)

#ANSWER : All except raceethWhite

#2.3 For a student who is Asian, which binary variables would be set to 0? 
#All remaining variables will be set to 1.

#ANSWER : All except raceethAsian 

#For a student who is white, which binary variables would be set to 0? 
#All remaining variables will be set to 1. 

#ANSWER : All of them, because white is the reference we set all to 0

#3.1 Set the reference level of the factor by typing the following two 
#lines in your R console:

pisaTrain$raceeth = relevel(factor(pisaTrain$raceeth), ref="White")
pisaTest$raceeth = relevel(factor(pisaTest$raceeth), ref="White")

#Now, build a linear regression model (call it lmScore) using the training 
#set to predict readingScore using all the remaining variables.

lmScore = lm(readingScore ~., data = pisaTrain)

#What is the Multiple R-squared value of lmScore on the training set?

summary(lmScore)

#ANSWER : 0.3251

#3.2 What is the training-set root-mean squared error (RMSE) of lmScore?

#Residual sum of squares
RSS <- c(crossprod(lmScore$residuals))
MSE <- RSS / length(lmScore$residuals)
RMSE <- sqrt(MSE)
#Also given by Residual standard error by the summary function

#ANSWER : 73.81

#3.3 Consider two students A and B. They have all variable values the same, 
#except that student A is in grade 11 and student B is in grade 9. 
#What is the predicted reading score of student A minus the predicted 
#reading score of student B?

lmScore
29.54271*2

#ANSWER : grade has the coefficient 29.54271
#so we do 29.54271*11 - 29.54271*9 = 29.54271*2 = 59.08542


#3.4 What is the meaning of the coefficient associated with variable 
#raceethAsian?

#Predicted difference in the reading score between an Asian student and 
#a white student who is otherwise identical

#3.5 Based on the significance codes, which variables are candidates for 
#removal from the model? Select all that apply. (We'll assume that 
#the factor variable raceeth should only be removed if none of its 
#levels are significant.)

summary(lmScore)

#ANSWER :#preschool, motherHS, motherWork, fatherHS, fatherWork, selfBornUS,
#motherBornUS, fatherBornUS, englishAtHome, minutesPerWeekEnglish,
#studentsInEnglish, schoolHasLibrary, urban

#4.1 What is the range between the maximum and minimum predicted reading 
#score on the test set?

predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)

#ANSWER = min (353.2) and max(637.7)

#4.2 What is the sum of squared errors (SSE) of lmScore on the testing set?

SSE = sum((pisaTest$readingScore - predTest)^2)
SSE

#ANSWER : 5762082

#What is the root-mean squared error (RMSE) of lmScore on the testing set?

sqrt(mean((pisaTest$readingScore - predTest)^2))

#ANSWER : 76.29079

#4.3 What is the predicted test score used in the baseline model? 
#Remember to compute this value using the training set and not the test set.
mean(pisaTrain$readingScore)

#ANSWER : 517.9629

#What is the sum of squared errors of the baseline model on the testing set?
#HINT: We call the sum of squared errors for the baseline model the total 
#sum of squares (SST).

SST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
SST

#ANSWER : 7802354

#4.4 What is the test-set R-squared value of lmScore?

R2 = 1 - SSE/SST
R2

#ANSWER : 0.2614944

#Additional questions:

#According to the model, who have the highest scores taking into 
#account the race of the students? 
#Indian, Asian, White, ...? To answer the question, take a look at 
#the coefficients.

lmScore

#ANSWER : raceethWhite has the highest coefficient because
#all other coefficients are negative

#However, what do you get when you use "tapply" to calculate the mean 
#of each group?

tapply(pisaTrain$readingScore, pisaTrain$raceeth, mean)

#ANSWER : Asian students have the highest scores 
  
#Are the results coherent? If not, why not?

#No the mean is not consistent with the coefficients

