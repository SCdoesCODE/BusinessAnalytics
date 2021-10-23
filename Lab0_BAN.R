#What is the age of the 7th rider in the dataset? 45
BikeData$age[7]
#How many of the first 10 riders in the dataset ride daily? 3
first_ten <- BikeData$cyc_freq[1:10]
daily<- first_ten[first_ten=="Daily"]
length(daily)
#What is the speed of the first female who cycles less than one time per month (in
#miles/hour)? 8.1
gen <- BikeData$gender
females <- gen[gender=="F"]
first_fem <- females[1,]
first_fem$speed

#What type of variable is student? integer
typeof(BikeData$student)
#What type of variable is cyc_freq? character
typeof(BikeData$cyc_freq)
#What type of variable is distance? double
typeof(BikeData$distance)

#In this lab, we will be creating a new dataset that includes just the student riders and all
#of their variables. What is the correct terminology for this new dataset?
#Answer : data frame

#How many students are in the dataset? 14
#show number of students
table(BikeData$student)

#When you look in your workspace, how many variables do you see in this new data
#frame, student? 14
#Pull out student data into a new data frame
student <-BikeData[BikeData$student==1,]
student

#What is the most frequently observed answer? Daily
#Find how often the students ride
table(student$cyc_freq)

#How is this vector "distance" described in the workspace? num [1:10] 
#Create vector for the variable distance
distance <-student$distance
distance

#How far do the students ride on average? 6.26 miles
#Find average distance ridden
mean(distance)

#How many of the cyclists were students, how often did they ride, and what was the
#average distance they rode? 
nrow(student) #14
table(student$cyc_freq) #daily 8, several times per week 6
mean(distance) #6.26 miles

#In this lab, we examined data on 14 student riders. Most of the student riders ( a
#total of 8 out of 14) rode their bikes daily . On average, the
#students rode about 6.26 miles on each trip.
