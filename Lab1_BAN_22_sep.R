#Problem 1

#How many rows of data (observations) are in this dataset? 191641
nrow(mvtWeek1.1)
#How many variables are in this dataset? 
#Answer : 11 variables (from the environment window)
#Using the "max" function, what is the maximum value of the variable "ID"? 9181151
max(mvtWeek1.1$ID)
#What is the minimum value of the variable "Beat"? 2535
max(mvtWeek1.1$Beat)
#How many observations have value TRUE in the Arrest variable 
#(this is the number of crimes for which an arrest was made)? 15536
arrests <- mvtWeek1.1[mvtWeek1.1$Arrest == TRUE,]
nrow(arrests)
#How many observations have a LocationDescription value of ALLEY? 2308
alleys <- mvtWeek1.1[mvtWeek1.1$LocationDescription == "ALLEY",]
nrow(alleys)

#Problem 2

#What is the month and year of the median date in our dataset? May 2006
DateConvert = as.Date(strptime(mvtWeek1.1$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

mvtWeek1.1$Month = months(DateConvert)
mvtWeek1.1$Weekday = weekdays(DateConvert)
mvtWeek1.1$Date = DateConvert

#In which month did the fewest motor vehicle thefts occur? April
min(mvtWeek1.1$Month)
#On which weekday did the most motor vehicle thefts occur? Wednesday
max(mvtWeek1.1$Weekday)
#Which month has the largest number of motor vehicle thefts for 
#which an arrest was made?
cross <- table(mvtWeek1.1$Arrest, mvtWeek1.1$Month)
which.max(cross[2,])

#Problem 3

hist(mvtWeek1.1$Date, breaks=100)

#In general, does it look like crime increases or decreases from 2002 - 2012?
#Answer : decreases

#In general, does it look like crime increases or decreases from 2005 - 2008?
#Answer : decreases

#In general, does it look like crime increases or decreases from 2009 - 2011?
#Answer : increases

#Problem 3.2

#Does it look like there were more crimes for which arrests were made in 
#the first half of the time period or the second half of the time period?
#Answer : It looks like there were more arrests in the first half
boxplot(mvtWeek1.1$Date ~ mvtWeek1.1$Arrest)

#Problem 3.3

#For what proportion of motor vehicle thefts in 2001 was an arrest made? 0.1041173
arrests_2001 <- mvtWeek1.1[mvtWeek1.1$Arrest == TRUE & mvtWeek1.1$Year==2001,]
all_2001 <- mvtWeek1.1[mvtWeek1.1$Year==2001,]
nrow(arrests_2001)/nrow(all_2001)

#For what proportion of motor vehicle thefts in 2007 was an arrest made? 0.08487395
arrests_2007 <- mvtWeek1.1[mvtWeek1.1$Arrest == TRUE & mvtWeek1.1$Year==2007,]
all_2007 <- mvtWeek1.1[mvtWeek1.1$Year==2007,]
nrow(arrests_2007)/nrow(all_2007)

#For what proportion of motor vehicle thefts in 2012 was an arrest made? 0.03902924
arrests_2012 <- mvtWeek1.1[mvtWeek1.1$Arrest == TRUE & mvtWeek1.1$Year==2012,]
all_2012 <- mvtWeek1.1[mvtWeek1.1$Year==2012,]
nrow(arrests_2012)/nrow(all_2012)

#Problem 4
sort_locations <- sort(table(mvtWeek1.1$LocationDescription))
#Which locations are the top five locations for motor vehicle thefts, 
#excluding the "Other" category? You should select 5 of the following options.
tail(sort_locations,6)
#Answer : 
#STREET
#PARKING LOT/GARAGE(NON.RESID.)
#ALLEY
#GAS STATION 
#DRIVEWAY - RESIDENTIAL

#Problem 4.2
#How many observations are in Top5? 177510
Top_1 <- mvtWeek1.1[mvtWeek1.1$LocationDescription == "STREET",]
Top_2 <- mvtWeek1.1[mvtWeek1.1$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)",]
Top_3 <- mvtWeek1.1[mvtWeek1.1$LocationDescription == "ALLEY",]
Top_4 <- mvtWeek1.1[mvtWeek1.1$LocationDescription == "GAS STATION",]
Top_5 <- mvtWeek1.1[mvtWeek1.1$LocationDescription == "DRIVEWAY - RESIDENTIAL",]

Top5 <- rbind(Top_1,Top_2,Top_3,Top_4,Top_5)
nrow(Top5)

#Problem 4.3
Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)
Top5$LocationDescription

#TODO
#One of the locations has a much higher arrest rate than the 
#other locations. Which is it?

rate_alley <- nrow(Top5[Top5$Arrest == TRUE & Top5$LocationDescription == "ALLEY",])/nrow(Top5[Top5$LocationDescription == "ALLEY",])
rate_alley
rate_gas <- nrow(Top5[Top5$Arrest == TRUE & Top5$LocationDescription == "GAS STATION",])/nrow(Top5[Top5$LocationDescription == "GAS STATION",])
rate_gas
rate_street <- nrow(Top5[Top5$Arrest == TRUE & Top5$LocationDescription == "STREET",])/nrow(Top5[Top5$LocationDescription == "STREET",])
rate_street
rate_residential <-nrow(Top5[Top5$Arrest == TRUE & Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL",])/nrow(Top5[Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL",])
rate_residential
rate_parking <- nrow(Top5[Top5$Arrest == TRUE & Top5$LocationDescription == "STREET",])/nrow(Top5[Top5$LocationDescription == "STREET",])
rate_parking

areas <- c("ALLEY", "GAS", "PARKING","RESIDENTIAL","STREET")
rates <- c(rate_alley, rate_gas, rate_parking,rate_residential,rate_street)

areas_name <- "areas"
rates_name <- "rates"

rates_df <- data.frame(areas,rates)
names(rates_df) <- c(areas_name,rates_name)

rates_df

#ANSWER : Gas station

#On which day of the week do the most motor vehicle thefts 
#at gas stations happen?
gas_thefts <- Top5[Top5$LocationDescription == "GAS STATION",]
which.max(table(gas_thefts$Weekday))
#ANSWER : Saturday

#On which day of the week do the fewest motor vehicle thefts in 
#residential driveways happen? 
res_thefts <- Top5[Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL",]
which.min(table(res_thefts$Weekday))
#ANSWER : Saturday





