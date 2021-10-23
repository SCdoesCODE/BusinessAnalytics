#creating a vector, a set of values that are of the same type
myvar <- c(72,28,-9,12,11)
myvar2 <- myvar*2
mean(myvar)
length(myvar)

#indexing a vector, vectors in R are 1-indexed
myvar[2]
#get a boolean vector with either TRUE or FALSE depending on the condition
myvar>2
#only get the elements from your vector that satisfied the condition
myvar[myvar>0]

#assign to a vector the even values from 1 to 10
z<-c(1:11)
z[z%%2==0]
#otherwise we can use seq
z<- seq(2,10,2)
z

#return all elements larger than 4
z[z>4]

#working with dataframes (collection of different vectors)
#csv are the easiest files to import in R
#go to the Environment window and click on Import Dataset and then from text(base)
#use the dollar sign notation to access a specific column
mean(BikeData$distance)
#use the table function to find the frequency of the different values in a column
#we use this for categorical columns/variables
table(BikeData$gender)

#Indexing dataframes [row,column]
BikeData[2,5]
#Get a specific column
BikeData[,5]
#look at the gender column and get a T/F stating if it is a female or not female
BikeData$gender =="F"
#we can now use this to get a dataframe with only the females
#however, we have to remember to put it in the row spot, because we want a 
#specific number of row but all the columns
females <- BikeData[BikeData$gender =="F",]
females
#only get the speeds of the female bikers
femalespeeds <- BikeData$speed[BikeData$gender == "F"]
femalespeeds

#new dataframe with only the cyclists that are employed
employed <- BikeData[BikeData$employed ==1,]
employed
length(employed[,1])
#vector with distances for employed cyclists
employed$distance




