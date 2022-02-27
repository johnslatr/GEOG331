# Activity 3

### Start of Exercise ###
#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")

#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

###### Exercise Begins ######
# Read in data 
datW <- read.csv("Z:/students/jslater/Data/bewkes/bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)

#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("Z:/students/jslater/Data/bewkes/bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#use install.packages to install lubridate
#install.packages(c("lubridate"))

library(lubridate)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil moisture
length(which(is.na(datW$soil.moisture)))

#soil temperature
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch) for soil moisture
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch) for air 
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#I'm going to make a new column to work with that indicates that I am conducting QAQC
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

# Show quantiles of air temperature
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy))
* datW$lightning.acvitivy

#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")

#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

### Question 5 ###
# Prove we can use lightscale by saying lightscale has the same dates
assert(!is.null(datW$DD[lightscale>0]), "error = not part of subset")

assert(length(lightscale) == length(datW$precipitation), "error: Does not contain components because they're unequal lengths")

### Question 6 ###
#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    

#create a new air temp column, example code to follow for question 
datW$air.tempQ2 <-  ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

# Do the same as above to get rid of stormy wind
datW$wind.speedQ1 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                            ifelse(datW$precipitation > 5, NA, datW$wind.speed))

# Assert statement to check if ....
assert(length(datW$wind.speedQ1) == length(datW$air.tempQ2), "error = Unequal lengths")

# Plot the new corrected wind speed variable 
plot(datW$DD, datW$wind.speedQ1, xlab = "Day of Year", ylab = "Wind Speed (m/s)",
     type="n")
points(datW$DD[datW$wind.speedQ1 > 0], datW$wind.speedQ1[datW$wind.speedQ1 > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)
lines(datW$DD[datW$wind.speedQ1 > 0], datW$wind.speedQ1[datW$wind.speedQ1 > 0],
      col= rgb(200/255,30/255,100/255,.5), pch=15)

### Question 7 ###
datW$soil.tempQ1 <- 

datW$soil.moistureQ1 <-

### Question 8 ###
