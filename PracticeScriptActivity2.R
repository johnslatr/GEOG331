#Activity 2 Practice 

# set working directory to noaa folder 
setwd("Z:/students/jslater/Data/noaa_weather/")

# Make a vector of tree heights in meters
heights <- c(30,41,20,22)

# Convert to cm
heights_cm <- heights*100

# Set up a matrix 
Mat <- matrix(c(1,2,3,4,5,5),ncol=2,byrow=TRUE)

#Question 2 example vectors 
numeric <- c(31.0,41.555,1.222,88.88,0.00001)
characters <- c("Gabe","Sits","To","My","Right")
integers <- c(1,2,3,4,5)
sports <- factor(c("basketball","football","basketball","football"))

# read weather data 
datW <- read.csv("Z:/students/jslater/Data/noaa_weather/2011124.csv")

# Change date format
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

# Add year
datW$year <- as.numeric(format(datW$dateF,"%Y"))

# Make places a factor
datW$NAME<- as.factor(datW$NAME)

# Unique site names
unique(datW$NAME)

# Mean max temp in Celcius for Aberdeen ignoring NA values
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#  calculate average
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

# Change column names in averageTemp output
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

# Change the site variable from factor to vector 
datW$siteN <- as.numeric(datW$NAME)

# put all graphs together 
par(mfrow=c(2,2))

# Aberdeen Histogram
h1 <- hist(datW$TAVE[datW$NAME == "ABERDEEN, WA US"],
        freq=FALSE, 
        main = paste(levels(datW$NAME)[1]),
        xlab = "Average daily temperature (degrees C)", 
        ylab="Relative frequency",
        col="grey50",
        border="white")
#add mean line with red (tomato3) color and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "ABERDEEN, WA US"],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "ABERDEEN, WA US"],na.rm=TRUE) - sd(datW$TAVE[datW$NAME == "ABERDEEN, WA US"],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "ABERDEEN, WA US"],na.rm=TRUE) + sd(datW$TAVE[datW$NAME == "ABERDEEN, WA US"],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$NAME == "ABERDEEN, WA US"],na.rm=TRUE),
                 sd(datW$TAVE[datW$NAME == "ABERDEEN, WA US"],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

# Morrisville Histogram
h2 <- hist(datW$TAVE[datW$NAME == "MORRISVILLE 6 SW, NY US"],
        freq=FALSE, 
        main = paste(levels(datW$NAME)[2]),
        xlab = "Average daily temperature (degrees C)", 
        ylab="Relative frequency",
        col="purple",
        border="white")
#add mean line with red (tomato3) color and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "MORRISVILLE 6 SW, NY US"],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "MORRISVILLE 6 SW, NY US"],na.rm=TRUE) - sd(datW$TAVE[datW$NAME == "MORRISVILLE 6 SW, NY US"],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "MORRISVILLE 6 SW, NY US"],na.rm=TRUE) + sd(datW$TAVE[datW$NAME == "MORRISVILLE 6 SW, NY US"],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# Livermore Histogram
h3 <- hist(datW$TAVE[datW$NAME == "LIVERMORE, CA US"],
        freq=FALSE, 
        main = paste(levels(datW$NAME)[3]),
        xlab = "Average daily temperature (degrees C)", 
        ylab="Relative frequency",
        col="blue",
        border="white")
#add mean line with red (tomato3) color and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "LIVERMORE, CA US"],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "LIVERMORE, CA US"],na.rm=TRUE) - sd(datW$TAVE[datW$NAME == "LIVERMORE, CA US"],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "LIVERMORE, CA US"],na.rm=TRUE) + sd(datW$TAVE[datW$NAME == "LIVERMORE, CA US"],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


# Mormon Flat Histogram
h4 <- hist(datW$TAVE[datW$NAME == "MORMON FLAT, AZ US"],
        freq=FALSE, 
        main = paste(levels(datW$NAME)[4]),
        xlab = "Average daily temperature (degrees C)", 
        ylab="Relative frequency",
        col="green",
        border="white")
#add mean line with red (tomato3) color and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "MORMON FLAT, AZ US"],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "MORMON FLAT, AZ US"],na.rm=TRUE) - sd(datW$TAVE[datW$NAME == "MORMON FLAT, AZ US"],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$NAME == "MORMON FLAT, AZ US"],na.rm=TRUE) + sd(datW$TAVE[datW$NAME == "MORMON FLAT, AZ US"],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

