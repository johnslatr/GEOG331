### Beginning of Activity 5 ###

#load in lubridate
library(lubridate)

#read in streamflow data
datH <- read.csv("Z:/students/jslater/Data/streamflow/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("Z:/students/jslater/Data/streamflow/2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#define time for streamflow

#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#define time for precipitation   
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#get decimal formats 
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#basic formatting - define avg discharge and SD 
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
#bigger margins
par(mai=c(1,1,1,1))
#make plot of day of year vs avg discharge
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
#### Question 5 ####
lines(datD$discharge[datD$year == 2017],
      col= rgb(200/255,30/255,100/255,.5), pch=15)
# add polygons to show the data within 1 SD of the average plot
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
# adjust the axis display - set interval and set ticks
axis(1, seq(15,345, by=30), #tick intervals
     lab=seq(15,345, by=30)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
# add a legend to the plot 
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n") #no legend border

#### Question 7 ####
# count every day's measurements 
precip <- aggregate(datP$hour, by = list(datP$year, datP$doy), FUN = "length")

# to add to the plot, add a decimal day variable to data frame
precip$decYear <- ifelse(leap_year(precip$Group.1), precip$Group.1 + (precip$Group.2/366),
                         precip$Group.1 + (precip$Group.2/365))
# use dplyr to filter days with 24 measurements 
library(dplyr)
precipFullMeasure <- filter(precip, x == 24, .preserve = FALSE)

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")), ylim = c(15,425))

# use a for loop to add indicators of 24 hr measurement days 
for (i in precipFullMeasure){
  abline(v=i, col = rgb(200/255,30/255,100/255,.3) )
}
# add a legend for our plot
legend("topright", c("24hr day"), #legend items
       lwd=c(1,NA),#lines
       col=c(rgb(200/255,30/255,100/255,.3)),#colors
       pch=c(NA,15),#symbols
       bty="n") #no legend border

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#### Question 8 ####
#subsest discharge and precipitation within range of interest
hydroDq8 <- datD[datD$doy >= 3 & datD$doy < 5 & datD$year == 2012,]
hydroPq8 <- datP[datP$doy >= 3 & datP$doy < 5 & datP$year == 2012,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
ylq8 <- floor(min(hydroDq8$discharge))-1
#ceiling rounds up to the integer
yhq8 <- ceiling(max(hydroDq8$discharge))+1
#minimum and maximum range of precipitation to plot
plq8 <- 0
pmq8 <-  ceiling(max(hydroPq8$HPCP))+.5
#scale precipitation to fit on the 
hydroPq8$pscale <- (((yhq8-ylq8)/(pmq8-plq8)) * hydroPq8$HPCP) + ylq8

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroDq8$decDay,
     hydroDq8$discharge, 
     type="l", 
     ylim=c(ylq8,yhq8), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroPq8)){
  polygon(c(hydroPq8$decDay[i]-0.017,hydroPq8$decDay[i]-0.017,
            hydroPq8$decDay[i]+0.017,hydroPq8$decDay[i]+0.017),
          c(ylq8,hydroPq8$pscale[i],hydroPq8$pscale[i],ylq8),
          col=rgb(0.392, 0.584, 0.929), border=NA)
}

# use ggplot
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

#### Question 9 ####
# create a data frame with the desired data only from 2016
datD2016 <- data.frame(datD$doy[datD$year == 2016], 
                       datD$discharge[datD$year == 2016])

#edit the column names 
colnames(datD2016) <- c('doy', 'discharge')

#use multiple ifelse statements to define the seasons based on the day of year 
datD2016$season <- ifelse((datD2016$doy < 80), "winter",
                    ifelse((datD2016$doy < 170), "spring",
                    ifelse((datD2016$doy < 260), "summer",
                    ifelse((datD2016$doy < 330), "fall", "winter"))))

#make a violin plot using the seasonal data 
ggplot(data= datD2016, aes(season,discharge)) + 
  geom_violin() + labs(title = "2016 Stream Flow Violin Plot")

# repeat the process above for 2017
datD2017 <- data.frame(datD$doy[datD$year == 2017], 
                       datD$discharge[datD$year == 2017])

colnames(datD2017) <- c('doy', 'discharge')

datD2017$season <- ifelse((datD2017$doy < 80), "winter",
                    ifelse((datD2017$doy < 170), "spring",
                      ifelse((datD2017$doy < 260), "summer",
                        ifelse((datD2017$doy < 330), "fall", "winter"))))

#make a violin plot
ggplot(data= datD2017, aes(season,discharge)) + 
  geom_violin() + labs(title = "2017 Stream Flow Violin Plot")