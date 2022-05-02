# install -- Only do once!
#install.packages('R.utils')
install.packages("BiocManager")
install.packages("neonUtilities")

# Libraries
library(data.table)
library("rhdf5")
library(neonUtilities)

# set working directory
setwd("Z:/students/jslater/")


# saving file paths
f <- "Z:/GEOG331_S22/students/jslater/Data/NEON_eddy-flux/NEON.D03.DSNY.DP4.00200.001.2017-03.basic.20220120T173946Z.RELEASE-2022/NEON.D03.DSNY.DP4.00200.001.nsae.2017-03.basic.20211220T152138Z.h5/NEON.D03.DSNY.DP4.00200.001.nsae.2017-03.basic.20211220T152138Z.h5"
fileUrl <- "Z:/students/jslater/Data/NEON_eddy-flux/NEON.D03.DSNY.DP4.00200.001.2017-03.basic.20220120T173946Z.RELEASE-2022/NEON.D03.DSNY.DP4.00200.001.nsae.2017-03.basic.20211220T152138Z.h5/NEON.D03.DSNY.DP4.00200.001.nsae.2017-03.basic.20211220T152138Z.h5"
fileUrl2 <- "Z:/students/jslater/Data/NEON_eddy-flux/NEON.D03.DSNY.DP4.00200.001.2020-07.basic.20220120T173946Z.RELEASE-2022/NEON.D03.DSNY.DP4.00200.001.nsae.2020-07.basic.20211220T175101Z.h5"



# Read in data to flux variable
# MARCH 2017
flux <- stackEddy(filepath = fileUrl,
                  level = "dp04")
# JULY 2020
flux2 <- stackEddy(filepath = fileUrl2,
                   level = "dp04")

# Plot Graph
p <- plot(flux2$DSNY$data.fluxCo2.nsae.flux ~ flux$DSNY$timeBgn, 
     type="l", pch=".", xlab="Time", ylab="CO2 flux")

integrate(p)

plot(flux2$DSNY$data.fluxCo2.stor.flux ~ flux2$DSNY$timeBgn,
     type="l", pch=".", xlab="Time", ylab="CO2 Storage")


# Unzip files 
unzip(zipfile, files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)

# Read in temperature files

tdata <- read.csv("Z:/students/jslater/Data/NEON_temp-air-triple/NEON.D03.DSNY.DP1.00003.001.2017-01.basic.20220120T173946Z.RELEASE-2022/NEON.D03.DSNY.DP1.00003.001.000.040.030.TAAT_30min.2017-01.basic.20211210T223026Z.csv")

library(lubridate)

#convert dates to usable format using strptime, specifying each part

plot(tdata$startDateTim ~ tdata$tempTripleMean, 
     type="p", pch=".", xlab="Date", ylab="Temperature", ylim = c(-20,40))
