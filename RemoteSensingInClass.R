# in-class remote sensing 

# load terra package 
library(terra)

#set working directory 
setwd("Z:/students/jslater")

# read a raster file 
p <- rast("Z:/students/jslater/Data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")

# plot the raster 
plot(p)

# plot an rgb rendering of the data 
plotRGB(p, r=3, g=2, b=1)

# plot an rgb rendering of the data 
plotRGB(p, r=3, g=2, b=1, 
        scale = 65535, # to fix color intensity problem
        stretch = "hist") #for contrast because values for RGB are closer to zero 

# read file with field observations of canopy cover 
tree <- read.csv("Z:/students/jslater/Data/rs_data/siberia_stand_data.csv", header= T)

#convert to vector object using terra package 
gtree <- vect(tree, geom = c("Long", "Lat"), "epsg:4326")

# project the data to match the coordinate system of the raster layer 
gtree2 <- project(gtree, p)

#create a polygon from the extent of the points 
b <- as.lines(ext(gtree), "epsg:4326")

# reproject the polygons to the same projection as our raster 
b2 <- project(b,crs(p))

# buffer the extent by 200m
b3 <- buffer(b2, width = 200)