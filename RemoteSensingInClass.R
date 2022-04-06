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