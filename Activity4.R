#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
install.packages(c("dplyr"))
install.packages(c("ggplot2"))

library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

flower <- iris[iris$Species == "versicolor",]
firstVec <- list(flower$Sepal.Length, flower$Petal.Length, flower$Sepal.Length)
secondVec <- list(flower$Sepal.Width, flower$Petal.Width, flower$Petal.Length)
fit <- list()

for (i in 1:3) {
  fit[i] <- lm(unlist(firstVec[i]) ~ unlist(secondVec[i]))
}

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

joined_height <- full_join(iris, height)

#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() 

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################