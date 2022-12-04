# ----------------------------------#
#           Week 03--Day05          #
#          Bar Graph Review         #
#      Histogram (Distribution)     #
#-----------------------------------#

#==================================================#
# Part 3: Visualizing Frequency/Amount--Bar graph  #
#                        Review                    #
#==================================================#

### (3.1) Visualizing frequency ###

### Day04 Material ###
## (1) Bar Graph Intro
library(tidyverse)
head(diamonds)

## Make a bargraph with "geom_bar" for variable color in diamonds data
## add a centered title and change the labels, flip the coordinator
barChart2 <- ggplot() +
  geom_bar(aes(x=color), data=diamonds) +
  labs(y="Number of Diamonds", x="Color") + 
  ggtitle("Bar Graph of Diamond Colors") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()
barChart2


## Work on the summarized data
color_summary <- diamonds %>% group_by(color) %>% summarise(n=n())

## make a bargraph with "geom_col" and "geom_bar" for variable color
ggplot() +
  geom_col(aes(x=color, y=n), data=color_summary)

ggplot() +
  geom_col(aes(x=color, y=n), data=color_summary, stat = "identity")




barChart2 <- ggplot() +
  geom_bar(aes(x=color), data=diamonds) +
  labs(y="Number of Diamonds", x="Color") + 
  ggtitle("Bar Graph of Diamond Colors") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()
barChart2


### (3.2) Visualizing amount ###

## Use "stat_summary" to make the bar graph for the median price for each type of color

ggplot() +
stat_summary(aes(x=color,y=price), data=diamonds,fun="median",geom="bar")

## Work on the summarized data and use 'geom_col' 

median.price <- diamonds %>% group_by(color) %>% summarise(med_price=median(price))
ggplot() +
  geom_col(aes(x=color,y=med_price),data=median.price)


#==============================================#
#       Part 4: Visualizing Distribution       #
# Histograms, Density, Boxplots, Violin Plot  #
#      (Fundamentals of dataVis: Chapter 7)    #
#==============================================#
## Note: Visualizing distributions are usually for 
## continuous (quantitative) variables. For 
## qualitative variables, the bargraph that shows
## the frequency of each categories is the distribution. 

### (4.1) Histograms ###
## An approximate representation of the distribution of numerical data.
## Continuous variable (quantitative variable)
## See example: https://www.mathsisfun.com/data/histograms.html

## Bar graphs can be viewed as a method to visualize distribution 
## of categorical variable.

## (1) Histogram Intro
?geom_histogram

head(diamonds)
# Make a Histogram for variable price in diamonds dataset
ggplot() +
  geom_histogram(aes(x=price), data=diamonds,binwidth=120)


# Note the warning 'bins=30' is the default. 
# We can improve (the appearance) of histograms by changing 
# the binwidth parameter

## (2) Change color/binwidth:
ggplot() +
  geom_histogram(aes(x=price), data=diamonds,binwidth=100,fill="blue")

# Use "fill" instead of "color"
ggplot() +
  geom_histogram(aes(x=price, fill=cut), data=diamonds,binwidth=100)


# Follow similar rule as "color" when specifying aesthestic 
# options inside and outside "aes()"



### (4.2) Density Plots ###
# What is the density function?
# https://en.wikipedia.org/wiki/Probability_density_function
?geom_density
# Question: What is kernel density estimates?
# application of kernel smoothing for the for the probability density estimation   

# Please refer to the following for definition
# https://en.wikipedia.org/wiki/Kernel_density_estimation

ggplot() +
  geom_density(aes(x=carat), data=diamonds,adjust=4, size=1.0)


# Default bandwidth is the standard deviation of the smoothing kernel
# change bandwidth/size of the line (change the value for "adjust=")
# Try adjust=0.2,1,3,6, etc


# Using line instead of bars (e.g. histogram) has advantage of stackability
ggplot() +
  geom_density(aes(x=carat, fill=cut), data=diamonds,alpha=0.5)


library(carData)
head(Blackmore)



head(summarizedData)

ggplot() +
  geom_density(aes(x=exercise,fill=group), data=Blackmore,alpha=0.5)



