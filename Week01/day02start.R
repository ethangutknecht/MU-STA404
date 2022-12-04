# -----------------------------#
#       Week 01--Day02         #
#  Introduction to data types  #
#------------------------------#

#=======================#
# Part 1: Basic R stuff #
#=======================#

### (1.1) Calculator and other stuff ###
2+4
4-2
4*5
10/5
log(10)
exp(2)


# You may use # to comment your R script.
# If you want to comment/uncomment a few lines together, use Ctrl+Shift+C (Window) or Command+Shift+C (Mac).
# This is some comments

### (1.2) Assignment  ###
# You can assign numbers, characters, etc to an object in R.
# Assignment is done via the <-operator (shortcut: Alt" + "-" (windows) and "Option"+"-" (mac))
# Ctrl+enter (windows) Ctrl+enter (mac)
x=3
x
x<-
x

xx <- 
xx

test <- 
test
test.score <- 
test.score
# Acceptable name may be made out of letters, numbers, and the period(.) symbol, 
# but names must start with a letter.
test2 <- 2
test2
# 2test <- 2 # This assignment is not appropriate
true <- 6
# TRUE <- 6 #TRUE is reserved for logical valeus



# vectors
y <- c(2, 5, 1, 3, 4) #c is used to create a vector
y1 <- rep(1, 6) # repeated values
y1
y2 <- 1:10
y2
y3 <- seq(1,10,by=0.5)
y3
y4 <- seq(10, 1,-0.5)
y4
y5 <- c(y1, y)
y5
y5[3]
mean(y5)
sd(y5)
median(y5)

# R uses the notation `[n]' at the beginning 
# of each output line to indicate which entries are shown on that line.
test.subjects <- c("Economics","Math","English")
test.subjects

mix <- c(6, "Seven")
mix


### (1.3) Different data types ###
## (1) vector (numeric, character,logic, etc)

class(mix)
class(y5)
h <- c(TRUE, FALSE, TRUE)
class(h)

class(h) <- "group"
class(h)




## (2) Matrix
mat <- matrix(1:12, nrow=4)
mat
mat1 <- matrix(1:12,nrow=4,byrow=TRUE)
mat1
mat2 <- cbind(1:4, 2:5)
mat2
mat3 <- matrix(1:12, nrow=4, ncol=4)
mat3
mat4 <- cbind(1:4, 2:4) #second column will be recycled
mat4
mat4[3,1]
mat4[,1]
mat4[2,]

mat5 <- matrix(c("Red", "Blue", "Green", "Yellow")2,2)
mat5

# However, matrix could not have a mix of numeric and characteristic columns.
mat <- matrix(c(seq(1,16,1),rep("2",4)),nrow=4)
mat #all the first 16 nubers are transformed to character


## (3) Arrary
# Array can have multiple dimenions, but generally similar like matrix
array <- array(1:3, dim=2:4)
array1

array2 <-array(1:24, dim=c(2,3,4))
array2





## (4) Data frame
# Similar as matrix, Different columns can have different characters
matrix(c(seq(1,3,1),c("A","B","C")),nrow=3)

test.data <- data.frame(subjects=c("English", "Math", "Econ"),
                        registration=c(TRUE,TRUE,TRUE),
                        score=c(98,92,NA))
  
people.info <- data.frame(name=c("Ethan", "John", "Mark", "Carson", "Peter"),
                        age=c(20,30,22,45,36),
                        married=c(FALSE, TRUE, FALSE,FALSE,FALSE))
people.info[2,3]



test.data
class(test.data)
class(test.data$subjects)
class(test.data$registration)
class(test.data$score)

?str 
str(test.data)

?iris


# Question: what is a factor?

## (5) Factor
# Refer to this website for more details:
# Berkeley: https://www.stat.berkeley.edu/~s133/factors.html
# UCLA: https://stats.idre.ucla.edu/r/modules/factor-variables/
# Factors in R are stored as a vector of integer values with a 
# corresponding set of character values to use when the factor is displayed. 
# categorical variable: a.without order --nominal -- Red, Blue, Orange
#                       b.with order --  ordinal -- Freshman, Sophomore, Junior, Senior
#                      

## Nominal factor
color.vec <- c("Red", "Blue", "Yellow", "Red", "Yellow")
class(color.vec)
color.fac <- factor(color)
color.fac
factor(color.vec, levels = c("Red","Blue","Yelow"))



# the levels are ordered according to the alphabetical order of 
# all the elements appeared in color.vec. Want to change the order?



# Change the factor labels
x <- c(1,3,5,2,4,3,2)
factor(x)
factor(x, labels = "A", "B", "C", "D", "E", levels = c(5,1,4,3,2))




## Ordinal factor
dose.vec <- c("Dose1", "Dose2", "Dose3")
ordered(dose.vec)
ordered(dose.vec, levels=c("Dose2", "Dose1", "Dose3"))



## Advantages of using factor variables
# (1) Useful in statistical models, DF can be computed correctly.
# (2) Useful in different types of graphics.
# (3) Storing string variables as factor variables is a more efficient use of memory. 


#==============================================#
# Part 2: Visualizing Association between Two  #
#         Quantitative Variables--Scatter Plot #
#    (Fundamentals of dataVis: Chapter 12)     #
#==============================================#

## Quantitative Variable: numerical values and represent some kind of measurement. 
## e.g. # of students in a class, time, weight, etc.
## Refer to more information at: https://www.statisticshowto.com/quantitative-variables-data/
##                        or at: https://online.stat.psu.edu/stat200/lesson/1/1.1/1.1.1

# install.packages("tidyverse")
library(tidyverse)
?diamonds #look for the help file
help(diamonds)
head(diamonds)
#tibble: morden version of data frames, they tweak some older behaviours to make life a little easier.
#refer to https://tibble.tidyverse.org/ for more details



#This is like a transposed version of print(): 
#columns run down the page, and data runs across. This makes it possible to see every column in a data frame



### (2.1) Scatter plot intro ###

## Relationship between carat & price?

# ggplot2: a system for declaratively creating graphics.
# "gg": Grammar of Graphics. 
# You provide the data, tell ggplot2 how to map variables to aesthetics, 
# what graphical primitives to use, and it plots the data.

# first, start with a blank canvas



# Second, add a layer of geometric features to the blank canvas
# via "aesthetic mapping". Specify values through "aes()"





## Question: How to add additional aesthetic options?

### (2.2) Color ###
## (1) Color Intro


## what's wrong with the plot?

## you can map (inside aes) a varialbe of your data to an aesthetic, e.g. (aes(color=cut))
## you can set (outside aes, but inside a geom element) an aesthetic to a constant value, e.g. "blue"


## (2) Self defined colors 
# If you don't like the default color, you can also specify it
# scale_color_manual can be used for lines and points



# Search for other color schemes
# REG triplet/hexadecimal format

# Refer for this for more details about the colors 
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
# Design colorblind-friendly visualizations.
# About 1 in 12 men (8%) and 1 in 200 women in the world are color blind.




### (2.3) Overplotting (change point size, transparancy scales) ###
# Recall
ggplot() +
  geom_point(aes(x=carat,y=price),data=diamonds)
## Any problems?

# Potential problem: overplotting
# a. Can summarize # of points at each location 
# and display that in some way, using "geom_count()"
# refer more here: https://ggplot2.tidyverse.org/reference/geom_point.html
#             and: https://ggplot2.tidyverse.org/reference/geom_count.html



# b. Add alpha blending

# 1: not transparent, usually used to 
# down-weight less important observations


### (2.4) Add labels ###
# adding labels for the axes can also be regarded as a layer




### (2.5) Add titles ###



### (2.6) Change size/shape of point ###
#http://www.sthda.com/english/wiki/ggplot2-point-shapes




#size - (default: 0.5) diameter of the point
#shape - (default: 16=dot) the shape of the point

### (2.7) Legends ###
# Change name of the legend





### (2.8) Display Layout panels in a grid ###
# https://plot.ly/ggplot2/facet_grid/
# syntax: facet_grid(var_vertical~var_horizontal)

## (1) Vertical direction





## (2) Horizontal direction



