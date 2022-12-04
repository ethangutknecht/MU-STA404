# --------------------------------------------------#
#               Week 04--Day07                      #
#            Boxplot and Violin Plot Review         #
#              Bivariate Categorical Data           #
#              (Stacked/Grouped Bargraph,           #
#                 Add numbers on graph)             #
#---------------------------------------------------#

#==============================================#
#       Part 4: Boxplots, Violin Plot Review   #
#      (Fundamentals of dataVis: Chapter 7)    #
#==============================================#
library(tidyverse)

### (4.3) Boxplots ###
## (3) Options: scale, color, fill, shape and size of 
#      outlier, orientation, etc
## Make a boxplot for variable table in diamonds dataset

ggplot() +
  geom_violin(aes(x=price, y=log10(carat), fill=clarity),data=diamonds) +
  geom_boxplot(aes(x=price, y=log10(carat)), width=0.3, data = diamonds) + 
  labs(x="Price of Diamond") + 
  ggtitle("Box and Violin Plot of Diamond Price and Carat") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot() +
  geom_boxplot(aes(x=table, fill=cut), width=0.3, outlier.size=0.5, outlier.shape = 3, data = diamonds) + 
  coord_flip()



### (4.4) Violin Plots ###
## Make a violin for variable table in diamond dataset
## (1) Violin plot
ggplot() +
  geom_violin(aes(x=table,y=cut, fill=cut), width=0.3, data = diamonds) + 

## (2) Combine violins with boxplots 
ggplot() +
  geom_boxplot(aes(x=table,y=cut, fill=cut), width=0.3, outlier.size=0.5, outlier.shape = 3, data = diamonds) + 
  geom_violin(aes(x=table,y=cut, fill=cut), width=0.3, data = diamonds) + 
  coord_flip()



### (4.5) Summary ### 
# Summarize the types of plots, the number of variables involved, etc, 
# by yourselves. 

# Data Type     # of variables:        Type of graph to use:
# Categorical          1                bargraph
# Numerical            1                histogram/boxplot/density/voilin
# 1 Categorical
# and 1 numerical      2                  


#=================================================#
# Part 5: Displays for Bivariate Categorical Data #
#       (Fundamentals of datavis: Chapter 6)      #
#=================================================#
## Explore the data
head(diamonds)
str(diamonds)
## Select two categorical variables
cc_summary <- data.frame(table(diamonds$clarity, diamonds$cut))
cc_summary
colnames(cc_summary) <- c("clarity", "cut", "freq")
head(cc_summary)


### (5.1) Stacked bargraphs ###
## Show breakdown of a second categorical variable within each bar.

## (1) Height represents frequency
## a. Original data
ggplot()+
  geom_bar(aes(x=cut),data=diamonds)

ggplot() +
  geom_bar(aes(x=cut, fill=clarity),stat="count", position = "stack", data=diamonds)




## b. Summarized data

ggplot() +
  geom_bar(aes(x=cut, y=freq, fill=clarity),stat="identity", position = "stack", data=cc_summary)


## Any problems? 

class(cc_summary$clarity)
class(diamonds$clarity)
diamonds$clarity
cc_summary$clarity <- ordered(cc_summary$clarity, levels=c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"))

# If not very sure, you can first assign it to an R object x, then replace the 
# clarity variable in data cc_summary

ggplot() +
  geom_bar(aes(x=cut, y=freq, fill=clarity),stat="identity", position = "stack", data=cc_summary)


## (2) Proportionally Stacked-Bargraph
## a. Original data
## copy the stack case above and change the "stack" to "fill"
ggplot() +
  geom_bar(aes(x=cut, y=freq, fill=clarity),stat="identity", position = "fill", data=cc_summary)



## calculate the proportion (clarity.cut/ cut) for each value of cut,
## and to do that it uses the count statistic,
## but the actual value represented by the cumulative height of
## the different color bars is not a count.

## b. Summarized data
ggplot() +
  geom_bar(aes(x=cut, fill=clarity),stat="count", position = "fill", data=diamonds)

ggplot() +
  geom_bar(aes(x=cut,y=freq, fill=clarity),stat="identity", position = "fill", data=cc_summary)


## c. Similar idea can be applied to 
##    Multiple histograms with proportional representation

ggplot()+
  geom_histogram(aes(x=carat, y=..density..),binwidth=.1, data=diamonds)+
  facet_grid(cut ~ .)


#..density.. is a special variable in ggplot, can also try ..count..


### (5.2) Grouped bargraphs (similar to faceting) ###
## set position="dodge".
ggplot()+
  geom_bar(aes(x=cut, fill=clarity), 
           stat="count",position ="dodge",
           data=diamonds)
## you may also omit the stat="count"
## "dodge" display the graph side by side.




## Similar information via facet_grid
ggplot()+
  geom_bar(aes(x=clarity, fill=clarity), 
           data=diamonds, color="black") + 
  facet_grid(cut~., scale="free_y")

## "free_y" means that each row of facets can set its y-axis 
## independent of the other rows. 


### (5.3) Add numbers on top of graphs ###
## (1) Bargraph
## Start with the following
ggplot()+
  geom_bar(aes(x=cut),data=diamonds)


ggplot()+
  geom_bar(aes(x=cut),data=diamonds) +
  geom_text(aes(x=cut, label=..count..), data=diamonds, stat="count",vjust=-1)


# in class:
ggplot()+
  geom_bar(aes(x=cut,y=freq, fill=clarity), 
           stat="identity",position ="dodge",
           data=cc_summary)


# label: the value you want to display, can be ..count.., can be ..density..
# vjust: adjust where to put the numbers. 
# https://www.gl-li.com/2017/08/18/place-text-at-right-location/




## (2) Stacked bargraph


## adjust the display of the numbers




#position=: change the positions of where you want to put the number.
#size: how large the number is
## Similar idea for the proportionally stacked-bargraph 
# https://stackoverflow.com/questions/44724580/add-percentage-labels-to-a-stacked-barplot

## (3) Histogram
?stat_bin

ggplot(aes(x=price),data=diamonds)+
  geom_histogram()
  


#geom: The geometric object to use display the data. e.g. "bar","point","line"
#stat_bin: bin data, something related with what information you want to display, etc


## not happy with the display, maybe too many bins?




## move the number to other locations? maybe in the middle of the bars?
## also take a look at the x-axis, the numbers do not match with the bins.


## boundary=0: indicates you want to display the histogram beginning from 0.

## closed="right" or "left": specifies if a number occurs on the boundary, e.g. 3000,
##                           will it be put in the left bar, or the right bar. 
##                           right:(0,3000]   left:[0,3000)
## #scale_x_continuous(breaks=seq(min(diamonds$price),max(diamonds$price),3000))

## Refer to https://ggplot2.tidyverse.org/reference/geom_histogram.html



##color mark it by another variable


