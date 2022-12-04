# -----------------------------------------------#
#                 Week 09--Day17                 #
#               Gestalt principles               #
#------------------------------------------------#
#=================================#
# Part 10: Gestalt principles     #
#=================================#

library(tidyverse)

## (1) Poximity
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width),data=iris)

## (2) Similarity
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,color=Species),data=iris)

ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,shape=Species),data=iris)

## (3) Enclosure
?geom_rect
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width),data=iris)+
  geom_rect(aes(xmin=4.5, xmax=5.5, ymin=3.5, ymax = 4.5))
  
  ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width),data=iris)+
  
  
  
## (4) Closure
library(readxl)
stardata <- read_excel("stardata.xlsx")
head(stardata)
ggplot()+
  geom_point(aes(x=x,y=y),data=stardata)+
  theme_void()
# http://robertgrantstats.co.uk/drawmydata.html 
# This is a website that you can draw any pictur 
# using dot, then download the dataset.

## (5) Continuity
ggplot(aes(x = clarity),data = diamonds)+
  geom_bar()+
  theme_void()+  
  geom_text(aes(label=clarity),stat="count",
            position=position_stack(vjust=0.5),
            color="white")

## (6) Connection
?economics
head(economics)
library(lubridate)


## we'll plot the variable unemploy (number of unemployed 
## in thousands) by date (x-axis).
ggplot(aes(x=date,y=unemploy),data=economics)+
  geom_point()

## Mark the data by decades
new <- economics %>% 
  mutate() 
new
ggplot(aes(x=date,y=unemploy,shape=),data=new)+
  geom_point()

## Example of Decluttering
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,color=Species),data=iris)+
  ## https://felixfan.github.io/ggplot2-remove-grid-background-margin/
  
  
#=================================#
# Part 11: Fluctuation Diagrams   #
#=================================#
  
## Related with heatmap and the scatterplot.
## Same idea as heatmap, but let the size, not color, 
## determine the intensity
## In a scatteplot, the size of the point reflects the intensity/freqency

## Note: If x & y were numeric variables, this is called
## a "bubble plot" (will be discussed in the future)
## More informaiton about bubble plot: 
## https://www.data-to-viz.com/graph/bubble.html

##--------------------------##
## Summary table from previous lecture
summary <- diamonds %>%
  group_by(clarity, cut) %>%
  summarise(n=n())
summary
##---------------------------##


ggplot() + 
  geom_point(aes(x=clarity, y=cut, size=n), 
             shape=7, data=summary) + ## can customize
  scale_size_continuous(range=c(1,10)) + ## range specifies size of squares
  theme_bw()


#========================#
# Part 12: Mosaic Plots  #
#========================#
## Wikipedia Definition:
## https://en.wikipedia.org/wiki/Mosaic_plot#:~:text=A%20mosaic%20plot%20(also%20known,information%20for%20only%20one%20variable.
## A mosaic plot is a graphical display of the cell 
## frequencies of a contingency table in which the area
## of boxes of the plot are proportional to the cell 
## frequencies of the contingency table.

## Review ##
##----------------------------------------------------##
## Stacked bar graph
ggplot()+
  geom_bar(aes(x=cut, fill=clarity), position ="fill",
           data=diamonds)
##----------------------------------------------------##

## Difference between Mosaic plot and stacked bargraph
## https://www.uv.es/visualstats/vista-frames/help/lecturenotes/lecture02/repvis4a.html#:~:text=In%20a%20mosaic%20plot%2C%20each,same%20height%2C%20representing%20100%25.

##----------------------------------------------------##
## Summary2 table similar in previous lecture
## conditional clarity proportions by cut
summary2 <- summary %>%
  group_by(cut) %>%
  mutate(cutcount= sum(n)) %>%
  mutate(prop= n/cutcount) %>%
  ungroup() %>%
  arrange(cut)
summary2
##----------------------------------------------------##


#install.packages("ggmosaic")
library(ggmosaic)
help(package="ggmosaic")
?geom_mosaic
## https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html
# weight : select a weighting variable
#      x : select variables to add to formula
#          declared as x = product(x1, x2, …)
#   fill : select a variable to be filled
?product ## a wrapper for a list
class(diamonds$cut)
class(product(diamonds$cut))

ggplot() +
  geom_mosaic(aes(weight=prop, x=product(cut), fill=clarity), data=summary2)


## I can also use geom_mosiac on the raw data, not the counted frequencies


## The mosaic plot can show more types of relationships:
## https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html




