# -------------------------------#
#        Week 03--Day06          #
#   Visualizing Distribution     #
# Histograms+Density Plot Review #
#     Boxplots+Violin Plot       #
#--------------------------------#


#==============================================#
#     Histograms + Density Plot  Review        #
#==============================================#

library(tidyverse)

### (4.1) Histograms ###
# make a histogram for variable Sepal.Length in iris dataset, pick an appropriate binwidth
# add more features like the labels, title, color et al
# any idea about the distribution? 

ggplot() +
  geom_histogram(aes(x=Sepal.Length, fill=Species), data=iris,binwidth=0.2) +
  labs(y="Count", x="Length of Sepal") + 
  ggtitle("Bar Graph of Iris Sepal Length") +
  theme(plot.title = element_text(hjust = 0.5)) 





### (4.2) Density Plots ###

# make a density plot for variable Sepal.Length in iris dataset, pick an appropriate bandwidth
# any idea about the distribution? 
ggplot() +
  geom_density(aes(x=Sepal.Length, fill=Species), data=iris,adjust=2, alpha=0.5) +
  labs(y="Count", x="Length of Sepal") + 
  ggtitle("Bar Graph of Iris Sepal Length") +
  theme(plot.title = element_text(hjust = 0.5)) 




### (4.3) Boxplots ###
# What is boxplot? https://www.khanacademy.org/math/statistics-probability/summarizing-quantitative-data/box-whisker-plots/a/box-plot-review

## (1) Simple boxplot for price
?geom_boxplot

# Median, hinge: (Q1, Q3), whiskers: (Q1-1.5*IQR, Q3+1.5*IQR) 
# IQR: interquartile range, IQR=Q3-Q1
# outliers

ggplot() +
  geom_boxplot(aes(x=Sepal.Length, fill=Species),data = iris)+
  labs(x="Length of Sepal") + 
  ggtitle("Box Plot of Iris Sepal Length by Species") +
  theme(plot.title = element_text(hjust = 0.5))


## (2) Side-by-side for each clarity category
ggplot() +
  geom_boxplot(aes(x=log10(price), y=clarity, fill=cut),data = diamonds)+
  labs(x="Length of Sepal") + 
  ggtitle("Box Plot of Iris Sepal Length by Species") +
  theme(plot.title = element_text(hjust = 0.5))
 # Advantages and disadvantages?
# Advantages:
# plot is more efficient
# view the 5 number summary very straight forwards
#
# Disadvantages


## Any problems with the data? Is there an alternative way to view this?



## (3) Options: scale, color, fill, shape and size of outlier, orientation, etc


## Question: What are the advantages and disadvantages of the boxplot?
## advantage:

## disadvantage: 


### (4.4) Violin Plots ###

## Violin plots are similar to box plots, except that they also 
## show the mirrored probability density of the data at different values, 
## usually smoothed by a kernel density estimator.
## https://en.wikipedia.org/wiki/Violin_plot#:~:text=A%20violin%20plot%20is%20a,by%20a%20kernel%20density%20estimator.
## https://datavizcatalogue.com/methods/violin_plot.html

?geom_violin # By default, R only display the mirrored probablility density.

## (1) Violin plot introduction
ggplot() +
  geom_violin(aes(x=cut, y=carat), color="blue",fill="lightblue",data=diamonds)

ggplot() +
  geom_violin(aes(x=cut, y=carat, fill=cut), color="blue",data=diamonds) +
  geom_boxplot(aes(x=cut, y=carat), width=0.3, data = diamonds)


## color: controls the color of the border, fill controls what 
## fill: controls what color fills the violine
## theme_bw(): change the theme of the background
## more options: http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
## Can also change the scale similar as the density plot.

## (2) Combine violin plots with boxplots


## Anything wrong? How to fix it?



## (3) Other options
## You can also customize the graph by changing color, scale, fill, etc
## scale_fill_manual() for box plot, bar plot, violin plot, dot plot, etc
  

# scale_fill_manual(values=c("#FFDB6D", "#C4961A", "#F4EDCA", 
#                             "#D16103", "#C3D7A4", "#52854C", "#4E84C4","#293352"))


## https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/


## For future design principals class: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/




ggplot() +
  geom_violin(aes(x=Sepal.Length, y=Species, fill=Species),data=iris) +
  geom_boxplot(aes(x=Sepal.Length, y=Species), width=0.3, data = iris) + 
  labs(x="Length of Sepal") + 
  ggtitle("Box and Violin Plot of Iris Sepal Length by Species") +
  theme(plot.title = element_text(hjust = 0.5))



