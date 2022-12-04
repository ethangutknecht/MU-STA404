# -----------------------------------------------#
#                 Week 08--Day15                 #
#                 Review Tables                  #
#                Heatmap Continue                #
#                  Slopegraph                    #
#------------------------------------------------#

#============================================#
# Part 9: Tables, Heatmap and Slopegraph     #
#============================================#

### (9.1) Tables  ###
library(tidyverse)

diamonds

## (1) Frequency table from day 06

## In day07, we took a look at the summary
## table corresponding to two variables 
table(diamonds$clarity, diamonds$cut)
## This is a wide formatted table

## (2) Create similar summary table in tidyverse
## a. This is a long formatted table
summary <- diamonds %>% 
  group_by(clarity,cut) %>% 
  summarise(n=n())

## b. Change it to wide format
summary %>% pivot_wider(names_from = cut, values_from = n)


### (9.2) Heatmap  ###
## Refer to this website for some introduction:
## https://en.wikipedia.org/wiki/Heat_map

## (1) View overall correlation heatmap
## https://boxuancui.github.io/DataExplorer/index.html
install.packages("DataExplorer")
library(DataExplorer)
plot_correlation(iris)
## This is usually useful in the data exploration stage.


## (2) Heatmap for the frequencies of the clarity and cut combination
?geom_tile
?scale_fill_gradient

summary %>%
  ggplot() +
  geom_tile(aes(x = clarity, y = cut, fill = n)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(fill = "Frequency")


## Many heatmap we saw have two different colors for lower and higher values
## Can we change it?

?scale_fill_gradient2
# https://www.rdocumentation.org/packages/ggplot2/versions/1.0.0/topics/scale_colour_gradient2

summary %>%
  ggplot() +
  geom_tile(aes(x = clarity, y = cut, fill = n)) +
  scale_fill_gradient2(low = scales::muted("blue"),
                       mid = "white", 
                       high = scales::muted("red")) +
  labs(fill = "Frequency")

# No blue, Why? Because no value in the frequency n is larger than 0
# and default middle point is zero

# How do we fix it?:
# Number 1
summary %>%
  ggplot() +
  geom_tile(aes(x = clarity, y = cut, fill = n)) +
  scale_fill_gradient2(low = scales::muted("blue"),
                       mid = "white", 
                       high = scales::muted("red"),
                       midpoint = mean(summary$n)) +
  labs(fill = "Frequency")


# Number 2
summary <- summary %>% 
  mutate(n2 = scale(n,center=TRUE, scale=TRUE))
# scale uses to scale data by forumula 

summary %>%
  ggplot() +
  geom_tile(aes(x = clarity, y = cut, fill = n2)) +
  scale_fill_gradient2(low = scales::muted("blue"),
                       mid = "white", 
                       high = scales::muted("red")) +
  labs(fill = "Frequency")
# Do not match because the data sets are grouped

summary2 <- summary %>% 
  ungroup() %>% 
  mutate(n3=scale(n, center=TRUE, scale = TRUE))

head(summary2)

summary2 %>%
  ggplot() +
  geom_tile(aes(x = clarity, y = cut, fill = n3)) +
  scale_fill_gradient2(low = scales::muted("blue"),
                       mid = "white", 
                       high = scales::muted("red")) +
  labs(fill = "Frequency")

?scale_fill_gradientn
range(summary$n)

summary %>%
  ggplot() +
  geom_tile(aes(x = clarity, y = cut, fill = n)) +
  scale_fill_gradient(breaks=c(9, 1500,3000,5071)) +
  colours = c(scales::muted("blue"),
              "yellow",
              "green",
              scales::muted("red"))) +
  labs(fill = "Frequency")

## (3) Heatmap for the density of clarity 
##     for each cut type 

## a. Long formatted table
diamonds %>%
  group_by(clarity, cut) %>%
  summarise(n=n()) 

summary2 <- diamonds %>% 
  group_by(clarity, cut) %>% 
  summarise(n=n()) %>% 
  group_by(cut) %>% 
  mutate(sum_cla=sum(n),
         cldensity=n/sum(n)) %>% 
  select(clarity, cut, cldensity)

head(summary2)



## b. Change it to wide format
summary2 %>% 
  pivot_wider(names_from=clarity,values_from=cldensity)


summary2 %>% 
  ggplot() +
  geom_tile(aes(x = clarity, y = cut, fill=cldensity)) +
  scale_fill_gradient(low="white", high = "dark green") +
  labs (fill="Density")




### (9.3) Slopegraph  ###

## (1) Introduction
## What is a slopegraph?
## https://www.betterevaluation.org/en/evaluation-options/slopegraph#:~:text=A%20slopegraph%20is%20a%20lot,at%20interpreting%20changes%20in%20direction.

## (2) Data preparation
## Drug Poisoning Mortality in the United States, 
## 2003-2010
## https://www.cdc.gov/nchs/data-visualization/drug-poisoning-mortality/
## Download the following dataset directly from webstie
## "NCHS_-_Drug_Poisoning_Mortality_by_State__United_States"

drug_poison <- read_csv("NCHS_-_Drug_Poisoning_Mortality_by_State__United_States.csv")
head(drug_poison) ##take a look at the data
names(drug_poison)
data0320 <- drug_poison %>% 
  filter(Year==2003|Year==2020)



data0320


## (3) Create a slopegraph
ggplot() + 
  geom_line(aes(),
            data=data0320)

?get()
?geom_line
# get(): Return the value of a named object




## (3) Clean up the plot and add appropriate information
colnames(data0320)=c("Year","State","Rate")

ggplot() + 
  geom_line(aes(x=Year, y=Rate, group=State), data=data0320) +
  labs(y="Average Model Based Death Rate",
       title="Drug Poisoning Mortality") + 
  theme(plot.title = (element_text(hjust=0.5)))+
  
  
  ?coord_cartesian

## (4) Highlight Ohio?





### (9.4) Ohter graphs  ###

## (1) Waterfall bar chart
## Often used in analytical purposes in the business 
## setting to show the effect of sequentially introduced 
## negative and/or positive values. Sometimes waterfall 
## charts are also referred to as cascade charts.
## https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/


## (2) Pie chart
## A piechart is a circle divided into sectors that 
## each represent a proportion of the whole. 
## https://www.r-graph-gallery.com/pie-plot.html

## However, it's often been criticized:
## https://www.data-to-viz.com/caveat/pie.html

## Code to create a pie chart: stacked bar graph--> pie
## http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization


## (3) Donut chart
## A donut or doughnut chart is a ring divided into sectors
## that each represent a proportion of the whole.
## https://www.r-graph-gallery.com/doughnut-plot.html

## Code to create a donut chart: rectangle--> ring
## https://www.r-graph-gallery.com/128-ring-or-donut-plot.html

