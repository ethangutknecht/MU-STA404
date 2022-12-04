# -----------------------------------------------#
#                 Week 08--Day16                 #
#   Design principals (slopegraph)               #
#             Gestalt principles                 #
#------------------------------------------------#

#============================================#
# Part 9: Tables, Heatmap and Slopegraph     #
#============================================#

### (9.3) Slopegraph  ###

## (1) Introduction
## What is a slopegraph?
## https://www.betterevaluation.org/en/evaluation-options/slopegraph#:~:text=A%20slopegraph%20is%20a%20lot,at%20interpreting%20changes%20in%20direction.

## (2) Data preparation
## Drug Poisoning Mortality in the United States, 
## 2003-2010
## https://www.cdc.gov/nchs/data-visualization/drug-poisoning-mortality/
## Download the following dataset directly from webstie
## "NCHS_-_Drug_Poisoning_Mortality_by_County__United_States"
library(tidyverse)

setwd("C:/Users/Gutkn/Documents/STA404/Week08")
drug_poison <- read_csv("NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv")
head(drug_poison) ##take a look at the data
names(drug_poison)
data0320 <- drug_poison %>% 
  filter(Year==2003|Year==2020) %>% 
  group_by(Year, State) %>% 
  summarize(rate=mean(Model-based Death Rate))

# Add back quate
head(drug_poison) ##take a look at the data
names(drug_poison)
data0320 <- drug_poison %>% 
  filter(Year==2003|Year==2020) %>% 
  group_by(Year, State) %>% 
  summarize(rate=mean(`Model-based Death Rate`)) # can use get("") function

head(data0320)


## (3) Create a slopegraph
ggplot() + 
  geom_line(aes(x=Year, y=rate, group=State),
            data=data0320)

#years are mis leading

?geom_line


data0320 <- drug_poison %>% 
  filter(Year==2003|Year==2020) %>% 
  mutate(Yearo=ordered(Year)) %>% 
  group_by(Yearo, State) %>% 
  summarize(rate=mean(`Model-based Death Rate`)) # can use get("") function



ggplot() + 
  geom_line(aes(x=Yearo, y=rate, group=State), data=data0320) +
  labs(y="Average Model Based Death Rate",
       title="Drug Poisoning Mortality")










## (3) Clean up the plot and add appropriate information
colnames(data0320)=c("Year","State","Rate")

ggplot() + 
  geom_line(aes(x=Year, y=Rate, group=State), data=data0320) +
  labs(y="Average Model Based Death Rate",
       title="Drug Poisoning Mortality") + 
  theme(plot.title = (element_text(hjust=0.5)))+
  coord_cartesian(ylim = c(5,15))
  
  
  ?coord_cartesian

## (4) Highlight Ohio?

data0320ind <- data0320 %>% 
  mutate(IsOhio = ifelse(State=="Ohio", "Ohio", ""))


ggplot() + 
  geom_line(aes(x=Year, y=Rate, group=State), data=data0320ind) +
  labs(y="Average Model Based Death Rate",
       title="Drug Poisoning Mortality") + 
  theme(plot.title = (element_text(hjust=0.5)))+
  coord_cartesian(xlim = c(1.5,1.5)) +
  geom_text(aes(x=Year, y=Rate, label=IsOhio), data=filter(data0320ind, Year==2020),
            hjust=-0.15, color="red", size=6)+
  scale_color_manual(values =  c("grey", "red")) +
  theme(legend.position = "none")



### (9.4) Other graphs  ###

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


#=================================#
# Part 10: Gestalt principles     #
#=================================#
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
  geom_rect(aes(xmin=4.5, xmax=5.6, ymin=3.25, ymax=4.0, color="black", alpha=0))
  
  ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width),data=iris)+
  geom_rect(aes(xmin=4.5, xmax=5.6, ymin=3.25, ymax=4.0, alpha=0.2))
  
  
  
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
  mutate(year=year(date),
         decade=factor(round(year/10) * 10)) 
new
ggplot(aes(x=date,y=unemploy,color=decade),data=new)+
  geom_point()






## Example of Decluttering
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,color=Species),data=iris)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color="black")
  )
  ## https://felixfan.github.io/ggplot2-remove-grid-background-margin/
  
  
  # in class: slope graph to diamonds dataset
  

head(diamonds)
  
diamondsData <- diamonds %>% 
  filter(cut=="Fair"|cut=="Ideal") %>% 
  group_by(cut, color)  %>% 
  summarize(avePrice=mean(price))

head(diamondsData)

ggplot() + 
  geom_line(aes(x=cut, y=avePrice, group=color), data=diamondsData) +
  labs(y="Average Price Of Diamond",
       title="Average Price of Diamond By Cut and Color")
  
  
  






