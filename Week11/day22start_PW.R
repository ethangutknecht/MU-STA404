# -----------------------------------#
#           Week 11--Day22           #
#       Multivariate Displays        #   
#------------------------------------#

#==============================================#
# Part 19: Multivariate Displays (Bubble plot, # 
# Scatterplot Matrix, Parallel Coordinate Plot)#
#==============================================#
## College data ##
# install.packages("ISLR")
library(ISLR)
library(tidyverse)
?College

# College data consists of 777 colleges.
head(College)
names(College)

### (19.1) Bubble Plot ###
# Extends a scatterplot into 3 dimensions by utilizing 
# the size of the point to determine a third variable
# See: https://en.wikipedia.org/wiki/Bubble_chart
#      https://www.data-to-viz.com/graph/bubble.html

## (1) Basic bubble plot
ggplot() + 
  geom_point(aes(x=Expend, y=Grad.Rate, size=Enroll),alpha=0.3, data=College)
  

# Question: anything wrong in the plot?
max(College$Grad.Rate)
which(College$Grad.Rate==118)
College[which(College$Grad.Rate==118),]



ggplot() + 
  geom_point(aes(x=Expend, y=Grad.Rate, size=Enroll, color=Private),alpha=0.3, data=College) +
  scale_y_continuous(limits = c(0,100))


## (2) Add a fourth variable's information
ggplot() + 
  


## (3) Let's highlight Miami on the plot
# Maybe change the color of Miami to have alpha=1, while other schools are transparent. 

head(College)
names(College)
rownames(College) # This is not a variable name.
# [366] "Miami University at Oxford"                   

College$Private[366] <- "Miami"
College$Private[366] <- "No"




# Change private to have value "Yes","No" and "Miami University"
College$Private 
#This is a two level factor, want to add another level "Miami"

# Which row corresponds to Miami University?
College %>% 
  mutate(School=rownames(College)) %>% 
  filter(str_detect(School,"Miami"))

College2 <- College %>% 
  mutate(School=rownames(College),
         Private=ifelse(School=="Miami University at Oxford",
                        "Miami",
                        as.character(Private)),
         Private=factor(Private,levels=c("No","Yes","Miami")))


class(College2$Private)
College2$Private

a <- factor (c("a", "b", "c", "b", "c", "b", "a", "c", "c"))
a
# What if I try to change an element to a new value, like "d"?
a[3] <- "d"
# However it's okay to set elements to values that are already levels
a[3] <- "a"
a
ggplot() + 
  geom_point(aes(x=Expend, y=Grad.Rate, 
                 size=Enroll, color=Private),
             alpha=0.4, data=College2)+
  scale_y_continuous(limits=c(0,100))+
  scale_color_manual(values=c("#0072B2", "#D55E00", "black"),
                     labels=c("Private", "Public", "Miami University"))+
  
  labs(y="Graduate Rate", x="Expenditures (thousands of $)") + 
  theme_bw() +
  theme(legend.position="bottom")



# scale_alpha_manual(values=c(0.4,0.4,0.9))


## (4) Extend bubble plot to more dimensions of data

# (a). We may plot composite scores from multiple variables (sums, rates, etc)
# F.Undergrad+P.Undergraduate (full time plus the part time undergraduates)






# (b). Facet to create small multiples
## Separate the estimated book costs intor three pieces
min(College$Books)
max(College$Books)
?cut
College3 <- College %>%
  mutate(total_undergrads = F.Undergrad+P.Undergrad,
         book_groups = cut(Books,breaks=c(0,500,700,2500),
                           labels=c("Under $500",
                                    "$500-$700",
                                    "$700-$2500")))

ggplot()+
  geom_point(aes(x=Outstate, y=Grad.Rate, 
                 size=total_undergrads,
                 color=Private),
             alpha=.4,
             data=College3)+
  scale_y_continuous(limits=c(0,100))+
  facet_grid(.~book_groups)



### (19.2) Scatterplot Matrix ###
# Scatterplot matrix explores all pairwise relationships quickly
# make many pairwise plots and organize into grid of scatterplots
# install.packages("GGally")
library(GGally)
?ggscatmat

# Can directly function on the whole dataset.
# ggscatmat(College) # It takes a long time to run this code.
# Warning message: In ggscatmat(College) : Factor variables are omitted in plot

ggscatmat(College, columns=2:10,alpha=0.3,color="Private")+
  theme_classic()
   #This is a ggplot object, can change the theme

### (19.3) Parallel Coordinate Plot ###

## (1) Definiton and basic plot
# See definition and examples in: https://datavizcatalogue.com/methods/parallel_coordinates.html
# Organize many numeric axes in parallel (instead of orthogonal)
# Connect observation values with line across all axes
?ggparcoord
ggparcoord(College, columns = 2:10)

ggparcoord(College, columns = 2:10, groupColumn = 1) # or groupColumn="Private"

## (2) Display Miami in a different color
College4 <- College %>% 
  mutate(School=rownames(College),
         Private2=ifelse(School=="Miami University at Oxford","Miami",as.character(Private)),
         Private2=factor(Private2,levels=c("No","Yes","Miami"))) %>% 
  arrange(Private2)

College4$Private2

ggparcoord(College4, columns = 2:10, groupColumn = "Private2", alpha=0.4)+
  scale_color_manual(values=c("pink", "pink4", "black"))
  
  
  
# Since R display the graphs in order, so we need to change the order
# of the observations to display the line related with Miami easier. 



#=================================#
# Part 20: Aggregation Based Plot # 
#=================================#

### (20.1) Airlines Data ###
getwd()
setwd("C:/Users/Gutkn/Documents/STA404/Week11")
load("Jan2020Flight.Rdata")
head(delay)
glimpse(delay)

# From the Federal Airlines Administration (FAA), flight records for every U.S.
# air flight for January 2020. The original CSV file is 95.4MB. 
# The `.RData` file has been trimmed and is in R format.
# The original data can be obtained here for those interested:
# https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
# https://www.transtats.bts.gov/DL_SelectFields.asp?gnoyr_VQ=FGJ

### (20.2) Unaggregated plots (not recommended to run) ###

## Scatterplot of Departure Delays vs Arrival Delays
## (Don't add alpha blending. It will probably break your computer.
##  Since there are too many points need to be displayed.)
# ggplot()+
#   geom_point(aes(x=DEP_DELAY,y=ARR_DELAY), data=delay)
# 
## Time vs Arrival Delays
# ggplot()+
#   geom_point(aes(x=FL_DATE,y=ARR_DELAY), data=delay)

### (20.3) Some Concepts about Aggregation ###
# Q1: What is aggregation? 
# A: Creating summary statistics within grouped subsets of data.
# Example: Average, medians, maximum, SD, count, proportions, etc. 
#------------------------------------------------

# Q2: Many of the plots we already know rely on aggregation: 
# A: Histograms, barplots, boxplots, choropleth maps, heatmaps. 
#------------------------------------------------

# Q3: What are those plots used for?
# A: Common goal of all of these plots to create a geometric 
#    objects with attributes based on summaries instead of 
#    raw data
#------------------------------------------------

# Histograms do binning on one dimension
ggplot()+
  geom_histogram(aes(x=dep_hour), data=delay,binwidth = 2)


### (20.4) Binned Scatterplots ### 
# (heatmap-style 2-dimensional histogram)
?geom_bin2d
ggplot()+
  geom_bin2d(aes(x=DEP_DELAY, y=ARR_DELAY), data=delay)+
  theme_bw()+
  scale_fill_continuous(low="gray90",
                        high="gray10",
                        trans="log10")+
  labs(x="Departure Delay (Scheduled - Actual)",
       y="Arrival Delay (Scheduled - Actual)",
       title="Relationship between Arrival and Departure Delays")

### (20.5) Other plots ###
## (1) barplot showing the number of flights per carrier 

ggplot()+
  geom_bar(aes(x=OP_UNIQUE_CARRIER), data=delay)


## (2) boxplots of departure time per state 
##     horizontal layout and ordered by 
##     decending median departure time
# Hint:
# (1) You will need to create a numeric version 
#     of the departure time, `dep_hour+dep_min/60`.
# (2) Look into using the `reorder` function in R, 
#     you need to pass in four variables to get it 
#     to work correctly, including `na.rm=TRUE`.

?reorder
names(delay)
ggplot()+
  geom_boxplot(aes(x=reorder(ORIGIN_STATE_NM, 
                             dep_hour+dep_min/60,
                             FUN=,na.rm=T),
                   y=dep_hour+dep_min/60),
               data=delay) +
  coord_flip() 


#====================#
# Part 21: Animation # 
#====================#
# We have separated this part in question 2 of homework 9 #



#=================#
# Part 22: Plotly # 
#=================#
# https://plotly.com/ggplot2/getting-started/
# https://plotly.com/r/
# install.packages("plotly")
library(plotly)
### (23.1) Load the data
# Load the gapminder data that contain country name, year, population, 
# life expectancy, income, child mortality, CO2 emission and region (asia,
# europe, africa, americas). 

# To prepare this dataset, you can go to this website:
# https://www.gapminder.org/data/geo/
# download the corresponding datasets and clean it similarly as what has 
# been done in part 21. 


# This is a similar dataset as the gapminder data in R directly. 
# I removed the observation corresponding to continent 
# value is NA, and added variables for children's mortality and CO2 emission.
# child mortality: death of children under five years of age per 1,000 live births.
# co2 emissions: co2 emission from the burning of fossil fuels (metric tonnes of co2 per person).

load("gapminderdata.RData")
head(gapminderdata)

all2019 <- gapminderdata %>%
  filter(Year == "2019") %>% 
  drop_na(four_regions)

min(all2019$Population)
max(all2019$Population)


# Look at the bubble plot:
p3 <- ggplot(all2019) +
  geom_point(aes(x=Income, y=LifeExp, size=Population, color=four_regions),
             alpha=0.5) +
  labs(y="Life Expentency", x="Income (log 10 scale)") +
  scale_y_continuous() + 
  scale_x_log10() +
  scale_color_discrete() +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  scale_size_continuous()
#scale_size_continuous guide=FALSE will remove the legend for size


p3
class(p3)
?ggplotly
ggplotly(p3)
class(ggplotly(p3))
## What are the differences between the regular ggplot object 
## and the plotly object? Anything that needs to be improved?
unique(all2019$four_regions)
class(all2019$four_regions)

all2019 <- all2019 %>% 
  mutate(four_regions=factor(four_regions,labels=c("Africa","Americas","Asia","Europe")))
head(all2019)  
all2019$four_regions

p4 <- 
  
  ggplotly(p4)

## Want to display more information in the box?
## Want to control what information to be displayed.
?ggplotly
ggplotly(p4,tooltip = c("label","y","size","text"))
# https://plotly.com/r/plotly-fundamentals/

# Another tool that can create a plotly object is through plot_ly
# Some comparision betwen ggplotly and plot_ly: https://jtr13.github.io/spring19/community_contribution_group17.html

