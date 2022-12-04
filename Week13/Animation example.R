###############################################################################
###############################################################################
###############################################################################
# The following Part 21 is related with animation. We will not discuss this
# during class and you may following the code below to test run it and see an 
# example of animation. 

#====================#
# Part 21: Animation # 
#====================#
library(tidyverse)
# An animated chart displays several chart states one after the other.
# You can get a sense of different animated graphs here: 
# https://www.r-graph-gallery.com/animation.html
# First, please watch this video:  
# Hans Rosling: 200 years in 4 minutes - BBC News
# https://www.youtube.com/watch?v=Z8t4k0Q8e8Y&feature=youtu.be
# We will create a simialr animation as shown in the video. 

### (21.1) Get the data ###
# We can get the data from https://www.gapminder.org/data/, 
# which we have used it in previous class. Or download from the canvas
# website. 
setwd("C:/Users/Gutkn/Documents/STA404/Week13")
# Income
income <- read_csv("income_per_person_gdppercapita_ppp_inflation_adjusted.csv")

# Life expectancy
life <- read_csv("life_expectancy_years.csv")

# Population
pop <- read_csv("population_total.csv")

# Demographic location
library(readxl)
geographics <- read_excel("Data Geographies - v1 - by Gapminder.xlsx",
                          sheet=2)
# downloaded from https://www.gapminder.org/data/geo/

head(income)
head(life)
head(pop)
head(geographics)


### (21.2) Prepare the data for plot ###
## (1) Change data from "wide" to "long"
income.tall <- income %>%
  pivot_longer(names_to="Year", values_to="Income",-country)
head(income.tall)  
income.tall %>% arrange(country)

life.tall <- life %>%
  pivot_longer(names_to="Year", values_to="LifeExp",-country)
head(life.tall)

pop.tall <- pop %>%
  pivot_longer(names_to="Year", values_to="Population",-country)
head(pop.tall)

# Combine them all
all <- left_join(pop.tall,life.tall,by=c("country","Year")) %>% 
  left_join(income.tall,by=c("country","Year")) 
head(all)

## (2) Subset the geographics data
head(geographics)
region <- geographics %>% 
  select(name,four_regions)
head(all)
head(region)
all <- all %>%
  left_join(region,by=c("country"="name"))
head(all)
tail(all)



### (21.3) Make a bubble plot for a specific year ###

# Get the range of the x axis and y axis
min(all$Income)
min(all$Income,na.rm=TRUE) #245
max(all$Income,na.rm=TRUE) #179000
min(all$LifeExp, na.rm=TRUE) # 1.01
max(all$LifeExp, na.rm=TRUE) #94.8
min(all$Population, na.rm=TRUE) #645
max(all$Population, na.rm=TRUE) # 1650000000


all2019 <- all %>%
  filter(Year == "2019")
head(all2019)
min(all2019$Population, na.rm=TRUE) #815

?scale_x_log10
?scale_size_continuous
p1 <- ggplot(all2019) +
  geom_point(aes(x=Income, y=LifeExp, size=Population, color=four_regions),
             alpha=0.5) +
  scale_y_continuous(limits=c(0,100)) + 
  scale_x_log10(limits=c(200,200000)) +
  labs(y="Life Expentency", x="Income (log 10 scale)") +
  scale_color_discrete(name="Continent",
                       labels=c("Africa", "Americas", "Asia", "Europe") ) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  scale_size_continuous(range=c(1,20),guide=FALSE,
                        limits=c(500,1650000000))
#scale_size_continuous guide=FALSE will remove the legend for size
p1

### (21.4) Make animated plot ###
#install.packages("gganimate")
library(gganimate)
# https://gganimate.com/articles/gganimate.html
# https://cran.r-project.org/web/packages/gganimate/gganimate.pdf

# If there's a message mentioning: No renderer backend detected. 
# gganimate will default to writing frames to separate files
# Consider installing:
#   - the `gifski` package for gif output
#   - the `av` package for video output
# and restarting the R session
#install.packages("gifski")

#install.packages("av")
#library(gifski)
#library(av)
all$Year
class(all$Year)
# Use the data "all" directly, and see whether it will give you the result.
alln <- all %>%
  mutate(Year = as.numeric(Year)) %>% 
  filter(Year<=2019) 

min(alln$Income,na.rm=TRUE) #245
max(alln$Income,na.rm=TRUE) #179000
min(alln$LifeExp, na.rm=TRUE) # 1.01
max(alln$LifeExp, na.rm=TRUE) #94.8 -->85.1
min(alln$Population, na.rm=TRUE) #645
max(alln$Population, na.rm=TRUE) # 1650000000 -->1.43e+09

ggplot(alln) +
  geom_point(aes(x=Income, y=LifeExp, size=Population, color=four_regions),
             alpha=0.5) +
  scale_y_continuous(limits=c(0,90)) + 
  scale_x_log10(limits=c(200,200000)) +
  labs(y="Life Expentency", x="Income (log 10 scale)") +
  scale_color_discrete(name="Continent",
                       labels=c("Africa", "Americas", "Asia", "Europe") ) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  scale_size_continuous(range=c(1,20),guide=FALSE,
                        limits=c(500,1440000000))+
  transition_time(Year)
?transition_states


### (21.5)  Add title and change some specific setting###
my.animation <- ggplot(alln) +
  geom_point(aes(x=Income, y=LifeExp, size=Population, color=four_regions),
             alpha=0.5) +
  scale_y_continuous(limits=c(0,90)) + 
  scale_x_log10(limits=c(200,200000)) +
  labs(y="Life Expentency", x="Income (log 10 scale)") +
  scale_color_discrete(name="Continent",
                       labels=c("Africa", "Americas", "Asia", "Europe") ) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title = element_text(size = 4),
        legend.text = element_text(size = 4)) + 
  scale_size_continuous(range=c(1,10),guide=FALSE,
                        limits=c(500,1440000000))+
  transition_time(Year)+
  ggtitle('Year:{as.integer(frame_time)}')+
  ease_aes('cubic-in-out') ## Slow start and end for a smoother look, ease_aes('linear')

# ease_aes: it needs to decide how the change from one value to another should progress. 
# This is a concept called easing. The default easing is linear, but others can be used,
?ggtitle()
?ease_aes 

anim_save("gifAnimation.gif", my.animation)

my.animation
?animate
animate(my.animation,
        nframe=length(unique(alln$Year)),
        fps=5,
        width=700,
        height=600,
        res=200)
# The animate() function documentation describes how to specify how the animation is rendered
# Label variables: frame_time. Gives the time that the current frame corresponds to.
# fps: Each image represents a frame, so if a video is captured and played back at 24fps, 
#      that means each second of video shows 24 distinct still images.
# res: set the resolution for each frame.

### (21.6) Check if there's anything wrong with the data ###
unique(alln$four_regions)

test <- alln %>% 
  filter(is.na(four_regions)*1==1)
test

###############################################################################
###############################################################################
###############################################################################

