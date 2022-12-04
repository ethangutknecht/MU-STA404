# ------------------------------------#
#           Week 9--Day18            #
#   Rescaling, annotations,themes    #
#               Maps                  #
#-------------------------------------#
#===========================================#
# Part 13: Rescalling, annotations, themes  #
#          French Fries Dataset             #
#===========================================#

### (13.1) Data description ###
## The data is from the reshape2 package
library(tidyverse)
library(reshape2)
?french_fries
head(french_fries)
test <- french_fries %>% arrange(subject)
test
# 10 time points, Different subjects may participate in all or some of the studies.
# Each subject takes two batches of test.

ff_subject <- french_fries %>% 
  group_by(subject, time) %>% 
  summarize(potatoavg = mean(potato), na.rm = TRUE)
ff_subject




### (13.2) Preliminary plots ###
## (1) Lineplot of average potato flavor over time, one line per person

# Data: Each person at each time point will have an observation
# (average potato score across the 3 treatments and 2 replicates(batches))

  
# The na.rm=TRUE is added since the average score for
# subject 15 at time 5 is NA. We need to take a look at
# the original data and decide whether that makes sense
# to remove those missing values. 
ff_subj <- french_fries %>% 
  group_by(subject, time) %>% 
  summarize(potatoavg = mean(potato, na.rm=TRUE))


french_fries %>% 
  filter(subject==15,time==5)

unique(ff_subj$subject)
length(unique(ff_subj$subject)) 

ggplot(data = ff_subj) + 
  geom_line(aes(x = time, y = potatoavg))
  












## (2) Lineplot of overall average potato flavor over time

# Data: Each time point has an average score
ff_time <- french_fries %>% 
  group_by(time) %>% 
  summarize(overall = mean(potato, na.rm=TRUE))
 
ff_time

ggplot() +
  geom_line(aes(x = time, y = overall, group= 1), data=ff_time)


## (3) Put them together
ggplot() +
  geom_line(aes(x = time, y = overall, group= 1), data=ff_time, color="red") + 
  geom_line(aes(x = time, y = potatoavg, group=subject), data=ff_subj, color="tan")



## (4) Add some features to it
ggplot() +
  geom_line(aes(x=time, y=potatoavg, group=subject),color="tan",data=ff_subj)+
  geom_line(aes(x=time, y=overall, group=1),color="red", data=ff_time)+ 
  annotate("text", x = 5.5, y = 9,
           label = "Dark red line is the overall potateo average",
           color = "red", angle=-10, size = 12) +
  theme_bw() +
  labs(x = "Potateo-y Flavor Score", y = "Weeks", title="Potato-y Flavor Score Over Time") +
  scale_y_continuous(breaks=c(0,4,8,12,16), limits = c(0,16)) +
  geom_text(aes(x = 5.5, y = 7), label = "Geom Text label", color = "red", angle=10, size=8)
  
  
  # annotate(): create an annotation layer
  # https://ggplot2.tidyverse.org/reference/annotate.html
  ?annotate()


### (13.3) Display Extra Information ###

## (1) Add the oil type and flavor information to the graph, facet?

## a. Convert to tall format where flavor and score 
# variables replace 5 flavor columns,
## b. then find average for each subject for each flavor 
# category and each treatment, at each week.

french_fries

ff_subj_tall <- french_fries %>% 
  pivot_longer(cols = 5:9, 
               names_to = "flavor", values_to = "score") %>% 
  group_by(subject, time, flavor, treatment) %>% 
  summarize(avgScore = mean(score))
  


cx## (2) Add a "smoother" for each flavor and oil treatment combination
# e.g.LOESS: locally estimated scatterplot smoothing. 
# It fit local polynomial regression.
# https://www.statisticshowto.com/lowess-smoothing/#:~:text=LOWESS%20(Locally%20Weighted%20Scatterplot%20Smoothing,between%20variables%20and%20foresee%20trends.
ggplot(aes(x=time,y=avgScore),data=ff_subj_tall)+
 


?geom_smooth
# span=: Controls the amount of smoothing for the default loess smoother. 
#        Smaller numbers produce wigglier lines, larger numbers produce smoother lines.
# se: Display confidence interval around smooth? (TRUE by default, see level to control.)
# Refer to https://ggplot2.tidyverse.org/reference/geom_smooth.html


# (3) Edit the theme, polish up the graphs.
# RGB: red green blue
# The red, green and blue use 8 bits each, which have integer values from 0 to 255. 
# This makes 256*256*256=16777216 possible colors. https://www.rapidtables.com/web/color/RGB_Color.html

# hexadecimal colors (HEX) https://www.developintelligence.com/blog/2017/02/rgb-to-hex-understanding-the-major-web-color-codes/
# Color wheels: https://www.canva.com/colors/color-wheel/
# Recall the website from Day 3: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/

# Define our own theme

mytheme <- theme(panel.background = element_rect(fill="white"),
                 panel.grid.major = element_line(color="gray85"),
                 panel.grid.minor = element_line(color="gray95"),
                 plot.title = element_text(color="darkblue"),
                 axis.title.y = element_text(color= "darkblue"))


# Change the oil variable from 1,2,3 to "Oil 1", "Oil 2", "Oil 3
ff_subj_tall$oil <- paste("Oil", ff_subj_tall$treatment)
glimpse(ff_subj_tall)


ggplot(aes(x=time, y=avgScore), data=ff_subj_tall)+
  geom_line(aes(group=subject)) +
  facet_grid(flavor~treatment)+
  geom_smooth(aes(group=1, color=flavor), method="loess", span=2,se=FALSE) +
  scale_y_continuous(breaks = seq(0,16,4), limits = c(0,16))  +
  theme_bw() +
  labs(x = "Potateo-y Flavor Score", y = "Weeks",
       title="Potato-y Flavor Score Over Time") +
  mytheme
  

ggplot(aes(x=time, y=avgScore), data=ff_subj_tall) +


?scale_color_manual
?guides()
# guides(): set or remove the legend for a specific aesthetic
# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software#guides-set-or-remove-the-legend-for-a-specific-aesthetic
# https://ggplot2.tidyverse.org/reference/guides.html

?ggsave
ggsave("FrenchFries.png",dpi=600,height=8,width=5, units="in")
# ggsave() is a convenient function for saving a plot. It defaults to saving the last plot that you displayed, using the size of the current graphics device. 
# It also guesses the type of graphics device from the extension.
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##

#===========================================#
#      Module 3:  Advanced Static Plots     # 
#===========================================#

### (14.1) Map Introduction ###
#install.packages('maps')
library(maps)

?map_data()
state_map_data <- map_data("state")
head(state_map_data)
tail(state_map_data)
# 
# long:  longitude. Things to the west of the prime meridian are negative, down to -180, 
#        and things to the east of the prime meridian run from 0 to positive 180.
# lat:   latitude.
# order: This just shows in which order ggplot should "connect the dots"
#        region and subregion tell what region or subregion a set of points surrounds.
# group: ggplot2's functions can take a group argument which controls
#        (amongst other things) whether adjacent points should be connected by lines. 
#        If they are in the same group, then they get connected, but if they are in different 
#        groups then they don't. Essentially, having two points in different groups means that 
#        ggplot "lifts the pen" when going between them.
# https://eriqande.github.io/rep-res-eeb-2017/map-making-in-R.html


### (14.2) Plot a map

## (1) Make an outline of the states in USA

# Try to use geom_line to connect the longitude and latitude information.
ggplot() + 
  

# What's wrong with the graph?
# geom_line() connects observations in order of the variable on the x axis
# geom_path() connects them in the order in which they appear in the data
ggplot() + 
  


## (2) Work with other maps
## ohio data by county

ohio_map_data <- map_data()
head(ohio_map_data)
tail(ohio_map_data)
ggplot() + 
  

## world map
world_map_data <- map_data("")
head(world_map_data)
tail(world_map_data)


## (3) Projections
# coord_map projects a portion of the earth, which is approximately spherical, 
# onto a flat 2D plane using any projection defined by the mapproj package. 
# https://ggplot2.tidyverse.org/reference/coord_map.html
nz_map_data <- map_data("nz")
head(nz_map_data)

#install.packages("mapproj")
library(mapproj)
?coord_quickmap 
# coord_quickmap() is a quick approximation that does preserve straight lines. 
# It works best for smaller areas closer to the equator.

ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=nz_map_data)+
  
  # With the aspect ratio approximation


?coord_map 
# coord_map() projects a portion of the earth, which is approximately spherical, 
# onto a flat 2D plane using any projection defined by the mapproj package.  
ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=nz_map_data)+
  
  # With correct mercator projection
  # https://en.wikipedia.org/wiki/Mercator_projection#:~:text=The%20Mercator%20projection%20(%2Fm%C9%99r,cartographer%20Gerardus%20Mercator%20in%201569.
  
  
  ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=world_map_data) + 
  
  # orthographic projection
  # default is c(90, 0, mean(range(x)))
  # orientation=c(latitude,longitude,rotation)
  
  
#============================#
# Part 15:  Choropleth Maps  #
#   Display data on the map  #
#============================#
  
# Choropleth maps are thematic maps in which different areas are colored 
# or shaded in accordance with the value of a statistical variable being 
# represented in the map.
# Definition in https://en.wikipedia.org/wiki/Choropleth_map

### (15.1) Make a Choropleth Map
# Use the world map data
#install.packages("gapminder")
library(gapminder)
?gapminder
head(gapminder)
# gapminder data is also available in 
# https://www.gapminder.org/data/

country2007 <- gapminder %>%
  filter(year==) 
head(world_map_data)
head(country2007)

str(world_map_data)
str(country2007)
# Let's merge our map data frame with the gapminder data. 
# Join two tbles together https://dplyr.tidyverse.org/reference/join.html
# left_join: return all rows from x, and all columns from x and y. 
#            Rows in x with no match in y will have NA values in the new columns. 
#            If there are multiple matches between x and y, 
#            all combinations of the matches are returned.
world_map_all <- 
  head(world_map_all)


# geom_polygon: Polygons are very similar to paths (as drawn by geom_path()) 
#                except that the start and end points are connected and the 
#                inside is coloured by fill.
?geom_polygon
gdpmap <- 
  
  gdpmap


library(ggthemes)
gdpmap+
  
  
  
  
### (15.2) Check and fix issues with the graph ### 
  
## (1) Fix the missing value for many countries
  
unique(world_map_data$region) # USA, Russia,UK
unique(country2007$country) # United States, United Kingdom, no Russia

## Thus, we need to change the value of those certain countries. 
# revalue is a function in package "plyr"
?revalue  
# If x is a character/factor, the named levels of the factor will 
# be replaced with the new values. 
# syntax: c("old value"="new value")
## cc. check the help file example.
x <- c("a", "b", "c")
plyr::revalue(x, c(a = "A", c = "C"))

country2007 <- gapminder %>%
  filter(year==2007) %>%
  mutate(country,plyr::revalue(country,c("United States"="USA",
                                         "United Kingdom"="UK"))) 

country2007$country
# This is tedious. Also, we cannot find all the countries that 
# do not match with each other. 
# We want more efficient/standard way to do this.

## (2) Match country names using an alternative method

# Question: is there a standard country name in R?
# Documentation for gapminder reveals that there is a country_codes data frame with ISO-3
# https://cran.r-project.org/web/packages/gapminder/gapminder.pdf


?country_codes
head(country_codes)
head(gapminder)

country2007_ISO3 <- gapminder %>%
  filter(year==2007) %>% 
  
  
  head(country2007_ISO3)


# install.packages("countrycode") 
# It converts country names and country codes
library(countrycode)
?countrycode()
?codelist
head(world_map_data)
world_map_data$iso3 <- countrycode(,
                                   origin = '',
                                   destination='')
# Look at the errors:
# country code: https://unstats.un.org/unsd/tradekb/knowledgebase/country-code
# Virgin Islands: https://en.wikipedia.org/wiki/Virgin_Islands

# Join the two datasets by iso country code again
world_map_all_iso3 <- left_join(, , 
                                by=c(""=""))
head(world_map_all_iso3)

## The updated map:
ggplot() +
  geom_polygon(aes(x=long,y=lat,group=group, fill=gdpPercap),
               data=world_map_all_iso3)+
  theme_map() +
  coord_quickmap()
gdpmap

## (3) Compare with the previous map, any finding?
## Note: We seem to have an issue with Sudan 

# str_detect: detect a substring
# Wild card symbol'.'(dot): represent any letter including special characters. 
# '*'(asterisk): match any preceeding characters zero or more times
# You can take a look at this website for more details. 
# https://blog.exploratory.io/filter-with-text-data-952df792c2ba
?str_detect

world_map_data %>% 
  filter(str_detect(region, "Sud.n")) %>% 
  select(region) %>% 
  unique()

  
  
# ISO codes may have updated after Sudan split 
# into Sudan and south Sudan after 2010 civil war 
# map_data("world") provides 2013 border outlines, 
# gapminder data is from 2007
  
# It is likely that our original merge was successful 
# by name but was not shading in 
# the territory in what is now south sudan 
# Shifting borders and changing nations adds challenging 
# complexity to map drawing



## (4) Change the appearance of the graph
ggplot() +
  geom_polygon(aes(x=long,y=lat,group=group, fill=gdpPercap),
               data=world_map_all_iso3)+
  theme_map() +
  coord_quickmap()+
  labs(title="2007 GDP per capita (US$, inflation-adjusted)", 
       subtitle="Data source: Gapminder.org (2007)")+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))




