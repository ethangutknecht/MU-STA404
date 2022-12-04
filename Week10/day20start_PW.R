# ------------------------------------#
#           Week 10--Day20            #           
#               Maps                  #
#-------------------------------------#

#===========================================#
#      Module 3:  Advanced Static Plots     # 
#===========================================#

### (14.1) Map Introduction ###
#install.packages('maps')
library(maps)
library(tidyverse)

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
  geom_line(aes(x=long,y=lat,group=group),data=state_map_data)

# What's wrong with the graph?
# geom_line() connects observations in order of the variable on the x axis
# geom_path() connects them in the order in which they appear in the data
ggplot() + 
  geom_path(aes(x=long,y=lat,group=group),data=state_map_data)


## (2) Work with other maps
## ohio data by county

ohio_map_data <- map_data("county", region = "ohio")
head(ohio_map_data)
tail(ohio_map_data)
ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=ohio_map_data)

## world map
world_map_data <- map_data("world")
head(world_map_data)
tail(world_map_data)
ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=world_map_data)

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
  coord_quickmap()
  
  # With the aspect ratio approximation


?coord_map 
# coord_map() projects a portion of the earth, which is approximately spherical, 
# onto a flat 2D plane using any projection defined by the mapproj package.  
ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=nz_map_data)+
  coord_map()
  
  # With correct mercator projection
  # https://en.wikipedia.org/wiki/Mercator_projection#:~:text=The%20Mercator%20projection%20(%2Fm%C9%99r,cartographer%20Gerardus%20Mercator%20in%201569.
  
  
  ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=world_map_data) + 
    coord_map(projection = "ortho", orientation = c(0,300,0))
  
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
  filter(year==2007) 
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
world_map_all <- left_join(world_map_data, country2007,by=c("region" = "country"))
  head(world_map_all)


# geom_polygon: Polygons are very similar to paths (as drawn by geom_path()) 
#                except that the start and end points are connected and the 
#                inside is coloured by fill.
?geom_polygon
gdpmap <- ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=gdpPercap), data=world_map_all)
  
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
  left_join(country_codes, by="country")
  
  
  head(country2007_ISO3)


# install.packages("countrycode") 
# It converts country names and country codes
library(countrycode)
?countrycode()
?codelist
head(world_map_data)
world_map_data$iso3 <- countrycode(world_map_data$region,
                                   origin = 'country.name',
                                   destination='iso3n')
# Look at the errors:
# country code: https://unstats.un.org/unsd/tradekb/knowledgebase/country-code
# Virgin Islands: https://en.wikipedia.org/wiki/Virgin_Islands

# Join the two datasets by iso country code again
world_map_all_iso3 <- left_join(world_map_data, country2007_ISO3, 
                                by=c("iso3"="iso3num"))
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


# Make a state level map based on county data
# 

cal_map_data <- map_data("county", region = "california")
ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=cal_map_data)



#====================================#
# Part 16: Nesting data, other stuff #
#====================================#
### (16.1) Nesting data with tidyr ###
# What if we store the data a little differently, 
# put all outline in a single row next to the state name?
# -problem: we have never put a complex object into a data
#           cell before, but tibble can handle it
?nest
world_data <- world_map_data %>%
  group_by(region) %>%
  nest()


test <- world_data %>% 
  filter(region=="Afghanistan")
head(test)

which(world_data$region=="Turkey")
world_data$data[[]]
?which

world_data$data[[2]]$long 


world_data$iso3new <- countrycode(world_data$region,
                                  origin = 'country.name',
                                  destination='iso3n')

world_all <- left_join(world_data,country2007_ISO3, 
                       by=c("iso3new"="iso_num"))
head(world_all)
# The gain here is that the storage size is much 
# smaller without the redundancy of having the 
# gapminder values repeated for each boundary outline. 

?unnest
head(unnest(world_all))
unnest(world_all,cols="data")

lifeExpmap <- ggplot() + 
  geom_polygon(aes(x=long, y=lat,
                   fill=lifeExp, group=group),
               data=unnest(world_all,cols=c(data)))+
  coord_map() # What's wrong with it? 
# issue with the coord_map(), see https://stackoverflow.com/questions/30360830/map-in-ggplot2-visualization-displaying-bug/30463740#30463740

lifeExpmap

### (16.2) Adding layer of labels to states ###
# bar-graph- add text
# slope graph (ohio, french fries)
?sort
sort(unique(world_all$lifeExp))

# Calculate the longtitude and latitude about where to put
# labels

world_lab <- world_all %>%
  filter(lifeExp>81.702) %>% 
  unnest(data) %>%  #unnest("data"), or unnest(c(data))
  summarise()

?round()
?range()
world_lab$long
world_lab

lifeExpmap+
  geom_text(aes(x=,y=,label=),
            data=world_lab)

