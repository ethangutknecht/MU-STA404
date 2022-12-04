library(tidyverse)
library(maps)
library(readxl)
library(ggthemes)
library(mapproj)
library(stringr)
setwd("C:/Users/Gutkn/Documents/STA404/Week13")
# Comment: set the working directory to the path you store the datasets.

#========================================================================#
# ---- The following is a stating code for Question 1 ------ #
#============================================================#
# Download the data (which is the excel data with name Data Download” from the website with a last uploaded date 9/10/2020)
# https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/
# Or you can download the data directly from canvas. 

## local foods data from "LOCAL" tab in excel sheets
local_food <- read_excel(path="FoodEnvironmentAtlas.xls", sheet="LOCAL") %>% 
  select(FIPS, State, County, FRESHVEG_FARMS12, ORCHARD_ACRES12, 
         GHVEG_FARMS12, GHVEG_SQFTPTH12,DIRSALES_FARMS12)
# Note: postal abbreviations on state names

## FIPS codes for State and county (unabbreviated) from "Supplemental Data - County" tab
county_fips <- read_excel(path="FoodEnvironmentAtlas.xls", sheet="Supplemental Data - County") %>% 
  select(FIPS, State, County)

## County boundary data
county_outlines <- map_data("county") 



# ---------------Start your code for Question 1 below ------------------- #
#=========================================================================#

# Before combining the datasets, you can think about how to combine
# the available datasets. Here are a few things to consider:
# 1. The spelling of the state, and county name in "county_outlines" 
# and the "local_food" are different, we need to find a dataset in 
# the middle (for example, "county_fips") to help joining the datasets.  

# 2. You may change the spelling of the state and county to lower in county fips data, 
# so that it can be combined with the county_outlines data

# Question 1:
# Before combining the datasets we must fix the counties of each data set
# so they both match up. We can do this changing the local_foods dataset
# to lowercase and adding the word " county" to the end of the county_outlines
# dataset.

county_outlines$subregion <- paste(county_outlines$subregion, "county")

county_fips$County <- tolower(county_fips$County)
county_fips$State <- tolower(county_fips$State)

# Change State Name
local_food$StateName <- state.name[match(local_food$State, state.abb)]
local_food$StateName <- tolower(local_food$StateName)
local_food$County <- tolower(local_food$County)
local_food$County <- paste(local_food$County, "county")

head(county_outlines)
head(county_fips)
head(local_food)

# Join data set
all_data <- left_join(county_outlines,county_fips, 
                       by=c("region"="State", "subregion"="County"))
all_data <- left_join(all_data,local_food, 
                      by=c("region"="StateName", "subregion"="County"))

# Create plot
ggplot(data=all_data)+ 
  geom_polygon(aes(x=long, y=lat, fill=DIRSALES_FARMS12, group=group))+
  coord_map()
ggsave("Q1Plot.png")
