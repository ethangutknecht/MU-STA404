# Ethan Gutknecht
# 10/14/2022
# Solo Project - STA404
# ----------------------------- #

# Load and display data from CSV filelibrary(tidyverse)
library(tidyverse)
library(lubridate)
setwd("C:/Users/Gutkn/Documents/STA404/Project01")
covidData <- read.csv("COVIDDeathData_CountyOfResidence.csv")


# ---------- Graph 1 ---------- #
# Format Data
covidDataG1 <- covidData %>% 
  mutate(date = ymd(Onset.Date)) %>% 
  mutate(month = as.character(month(date)), 
         year = as.character(year(date))) %>% 
  group_by(year, month) %>% 
  summarize(numberOfCases=n())



# Add order to months
covidDataG1$month <- ordered(covidDataG1$month, levels=c("1", "2", "3", "4", 
                                                         "5", "6", "7", "8", 
                                                         "9", "10", "11", "12"))

# Cases By Age Range 
ggplot(data=covidDataG1) +
  geom_histogram(aes(x=month, y=numberOfCases, fill=year),
                 stat="identity", position = "dodge") + 
  labs(x="Date", y="COVID-19 Case Count") + 
  ggtitle("COVID-19 Cases Over Time") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


# ---------- Graph 2 ---------- #
# Format Data
covidDataG2 <- covidData %>% 
  mutate(date = ymd(Onset.Date)) %>% 
  filter(County=="Franklin" | County=="Cuyahoga" | County=="Hamilton" |
           County=="Summit" | County=="Montgomery")



# Cases By Date Over Time
ggplot(data=covidDataG2) +
  geom_density(aes(x=date, color=County), alpha=0.5, adjust=1, size=2) + 
  labs(x="Date", y="COVID-19 Case Count") + 
  ggtitle("COVID-19 Cases Over Time") +
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5))


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


# ---------- Graph 3 ---------- #
# Format Data
covidDataG3 <- covidData %>% 
  mutate(date = ymd(Onset.Date)) %>% 
  filter(County=="Franklin" | County=="Cuyahoga" | County=="Hamilton" |
         County=="Summit" | County=="Montgomery" | County=="Lucas" |
         County=="Butler" | County=="Stark" | County=="Lorain" |
           County=="Warren") %>% 
  mutate(updatedCounty = as.factor(County))

# Add an order to the counties
covidDataG3$updatedCounty <- ordered(covidDataG3$updatedCounty, 
                                     levels=c("Franklin", "Cuyahoga", "Hamilton",
                                      "Summit",  "Montgomery", "Lucas", 
                                      "Butler", "Stark", "Lorain", "Warren"))


# Create Graph
ggplot(data=covidDataG3)+
  geom_bar(aes(x=updatedCounty),fill="#DC9A9C", stat="count") +
  labs(x="Most Populated Ohio County", y="COVID-19 Case Count") + 
  ggtitle("COVID-19 Cases By Ohio County") + 
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5))




