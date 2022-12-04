# -----------------------------------------------#
#                 Week 06--Day11                 #
#         Data Cleaning and Manipulationan3,     #
#                 Tidyr, Lineplots               #
#            Long and wide format data           #
#------------------------------------------------#


#===========================================#
# Part 6: Data Cleaning and Manipulationan  #
#===========================================#
### (6.5) Ohio BRFSS data ###

# The Behavioral Risk Factor Surveillance System (BRFSS) is the nation’s premier
# system of health-related telephone surveys that collect state data about U.S. 
# residents regarding their health-related risk behaviors, chronic health conditions,
# and use of preventive services.

## (1) Preparing the data

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The following is how I downloaded and prepared the data from the source website.
# You can try this by yourself after class.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--------------------------------------------------------------------------------------------#
# Download the 2020 Behavioral Risk Factor Survalence System (BRFSS) as 64.3M SAS Export File
# from CDC Website: https://www.cdc.gov/brfss/annual_data/annual_2019.html
# Go to "Data Files": 2020 BRFSS Data (SAS Transport Format) [ZIP 64.3 MB]
# What is a .xpt file: https://www.loc.gov/preservation/digital/formats/fdd/fdd000464.shtml


# install.packages("SASxport")
library(SASxport)
setwd("C:/Users/Gutkn/Documents/STA404/Week06")
dat <- read_xpt("LLCP2019.XPT ") #make sure there's a space after XPT
head(dat)
dim(dat)

#Too many variables and observations
#refer to the codebook: https://www.cdc.gov/brfss/annual_data/2019/pdf/codebook19_llcp-v2-508.HTML
#Only focus on Ohio.
ohio_brfss <- dat %>% filter(X.STATE == 39) #why there's a X. not as what's in the codebook?
write.csv(ohio_brfss, file="ohioBRFSS2019.csv")
#--------------------------------------------------------------------------------------------#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# End of the data preparation step
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tidyverse)

## (2) Read data in R
oh_brfss <- read_csv("File path/ohioBRFSS2020.csv")
#or
oh_brfss <- read_csv("ohioBRFSS2020.csv")
head(oh_brfss)

## (3) Data manipulation and plot exercises











# Q0. Take a look at code book for the PHYSHLTH and MENTHLTH variables.
# PHYSHLTH: Numerical
# MENTHLTH Numerical
# 1-30; 77 not sure; 88 none; 99refused;


# Q1. Narrow down the data to a small number of columns: 
#     {IMONTH IDAY PHYSHLTH MENTHLTH}, and understand the meaning of 
#     each of the columns.
oh_clean <- select(oh_brfss, IMONTH,IDAY,PHYSHLTH,MENTHLTH)
head(oh_clean)


# Q2. Convert the "None" for Physical and Mental health issues to 0 days. 
oh_clean <- oh_clean %>% 
    mutate(PHYSHLTH=ifelse(PHYSHLTH == 88, 0, PHYSHLTH)) %>% 
    mutate(MENTHLTH=ifelse(MENTHLTH == 88, 0, MENTHLTH))

  table(oh_clean$PHYSHLTH)

# Q3. Keep only complete records for values for health responses (drop 
#     the "not asked or missing", "don't know/not sure", "refused", etc.).

  oh_clean <- oh_clean %>% filter(PHYSHLTH <= 30, MENTHLTH <=30)
  table(oh_clean$MENTHLTH)
  
  
# Q4. Display the first 6 lines of the cleaned data.
  head(oh_clean)
# Q5. Make a plot of individual measurements for the relationships between 
#     physical and mental health,use appropriate method to show the relationship 
#     more clearly, and avoid some problems with just using the basic plot.  
  
ggplot(data=oh_clean) +
  geom_count(aes(x = PHYSHLTH, y = MENTHLTH), alpha=0.7)
  
  # There are multiple observations for physhlth and mentalhealth combination
  # we can add jitter and alpha scaling
  #geom_jitter
  

  ggplot(data=oh_clean) +
    geom_jitter(aes(x = PHYSHLTH, y = MENTHLTH), alpha=0.2)


  
# Q6. Create monthly summaries for both mental and physical health, 
#     show the result in a table.
  
  oh_summary <- oh_clean %>%group_by(IMONTH) %>% 
    summarize(phy_ave = mean(PHYSHLTH), 
              mtl_ave = mean(MENTHLTH))
  oh_summary
  
# Q7. Make a plot that shows the relationship between the above calculated monthly 
#     summaries of mental and physical health in one plot, where each month is 
#     distinguished by color.

  
  ggplot() +
    geom_point(aes(x=phy_ave, y=mtl_ave, color=IMONTH), data = oh_summary) +
    labs(x="Physical Health", y= "Mental Health") +
    ggtitle("Results from BRFSS", subtitle="State of Ohio, 2020") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    theme_bw()



#==============================#
# Part 7: Tidyr and Lineplots  #
#==============================#

### (7.1) tidyr? ###
## The goal of tidyr is to help you create tidy data. 
## What is a tidy data? https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html
#  Tidy data is data where:
#  Every column is variable.
#  Every row is an observation.
#  Every cell is a single value.
#  More examples and explanation: https://r4ds.had.co.nz/tidy-data.html

### (7.2) The GDP data ###
# The following is from FRED (Federal Reserved Bank Economic Data)
# https://fred.stlouisfed.org/series/GDPC1
# Download it directly from the website (change the date format
# to yyyy-mm-dd) or from canvas.
# Date from 1947-01-01 to 2021-10-01
gdpdata <- read_csv("GDPC1.csv")

## Run the following if you're not able to read in the 
## first column as a date variable successfully
gdpdata <- read_csv("GDPC1.csv",
                    col_types = cols(
                      DATE = col_date(format = "")
                    ))


gdpdata
glimpse(gdpdata)
gdpdata$GDPC1



gdpdata
## Sometimes the numbers after the decimal points are
## not displayed. You can adjust the length with
## pillar.sigfig: The number of significant digits that 
## will be printed and highlighted, default: 3
## (If there're more than 3 digits in integer, the integers
## will be fully displayed. )

options(pillar.sigfig=6)
gdpdata


### (7.3) Make a line plot (quarterly) ###
#install.packages("ggthemes")
library(ggthemes) ## To add some additional themes



?geom_line

ggplot(data=gdpdata) +
  geom_line(aes(x = DATE, y = GDPC1)) +
  labs(x="Year-Quarter", y= "Billions of Chained 2012 Dollars", caption="Source: U.S. Bureau of Economic Analysis") +
  ggtitle("Quartly Real Gross Domestic Product", subtitle="1947-01-01 to 2020-10-01, Seasonaly Adjusted Anual Rate") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  theme_update()


# Add title/subtitle: https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/
# theme_economist(): refer to https://www.datanovia.com/en/blog/ggplot-themes-gallery/#theme_economist
# Chained dollars is a method of adjusting real dollar 
# amounts for inflation over time, so as to allow comparison 
# of figures from different years.


### (7.4) Make a line plot (annual average) ###
###       Working with dates                ###

## (Step 1): Compute the average annual GDP
## (Step 2): Make the line plot

gdpdata %>% 
  group_by() %>% 
  summarize(mean())
# What's wrong with my calculation? 
class(gdpdata$DATE)
# What is "Date" type?


## (1) Dates introduction
## Dates are represented as the number of days since 1970-01-01, 
## with negative values for earlier dates.
today <- Sys.Date() #returns the current date
today
class(today) 
date() #returns current date and time

# format() function
# It formats and R object for pretty printing.


# We cant to have this format
# "Tuesday, September-27-2022"


format(today, "%A,%B-%d-%Y")
# %a weekday in abbreviated; %A unabbreviated
# %b month in abbreviated %B unabreviated
# %y 2 digits for the year; %y 



# Refer to this for specific date formats.
# https://www.statmethods.net/input/dates.html

## The following example is to show the date is stored in R as integer values clearly.


Datevec <- c("01/01/2020", "03/10/2021")
class(dates)


## Convert character string to date variable.
dates <-  as.Date(Datevec, "%m/%d/%Y")
class(dates)
dates
dates[2]-dates[1]

Datevec2 <- c("March05/2021")
as.Date(Datevec2)
as.Date(Datevec2, "%B%d/%Y")
dateVec3 <- c("Jan01/2021", "2020September01")


as.Date(Datevec3, "%B%d/%Y")




## (2) Work on date/time via "lubridate" package 

library(lubridate)
## https://lubridate.tidyverse.org/ 
## refer to the descriptions and cheat sheet
?is.Date
is.Date(today)
is.Date(gdpdata$DATE)


## Time
hms::as_hms() 
# a hms is a time stored as the number of seconds since 00:00:00
# hms is a package inside lubridate.

"2022Mar2"
ymd("2022Mar2") # By default, it uses "-" to link these numbers.
class(ymd("2022Mar2"))

"03022022 10:30:25"
year(ymd("2022Mar02"))



##Your Turn##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Make the line plot using annual average GDP data.
## (Step 1): Generate a summarized data containing the average annual GDP
gdp_annual



## (Step 2): Make the line plot



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%








#====================================#
# Part 8: Wide and Long format data  #
#         Work with strings and date #
#====================================#

### (8.1) Wide and Long format data introduction ###
# From wikipedia (https://en.wikipedia.org/wiki/Wide_and_narrow_data)

## Wide data (unstacked data)
# Each different data variable in a separate column.
# 
# Person	Age	  Weight	Height
# Bob   	32	  128	    180
# Alice	  24	  86	    175
# Steve	  64	  95	    165
#
# ---------------------------------------------------
#
## Long (Narrow, stacked data)
# One column containing all the values and 
# another column listing the context of the value
# 
# Person	Variable	Value
# Bob	    Age	      32
# Bob   	Weight	  128
# Bob	    Height	  180
# Alice 	Age	      24
# Alice	  Weight	  86
# Alice	  Height	  175
# Steve	  Age	      64
# Steve	  Weight	  95
# Steve	  Height	  165
# This is often easier to implement; 
# addition of a new field does not require any changes
# to the structure of the table, 
# however it can be harder for people to understand.

### (8.2) Change long-format data to wide-format ###

head(gdpdata)
# the gdpdata is long-format data, but we need to add more information

## (1) Working with character strings

## a. Concatenate
?paste
?paste0
# paste: Concatenate vectors after converting to character.
#        syntax: paste (…, sep = " ", collapse = NULL)
# paste0: is equivalent to paste(…, sep = "", collapse), 
#         slightly more efficient.



## b. Extract a sub-string
"2022-March-02"
?str_sub
# str_sub is from the stringr package. 
# syntax: str_sub(string, start=, end=)
# It can be used to substring some characters.



## How about this?



?str_trim #default trims both sides
# str_trim sometimes is useful to remove the spaces before or after




## (2) Add the year, month, quarter information. 

## (1) Change data from long to wide via "pivot_wider()"

## a. Use the gdpdata to create a table gdp_wide_quarter with head as follows:
# # A tibble: 6 x 5
#   Year     Quarter1    Quarter2    Quarter3    Quarter4
#   <dbl>     <dbl>       <dbl>       <dbl>        <dbl>
#1  1947      2033.1      2027.6      2023.5      2055.1
#2  1948      2086.0      2120.4      2132.6      2135.0
#3  1949      2105.6      2098.4      2120.0      2102.3
#4  1950      2184.9      2251.5      2338.5      2383.3
#5  1951      2415.7      2457.5      2508.2      2513.7
#6  1952      2540.6      2546.0      2564.4      2648.6

?pivot_wider



## Add a question mark between Quarter and number in gdpdata2:


## In R regex there are some metacharacters 
# (like . \ | ( ) [ { ^ $ * + ?) which have special meanings,
# you can add a backslash \ to let R know that you want to just use
# that specific symbol. Here you use \\ since \ itself has a special meaning. 
# See https://stackoverflow.com/questions/20450525/what-does-mean-in-r-regular-expression
#     https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
##    http://www.regular-expressions.info/rlanguage.html
names(gdp_wide_quarter)
gdp_wide_quarter



## Similarly, the column names can be month.




## b. Add a name prefix in pivot_wider
## here, we can follow what we did before, but add the Quarter2 variable to the data, 
## then run the following code



