# -----------------------------------------------#
#                 Week 07--Day12                 #
#               Tidyr, Lineplots,                #
#                   Date/Time                    #
#------------------------------------------------#

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
library(SASxport)
setwd("C:/Users/Gutkn/Documents/STA404/Week06")
library(tidyverse)
gdpdata <- read_csv("GDPC1.csv")

## Run the following if you're not able to read in the 
## first column as a date variable successfully
gdpdata <- read_csv("GDPC1.csv",
                    col_types = cols(
                      DATE = col_date(format = "")
                    ))

class(gdpdata$DATE)

gdpdata
glimpse(gdpdata)
gdpdata$GDPC1
## Run the following if you're not able to read in the 
## first column as a date variable successfully
# gdpdata <- read_csv("GDPC1.csv",
#                     col_types = cols(
#                       DATE = col_date(format = "")
#                     ))


gdpdata
glimpse(gdpdata)
gdpdata$GDPC1


gdpdata
## Sometimes the numbers after the decimal points are
## not displayed. You can adjust the length with
## pillar.sigfig: The number of significant digits that 
## will be printed and highlighted, default: 3
## (If there are more than 3 digits in integer, the integers
## will be fully displayed. )




### (7.3) Make a line plot (quarterly) ###
#install.packages("ggthemes")
library(ggthemes) ## To add some additional themes



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
  group_by(DATE) %>% 
  summarize(mean(GDPC1))
# What's wrong with my calculation? 
class(gdpdata$DATE)
# What is "Date" type?



## (1) Dates introduction
## Dates are represented as the number of days since 1970-01-01, 
## with negative values for earlier dates.
## https://www.statmethods.net/input/dates.html
today <- Sys.Date() #returns the current date
today
class(today) 
date() #returns current date and time

# format() function
# It formats and R object for pretty printing.
format(today)
# Refer to this for specific date formats.
# https://www.statmethods.net/input/dates.html






## The following example is to show the date is stored in R as integer values clearly.
Datevec <- c("01/01/2020","03/02/2022")
Datevec
Datevec[2]-Datevec[1] #error message

## Convert character string to date variable.
dates <-as.Date(Datevec, "%m/%d/%y")
class(dates)
dates
dates[2]-dates[1]

Datevec2 <- c("March02/2021")
as.Date(Datevec2) #error message
as.Date(Datevec2, "%B%d%y")


Datevec3 <- c("Jan01/2021","2021September01")
as.Date(Datevec3,"%b%d/%Y")
as.Date(Datevec3,"%Y%B%d")
as.Date(Datevec3, c("%b%d%Y", "%Y%B%d"))

Datevec4 <- c("Jan01/2021","2021September01", "Jan01/2026")
as.Date(Datevec4, c("%b%d%Y", "%Y%B%d"))

Datevec5 <- as.Date("1 01 1993", "%d %m %Y")
Datevec5

## (2) Work on date/time via "lubridate" package 

library(lubridate)
## https://lubridate.tidyverse.org/ 
## refer to the descriptions and cheat sheet
?is.Date
is.Date(today)
is.Date(gdpdata$DATE)


## Time
hms::as_hms(10030) 
# a hms is a time stored as the number of seconds since 00:00:00
# hms is a package inside lubridate.

as.Date("2022Mar2", "%Y%b%d")
ymd("2022Mar2") # By default, it uses "-" to link these numbers.
class(ymd("2022Mar2"))

"03022022 10:30:25"
year(ymd("2022Mar02"))



##Your Turn##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Make the line plot using annual average GDP data.
## (Step 1): Generate a summarized data containing the average annual GDP
gdp_annual <- gdpdata %>% 
  group_by(Year) %>% 
  summarize(meanGDP = mean(GDPC1))



## (Step 2): Make the line plot

ggplot() +
  geom_line(aes(x = DATE, y = meanGDP), data = gdp_annual) +
  labs(x = "YEAR", y = "Billions of Chained 2020 Dollars",
       caption = "Source: US Bureau of Econ Analysis") +
  ggtitle("Anual Average: Real GDP",
          subtitle = "1947-2020 not seasonlly adjusted") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))


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

dateString1 <- paste("2022", "September", "29")
paste("2022", "-", "September", "-","29")


## b. Extract a sub-string
"2022-March-02"
?str_sub
# str_sub is from the stringr package. 
# syntax: str_sub(string, start=, end=)
# It can be used to substring some characters.



## How about this?



?str_trim #default trims both sides
# str_trim sometimes is useful to remove the spaces before or after
str_trim("  2022-March-02 ")
str_trim("  2022-March-02 ", side="left")



## (2) Add the year, month, quarter information. 

gdpdata2 <- gdpdata %>% mutate(Year = year(DATE),
                               Month = month(DATE),
                               Quarter=quarter(DATE))

head(gdpdata2)

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
gdp_wide_quarter <- gdpdata2 %>% 
  select(Year, GDPC1, Quarter) %>% 
  pivot_wider(names_from = Quarter, 
              values_from = GDPC1,
              names_prefix = "Quarter ")
head(gdp_wide_quarter)


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



### (8.3) Change data from wide to long

## (1) Change data from wide to long through via "pivot_longer()"
head(gdp_wide_month)

?pivot_longer




head(gdp_long)
head(gdpdata)

# Question: gdp_long looks very similar to gdpdata, but not exactly. 
# Can we edit it to look exactly the same?

## (2) Add the DATE variable 
## a. Create a new variable containing the date string


## b. Change the character date string to "Date" variable




### (8.4) More Date/Time information ###

## Portable Operating System Interface (POSIX) is a family 
## of standards specified by the IEEE Computer Society for 
## maintaining compatibility between operating systems.
## https://en.wikipedia.org/wiki/POSIX

## (1) POSIXct/POSIXlt/POSIXt data/time type intro
##c. Mention the date type variable is already a POSIX format. 

## a. Difference between variable type "Date" and "POSIXct"
Sys.Date() # Date

now = Sys.time() # Date+Time
now
class(now)
# These objects store the number of seconds (for POSIXct) or 
# days (for Date) since January 1st 1970 at midnight.
# When printed on the console, they are displayed in a 
# human-readable format along the ISO 8601 standard.

library(lubridate)
time <- ymd_hms("2021-03-11 12:30:30")
time
class(time)
is.Date(time) 
## See this for more informaiton about date/time:
## https://garthtarr.github.io/meatR/datetime.html

## b. "POSIXct" and "POSIXlt".

# There are two POSIX date/time classes, which differ 
# in the way that the values are stored internally. 
# The POSIXct class stores date/time values as the number 
# of seconds since January 1, 1970, while the POSIXlt class 
# stores them as a list with elements for second, minute, 
# hour, day, month, and year, among others. Unless you need the
# list nature of the POSIXlt class, the POSIXct class is the usual
# choice for storing dates in R.
# https://www.stat.berkeley.edu/~s133/dates.html#:~:text=POSIX%20represents%20a%20portable%20operating,allow%20modification%20of%20time%20zones.


# "POSIXct": more convenient for including in data frames
# "POSIXlt": closer to human-readable forms. 
# "POSIXt" : a virtual class that inherits from both of the classes: 
#            it is used to allow operations such as subtraction to mix 
#            the two classes.
# https://astrostatistics.psu.edu/su07/R/html/base/html/DateTimeClasses.html#:~:text=A%20virtual%20class%20%22POSIXt%22%20inherits,add%20two%20date%2Dtime%20objects.


## (2) Change characters to POSIXct/POSIXlt time
## https://www.gormanalysis.com/blog/dates-and-times-in-r-without-losing-your-sanity/
class("2022-03-09 10:10:05")
x <- as.POSIXct("2022-03-09 10:10:05")
class(x)



# But the following has problem
x <- as.POSIXct("Mar/09/21, 10-10-05")
x
# Need to specify the format
# Refer date/time format cheatsheet: https://devhints.io/datetime
y <- as.POSIXct("Mar/09/21, 10-10-05", format = "%b/%d/%y, %H-%M-%S")
y

z <- as.POSIXct("Jan/01/22", format = "%b/%d/%y")
z
y-z

## (3) Easier way via lubridate package  #

## a. Introduction
day<-"09/03/2022"
?parse_date_time



# parse_date_time always assigns the "UTC" time zone,
# "UTC" stand for "Coordinated Universal Time" and is the 
# time at the 0 meridian; somewhat similar to Greenwich Mean
# Time or "GMT".


# if you don't want to display the time zone.

## b. For complicated formats
parse_date_time("09-03-21 10-30-07")

## c. Specify time zone of the data
?ymd_hms
x <- ymd_hms("21-03-09 11:00:00")#, tz="America/New_York")
x
class(x)
# This is a list of the time zone names:
# https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
ymd_hms("2021-03-09 20:10:05", tz=)




