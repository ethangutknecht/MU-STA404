# -----------------------------------------------#
#                 Week 07--Day13                 #
#             Long/wide format data              #
#                Strings, dates                  #
#------------------------------------------------#
#====================================#
# Part 8: Wide and Long format data  #
#         Work with strings and date #
#====================================#
### (8.2) Change long-format data to wide-format ###
library(tidyverse)
library(lubridate)
gdpdata <- read_csv("GDPC1.csv")
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

paste("SomeString", 2, "more", "additional")  # default space
paste0("SomeString", 2, "more", "additional") #no space

paste("SomeString", 2, "more", "additional",sep="-") #no space
paste0("SomeString", 2,"-", "more", "additional") #no space

paste("SomeString", 2, "more", "additional",collapse=",") #no space


## b. Extract a sub-string
"2022-March-02"
?str_sub
# str_sub is from the stringr package. 
# syntax: str_sub(string, start=, end=)
# It can be used to substring some characters.

str_sub("abcdefg", 1, 4)


## How about this?
str_trim("       d          ")

gdpdata2 <- gdpdata %>% mutate(Year=year(date),
         Quarter=quarter(date),
         Month=month(date))
head(gdpdata2)


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

gdp_wide_quarter <-gdpdata2 %>% 
  select(Year, Quarter, GDPC1) %>% 
  pivot_wider(names_from = Quarter,
              values_from = GDPC1,
              names_prefix="Quarter")

## Add a question mark between Quarter and number in gdpdata2:

gdpdata3 <- gdpdata %>% mutate(Year=year(DATE),
                               Quarter=paste(quarter(DATE)),
                               Month=month(DATE))


gdp_wide_quarter <- gdpdata3 %>% 
  select(Year, Quarter, GDPC1) %>% 
  pivot_wider(names_from = Quarter, values_from = GDPC1)


# Solution 1: Use pate0 instead of paste when creating the variable Quarter
# Solution 2: Use Sep=""
# Solution 3:

?str_remove

names(gdp_wide_quarter)
str_remove(names(gdp_wide_quarter)," ")
names(gdp_wide_quarter) <- str_remove(names(gdp_wide_quarter), " ")


# Solution 1:
gdpdata2 <- gdpdata %>% 
  mutate(Year=year(DATE),
         Quarter=quarter(DATE),
         Month=month(DATE))

gdp_wide_quarter <- gdpdata2 %>% 
  select(Year, Quarter, GDPC1) %>% 
  pivot_wider(names_from= Quarter,values_from = GDPC1)

str_ready <- c("Year", "Quarter1", "Quarter2", "Quarter3", "Quarter4")
str_replace(str_ready, "er", "er ? ")



# how do we remove question mark

str_remove(names(gdp_wide_quarter, " "))
partrmv <- str_remove_all(names(gdp_wide_quarter)," ")
partrmv
str_remove(names(partrmv), "?")
str_remove(part_rmeove,"\\?")

str_replace_all(names(gdp_wide_quarter), c("\\?"))


# Similarity with column names can be month

gdpdata4 <- gdpdata %>% 
  mutate(Year=year(date),
         Quarter=quarter(date),
         Month=month(date))

gdp_wide_month <- gdpdata2 %>% 
  select(Year, Month, GDPC1) %>% 
  pivot_wider(names_from= Month,values_from = GDPC1, names_prefix = "Month ")

head(gdp_wide_month)







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

gdp_long <- gdp_wide_month %>% 
  pivot_longer(cols)


head(gdp_long)
head(gdpdata)

# Question: gdp_long looks very similar to gdpdata, but not exactly. 
# Can we edit it to look exactly the same?

## (2) Add the DATE variable 
## a. Create a new variable containing the date string

gdp_long2 <- gdp_long %>% 
  mutate(DateString = paste(Year, Month, "01", sep="-"),
         DATE=as.Date(DateString))

head(gdp_long2)


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





# in class activity IC11


strvec <- c("apple", "orange", "banana")
strvec <- str_remove(strvec,"a")
strvec <- str_remove(strvec,"e")
strvec






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



