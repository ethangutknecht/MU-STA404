# -----------------------------------------------#
#                 Week 04--Day08                 #
#          Stacked/Grouped Bargraph Review       #
#               add numbers on graph             #
#         Data Cleaning and Manipulationan       #
#------------------------------------------------#

#=================================================#
#      Part 5: Stacked/Grouped Bargraph Review    #
#=================================================#

library(tidyverse)

cc_summary <- data.frame(table(diamonds$color, diamonds$cut))
cc_summary
colnames(cc_summary) <- c("color", "cut", "freq")
head(cc_summary)

### (5.1) Stacked bargraphs ### use variables cut and color in diamonds dataset
## (1) Height represents frequency (Original and summarized data)
ggplot()+
  geom_bar(aes(x=color, fill=cut),position ="fill", stat="count",
           data=diamonds)

## (2) Proportionally Stacked-Bargraph (Original and summarized data)



### (5.2) Grouped bargraphs (original and summarized data) ###
ggplot()+
  geom_bar(aes(x=color, fill=cut),position ="dodge", stat="count",
           data=diamonds)


## Same graph via facet_grid
ggplot()+
  geom_bar(aes(x=cut, fill=cut), data=diamonds) +
  facet_grid(.~color)


  
### (5.3) Add numbers on top of graphs ###
## (1) Bargraph
## Start with the following
ggplot()+
  geom_bar(aes(x=cut),data=diamonds)

ggplot() +
  geom_bar(aes(x=cut), data=diamonds) +
  geom_text(aes(x=cut,label=..count..), stat="count", 
          data=diamonds, vjust=-1)

# label: the value you want to display, can be ..count.., can be ..density..
# vjust: adjust where to put the numbers. 
# https://www.gl-li.com/2017/08/18/place-text-at-right-location/




## (2) Stacked bargraph
ggplot() +
  geom_bar(aes(x=cut, fill=clarity), data=diamonds, stat="count",
           position="stack") +
  geom_text(aes(x=cut,label=..count..), stat="count", 
            data=diamonds, vjust=-1)

## adjust the display of the numbers

ggplot() +
  geom_bar(aes(x=cut, fill=clarity), data=diamonds, stat="count",
           position="stack") +
  geom_text(aes(x=cut,label=..count..,group=clarity), stat="count", 
            data=diamonds,position = position_stack(vjust= 0.5), size=2.5, vjust=-1)


#position=: change the positions of where you want to put the number
# add the number to proportonally stack bargraphs

ggplot(aes(x=cut), data=diamonds) +
  geom_bar(aes(fill=clarity), stat="count", position="fill") +
  geom_text(aes(label=..count..,group=clarity), stat="count",
            position = position_fill(vjust= 0.5), size=2.5, vjust=-1)
# to change count to proportion we have to calculate the proportion
# manually and input it

## (2.a) add number on top of the grouped bargraphs

ggplot(aes(x=cut), data=diamonds) +
  geom_bar(aes(fill=clarity), stat="count", position="dodge") +
  geom_text(aes(label=..count..,group=clarity), stat="count",
            position = position_dodge(width=1), size=2.5, vjust = -3) 


#position=: change the positions of where you want to put the number.
#size: how large the number is
## Similar idea for the proportionally stacked-bargraph 
# https://stackoverflow.com/questions/44724580/add-percentage-labels-to-a-stacked-barplot

## (3) Histogram
?stat_bin

ggplot(aes(x=price),data=diamonds)+
  geom_histogram(aes(fill=cut), binwidth=3000, boundary = 0, closed="right") +
  stat_bin(aes(label=..count..),boundary = 0, binwidth = 3000, geom="text", vjust=-1) +
  scale_x_continuous(breaks = seq(0,max(diamonds$price), 3000))



#geom: The geometric object to use display the data. e.g. "bar","point","line"
#stat_bin: bin data, something related with what information you want to display, etc


## not happy with the display, maybe too many bins?




## move the number to other locations? maybe in the middle of the bars?
## also take a look at the x-axis, the numbers do not match with the bins.


## boundary=0: indicates you want to display the histogram beginning from 0.

## closed="right" or "left": specifies if a number occurs on the boundary, e.g. 3000,
##                           will it be put in the left bar, or the right bar. 
##                           right:(0,3000]   left:[0,3000)
## #scale_x_continuous(breaks=seq(min(diamonds$price),max(diamonds$price),3000))

## Refer to https://ggplot2.tidyverse.org/reference/geom_histogram.html



##color mark it by another variable





#===========================================#
# Part 6: Data Cleaning and Manipulationan  #
#===========================================#
### (6.1) dplyr Intro ###
## d: dataframe, The precursor to dplyr was called plyr. 
## 'ply': in plyr comes from an expansion/refining of 
##        the various "apply" functions in R as part of 
##        the "split-apply-combine" model/strategy. 
## Here's a good slideset that provides more insight into 
## the plyr name:
## https://www.slideshare.net/hadley/plyr-one-data-analytic-strategy


### (6.2) Diamonds Data ###
## Recall in day05, we generated many summary dataset. 
## Some further questions:
## (1) How many of the diamonds have clarity type VS1 or VVS2?
library(tidyverse)
diamonds %>% filter(clarity=="VS1" | clarity =="VVS2") %>% summarise(n=n())



## (2) What's the 70th percentile of the diamonds carat for 
##     those diamonds?
diamonds %>% filter(clarity=="VS1" | clarity =="VVS2") %>%
  summarise(percent70=quantile(diamonds$carat, 0.7))


### (6.3) Tennis Data ###
### select, mutate, group_by, summarize, arrange, etc. ###

## Refer the data sets and description from the website:
## https://github.com/JeffSackmann/tennis_atp
## There are multiple files per season. We will use the 
## tour-level qualifying and challenger main-draw matches 
## data for 2021 season.

## (1) Read the data into R
?read_csv
tennis <- read_csv("")


# R will scan the first 1000 lines of data to determine the variable type.
# See the following website:
# https://uomresearchit.github.io/r-tidyverse-intro/03-loading-data-into-R/




# or download from Canvas, then load into R
#setwd("---Set the path to your dataset here---")
#tennis <- read_csv("atp_matches_qual_chall_2021.csv")

# If you want to speciy the type of a certain variable, can use
# the following syntax:
tennis2 <- read_csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_qual_chall_2021.csv",
                    col_types=cols(
                      loser_seed=col_character()))



head(tennis,10)
tail(tennis,10)

ggplot() +
  geom_point(aes(x=winner_age, y=winner_rank_points),data=tennis) +
  theme_bw()


## (2) Data Exploration and Cleaning

## Too many variables, focus on a few
ts <- select(tennis, tourney_id:surface, tourney_date, 
             match_num, winner_id,
             winner_name, winner_hand,winner_age,
             loser_id,loser_name, loser_hand,loser_age,
             minutes,winner_rank,winner_rank_points,
             loser_rank,loser_rank_points)
dim(tennis)
dim(ts)



## (3) How many upset matches are there in total?

## a. Detect upset games
## Create an indicator column, specifying whether a
## match is an upset. (A person with a higher rank
## lose the same)
## rank  win or lose
## 1(3)    lose(win) --> upset     



## b. Count the total number of upset matches

## What's wrong with the calculation? 




## (4) What if I want to count how many games each player win?
# This is a review and you can try it by yourself





## (5) Who had the most number of wins? Reorder data




## (6) For the player that has the most number of wins,
## Let's calculate (a) average total time of a game; 
##                 (b) the total number of games;
## that this player had on different surface.
## surface average_total_time  total_#_of_games
## hard
## clay
## grass



## (7) What if I want to automate the process (4), (5), (6)
## for other years of data, without need to run the code separately
## Review: short cut for %>% : "ctrl"+"shift"+"M"

## a. Find the player
## Recall (4): Compute the number of wins for each player


## Recall (5): Find the player with the most number of wins



## Automate steps (4) and (5)




?top_n
?select
## top_n(x, n, wt): x is the data, n is the number of rows to return, 
##                  wt is the variable to use for ordering.
##                  if not specified, defaults to the last variable in x.

## select(): can choose which variable to be selected.  


## b. Do the computation


## Recall (6): Clculate the statistic for the selected player



## Automate step (6)




### (6.4) Change variables via "ifelse" ###
#  Conditional element selection
#  https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/ifelse
#
#  syntax: ifelse(test, yes, no)
#  test: logic statement
#  yes: the value you want to return if the logic statement is true
#  no: the value you want to return if the logic statement is false


diamonds$clarity
# There're 8 clarity types, after searching for the name of diamonds types, I find:
# "SI1" and "SI2"  ---> Slightly included (SI)
# "VS1", "VS2"     ---> Very Slightly included (VS)
# "VVS1", "VVS2"   ---> Very, Very Slightly included (VVS)
# Can I change the clarity type variable values according to the above assignment?
# e.g. change "SI1" and "SI2" values to "SI", just keep the 
# other clarity types the same.

## (1) Use ifelse() to make the changes




# Any problem? 



## (2) Change the clarity variable to character first, then do the assignment. 




## (3) change the new variable to be ordered factor
test=diamonds %>% 
  mutate(newvar= )
test
class(test$newvar)
test$newvar 
diamonds$clarity 
levels(test$newvar)
levels(diamonds$clarity)

## ?Anthying wrong?
# The order of the levels does not match with the original clarity data.
# We can specify the levels explicitly, to overcome the problem. 




## (4) Nested ifelse statements
# https://www.listendata.com/2017/03/if-else-in-r.html
# https://statisticsglobe.com/nested-ifelse-statement-in-r


