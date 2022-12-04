# -----------------------------------------------#
#                 Week 05--Day09                 #
#         Data Cleaning and Manipulationan       #
#------------------------------------------------#


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
## Recall in day05/day06, we generated many summary dataset. 
## Some further questions:
## (1) How many of the diamonds have clarity type VS1 or VVS2?
library(tidyverse)
dim(diamonds)
dim(diamonds[diamonds$clarity=="VS1"|diamonds$clarity=="VVS2",])[1]
dim(filter(diamonds,clarity=="VS1"|clarity=="VVS2",))[1]

diamonds%>%filter(clarity=="VS1"|clarity=="VVS2") %>% sumarize(n())


## (2) What's the 70th percentile of the diamonds carat for 
##     those diamonds?

diamonds%>%filter(clarity=="VS1"|clarity=="VVS2") %>% 
  summarize(pert_70=quantile(diamonds$carat,0.7))



## (2) What's the 70th percentile of carat for the diamonds mentioned
##     above?



### (6.3) Tennis Data ###
### select, mutate, group_by, summarize, arrange, etc. ###

## Refer the data sets and description from the website:
## https://github.com/JeffSackmann/tennis_atp
## There are multiple files per season. We will use the 
## tour-level qualifying and challenger main-draw matches 
## data for 2021 season.

## (1) Read the data into R
?read_csv
tennis <- read_csv("atp_matches_2021.csv")


# R will scan the first 1000 lines of data to determine the variable type.
# See the following website:
# https://uomresearchit.github.io/r-tidyverse-intro/03-loading-data-into-R/




# or download from Canvas, then load into R
#setwd("---Set the path to your dataset here---")
#tennis <- read_csv("atp_matches_qual_chall_2021.csv")

# If you want to speciy the type of a certain variable, can use
# the following syntax:
tennis <- read_csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_qual_chall_2021.csv",
                    col_types=cols(
                      loser_seed=col_character()))

# note use diff object names for same variables

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

# print var names using col names
colnames(tennis)

# to achieve the same goal, we can delete the variable we dont want
ts_d <- select(tennis, -loser_id:-loser_ioc)
dim(ts_d)


## (3) How many upset matches are there in total?

## a. Detect upset games
## Create an indicator column, specifying whether a
## match is an upset. (A person with a higher rank
## lose the same)
## rank  win or lose
## 1(3)    lose(win) --> upset     
ts_upset <- mutate(ts,upset=1*(winner_rank>loser_rank))
ts_upset[,17:19]

## b. Count the total number of upset matches

## What's wrong with the calculation? 
?sum
sum(ts_upset$upset,na.rm = TRUE)

# na.rm = TRUE means remove missing values
# na.rm = FALSE keep mising values and it is false by defulat

summarize(ts_upset,sum(upset,na.rm=TRUE))


## (4) What if I want to count how many games each player win?
# This is a review and you can try it by yourself

ts_winsByNameGroup <- group_by(ts,winner_name)
summarize(ts_winsByNameGroup,nwin=n())


## (5) Who had the most number of wins? Reorder data
ts_winsByNameGroup <- group_by(ts,winner_name)
ts_win <- summarize(ts_group,n_win=n())
ts_win <- arrange(ts_win,desc(n_win))



## (6) For the player that has the most number of wins,
## Let's calculate (a) average total time of a game; 
##                 (b) the total number of games;
## that this player had on different surface.
## surface average_total_time  total_#_of_games
## hard
## clay
## grass

winmost <- filter(ts,winner_name=="Tomas Martin Etcheverry" | loser_name=="Tomas Martin Etcheverry")
winmost_group <- group_by(winmost,surface)
summarize(winmost_group,ave_time=mean(minutes,na.rm=TRUE),
          tot_game=n())


## (7) What if I want to automate the process (4), (5), (6)
## for other years of data, without need to run the code separately
## Review: short cut for %>% : "ctrl"+"shift"+"M"

## a. Find the player
## Recall (4): Compute the number of wins for each player

ts_winsByNameGroup <- group_by(ts,winner_name)
summarize(ts_winsByNameGroup,nwin=n())


## Recall (5): Find the player with the most number of wins

ts_winsByNameGroup <- group_by(ts,winner_name)
ts_win <- summarize(ts_group,n_win=n())
ts_win <- arrange(ts_win,desc(n_win))

## Automate steps (4) and (5)
step4and5 <- group_by(winner_name) %>% summarize(n_win=n()) %>% arrange(desc(n_win))

step4and5


?top_n
?select
## top_n(x, n, wt): x is the data, n is the number of rows to return, 
##                  wt is the variable to use for ordering.
##                  if not specified, defaults to the last variable in x.

## select(): can choose which variable to be selected.  


## b. Do the computation


## Recall (6): Clculate the statistic for the selected player
winmost <- filter(ts,winner_name=="Tomas Martin Etcheverry" | loser_name=="Tomas Martin Etcheverry")
winmost_group <- group_by(winmost,surface)
summarize(winmost_group,ave_time=mean(minutes,na.rm=TRUE),
          tot_game=n())




## Automate step (6)

step6auto <- ts %>% filter(winner_name=="Tomas Martin Etcheverry" | loser_name=="Tomas Martin Etcheverry") %>%
  group_by(surface) %>% summarize(avg_time=mean(minutes,na.rm=TRUE), tot_game=n())
step6auto


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



