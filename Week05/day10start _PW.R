# -----------------------------------------------#
#                 Week 05--Day10                 #
#         Data Cleaning and Manipulation 2       #
#------------------------------------------------#
#=========================================#
# Part 6: Data Cleaning and Manipulation  #
#=========================================#
### (6.3) Tennis Data ###
### select, mutate, group_by, summarize, arrange, etc. ###

## Refer the data sets and description from the website:
## https://github.com/JeffSackmann/tennis_atp
## There are multiple files per season. We will use the 
## tour-level qualifying and challenger main-draw matches 
## data for 2021 season.
library(tidyverse)


tennis <- read_csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2021.csv")
ts <- select(tennis, tourney_id:surface, tourney_date, 
             match_num, winner_id,
             winner_name, winner_hand,winner_age,
             loser_id,loser_name, loser_hand,loser_age,
             minutes,winner_rank,winner_rank_points,
             loser_rank,loser_rank_points)


## (4) What if I want to count how many games each player win?
ts_win_grouped <- group_by(ts, winner_name)
ts_win_grouped
ts_win <- summarise(ts_win_grouped,
                    n_win=n())#.groups = "keep")
ts_win
# `summarise()` ungrouping output (override with `.groups` argument)
?summarise



## (5) Who had the most number of wins? Reorder data
ts_win <- arrange(tw_win,desc(n_win))
ts_win



## (6) For the player that has the most number of wins,
## Let's calculate (a) average total time of a game; 
##                 (b) the total number of games;
## that this player had on different surface.
## surface average_total_time  total_#_of_games
## hard
## clay
## grass

winmost <- filter(ts, winner_name=="Daniil Medvedev" | loser_name=="Daniil Medvedev")
# Group data by surface
winmost_group <- group_by(winmost,surface)
# summarize the data ; remove NA values using na.rm = TRUE
summarize(winmost_group, ave_time=mean(minutes, na.rm=TRUE),tot_game=n())


## (7) What if I want to automate the process (4), (5), (6)
## for other years of data, without need to run the code separately
## Review: short cut for %>% : "ctrl"+"shift"+"M"

## a. Find the player
## Recall (4): Compute the number of wins for each player
ts_win_grouped <- group_by(ts, winner_name)
ts_win <- summarise(ts_win_grouped, n_win=n())#.groups = "keep")

## Recall (5): Find the player with the most number of wins
ts_win <- arrange(tw_win,desc(n_win))



## Automate steps (4) and (5)
  # take original data set and group by winner name
  # summarize the data by number of wins
  # arrange the data by descending number of wins

ts %>% group_by(winner_name) %>%
  summarize(n_win=n()) %>% 
  arrange(desc(n_win))

# take original data set and group by winner name
# summarize the data by number of wins
# return the first win presented
ts %>% group_by(winner_name) %>%
  summarize(n_win=n()) %>% 
  top_n(1,n_win)

# take original data set and group by winner name     MORE DATA
# summarize the data by number of wins
# return the first win presented
# then only show the winner name                      LESS DATA
#
#
#                                                 You take data away as
#                                                 you go down and filter it.
ts %>% group_by(winner_name) %>%
  summarize(n_win=n()) %>% 
  top_n(1,n_win) %>% 
  select(winner_name)


?top_n
?select
## top_n(x, n, wt): x is the data, n is the number of rows to return, 
##                  wt is the variable to use for ordering.
##                  if not specified, defaults to the last variable in x.

## select(): can choose which variable to be selected.  


## b. Do the computation


## Recall (6): Clculate the statistic for the selected player
winmost <- filter(ts, winner_name=="Daniil Medvedev" | loser_name=="Daniil Medvedev")
# Group data by surface
winmost_group <- group_by(winmost,surface)
# summarize the data ; remove NA values using na.rm = TRUE
summarize(winmost_group, ave_time=mean(minutes, na.rm=TRUE),tot_game=n())


## Automate step (6)

winmost %>%
  filter(winner_name=="Daniil Medvedev" | loser_name=="Daniil Medvedev") %>%
  group_by(surface) %>% 
  summarize(ave_time=mean(minutes, na.rm=TRUE),tot_game=n())


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

test<-mutate(diamonds,newvar=ifelse(clarity=="SI1"|
                                    clarity=="SI2","SI", as.character(clarity)))
head(test)

# Any problems? The other categories appear as 4, 5, 6, 
# instead of the original characterse

# Any problem? 



## (2) Change the clarity variable to character first, then do the assignment. 

test<-mutate(diamonds,newvar=ifelse(clarity=="SI1"|
                                      clarity=="SI2","SI", as.character(clarity)))

head(test)

class(test$newvar)
class(diamonds$clarity)


## (3) change the new variable to be ordered factor
test=diamonds %>% 
  mutate(newvar=diamonds,newvar=ifelse(clarity=="SI1"|
                                         clarity=="SI2","SI", as.character(clarity)))
test
class(test$newvar)
test$newvar 
diamonds$clarity 
levels(test$newvar)
levels(diamonds$clarity)

## ?Anthying wrong?
# The order of the levels does not match with the original clarity data.
# We can specify the levels explicitly, to overcome the problem. 

test=diamonds %>% mutate(newvar=ordered(ifelse(clarity=="SI1"| clarity=="SI2","SI", as.character(clarity)), 
                                        levels=c("I1", "SI", "VS2", "VS1", "VVS2", "VVS1", "IF")))

head(test)
class(test$newvar)
test$newvar

## (4) Nested ifelse statements
# https://www.listendata.com/2017/03/if-else-in-r.html
# https://statisticsglobe.com/nested-ifelse-statement-in-r
#
# Change VS2 and VS1 to VS    Change VVS2 and VVS1 to VVS
# Calculate the average carat based on different level at this new variable
# make a new graph for the variable


test2 <- mutate(diamonds, newvar = ifelse(clarity=="VS2"|clarity=="VS1", "VS",  as.character(clarity),
                                  ifelse(clarity=="VVS2"| clarity=="VVS1","VVS", as.character(clarity))))
head(test2)





test2 = mutate(diamonds, newvar = ifelse(clarity=="SI1"|clarity=="SI2", "SI",
                                         ifelse(clarity=="VS1"| clarity=="VS2","VS", 
                                                ifelse(clarity=="VVS1" | clarity =="VVS2", "VVS", as.character(clarity))),
  levels=c("I1", "SI", "VS", "VVS", "IF")))




test <- mutate(diamonds, newvar=ordered(
  ifelse(clarity=="SI1"|clarity=="SI2", "SI",
         ifelse(clarity=="VS1"| clarity=="VS2","VS",
                ifelse(clarity=="VVS1" | clarity =="VVS2", "VVS", as.character(clarity)))),
         levels=c("I1", "SI", "VS", "VVS", "IF")))


test %>% group_by(newvar) %>% summarize(ave_carat=mean(carat))


ggplot() +
  geom_bar(aes(x=newvar), data=test)


# not use nested ifelse
test <- mutate(diamonds, newvar = ifelse(clarity="SI1"|clarity="SI2", "SI",
                                        as.character(clarity))) %>% 
  mutate(newvar2 = ifelse(newvar=="VS1"|newvar=="VS2", "VS", as.character(newvar) )) %>% 
  mutate(newvar3 = ifelse(newvar2=="VVS1"|newvar2=="VVS2", "VVS", as.character(newvar2)),
         levels=c("I1", "SI", "VS", "VVS", "IF"))



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
# Download the 2019 Behavioral Risk Factor Survalence System (BRFSS) as 101M SAS Export File
# from CDC Website: https://www.cdc.gov/brfss/annual_data/annual_2019.html
# Go to "Data Files": 2019 BRFSS Data (SAS Transport Format) [ZIP 101 MB]
# What is a .xpt file: https://www.loc.gov/preservation/digital/formats/fdd/fdd000464.shtml

# install.packages("haven")
library(haven)
setwd("C:/Users/Gutkn/Documents/STA404/Week05")
dat <- read_xpt("LLCP2019.XPT ") #make sure there's a space after XPT
head(dat)
dim(dat)

#Too many variables and observations
#refer to the codebook: https://www.cdc.gov/brfss/annual_data/2019/pdf/codebook19_llcp-v2-508.HTML
#Only focus on Ohio.
ohio_brfss <- dat[dat[,1] == 39,]  
write.csv(ohio_brfss, file="ohioBRFSS2019.csv")
#--------------------------------------------------------------------------------------------#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# End of the data preparation step
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## (2) Read data in R
oh_brfss <- read_csv("C:/Users/Gutkn/Documents/STA404/Week05/ohioBRFSS2019.csv")
#or
oh_brfss <- read_csv("ohioBRFSS2019.csv")
head(oh_brfss)

## (3) Data manipulation and plot exercises
# Q0. Take a look at code book for the PHYSHLTH and MENTHLTH variables.

# Q1. Narrow down the data to a small number of columns: 
#     {IMONTH IDAY PHYSHLTH MENTHLTH}, and understand the meaning of 
#     each of the columns.
ohio <- select(oh_brfss, IMONTH,IDAY,PHYSHLTH,MENTHLTH)
head(ohio)


# Q2. Convert the "None" for Physical and Mental health issues to 0 days. 
q2Data = ohio %>% mutate(PHYSHLTH=ifelse(PHYSHLTH == 88, 0, PHYSHLTH)) %>% 
                  mutate(MENTHLTH=ifelse(MENTHLTH == 88, 0, MENTHLTH))
q2Data

head(q2Data, 30)


# Q3. Keep only complete records for values for health responses (drop 
#     the "not asked or missing", "don't know/not sure", "refused", etc.).





# Q4. Display the first 6 lines of the cleaned data.
# Q5. Make a plot of individual measurements for the relationships between 
#     physical and mental health,use appropriate method to show the relationship 
#     more clearly, and avoid some problems with just using the basic plot.  
# Q6. Create monthly summaries for both mental and physical health, 
#     show the result in a table.
# Q7. Make a plot that shows the relationship between the above calculated monthly 
#     summaries of mental and physical health in one plot, where each month is 
#     distinguished by color.




