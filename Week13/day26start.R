#-----------------------------------------------#
#                 Week 13--Day26                #
#       Shiny App Example 3 (Flight Data)       #
#-----------------------------------------------#

#===============================#
# Part 24: Shiny App Example 3: #
#           Flight Delays Data  # 
#===============================#
## Let's work on the airline data on days 25, 26, and 27
setwd("C:/Users/Gutkn/Documents/STA404/Week13")
load("Jan2020Flight.Rdata")
library(tidyverse)
library(shiny)
library(lubridate)
library(ggthemes)
#head(delay)
# From the Federal Airlines Administration (FAA), flight records for every U.S.
# air flight for January 2020. The original CSV file is 95.4MB. 
# The `.RData` file has been trimmed and is in R format.
# The original data can be obtained here for those interested:
# https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time

## Let's create a shiny app that contains two plots:
## Plot 1: a line plot, which shows some delay statistics
##         (e.g. mean, std departure delay, bad delays, etc),
##         and highlight several user specified airports.
## 
## Plot 2: a map which shows the USA map. The center of circles
##         represents the airport location, the size of circle
##         represents how large the statistic is. 
#          The user can select a specific statistic, 
#          a date range and display the plot.
##
#


### (24.1) Data Preparation ###
## Find top 10 busiest airports :
busiest <- delay %>%
  group_by(ORIGIN_CITY_NAME) %>%
  summarize(count=n() ) %>%
  arrange(desc(count)) %>% 
  head(10) 

# delay %>% filter(ORIGIN_CITY_NAME=="New York, NY")
## Aggregate by airport
time_airport_stats <- delay %>%
  mutate(wday = wday(FL_DATE, label=T)) %>%
  group_by(FL_DATE,ORIGIN,ORIGIN_CITY_NAME) %>%
  summarize(avg_dep_delay = mean(DEP_DELAY, na.rm=T),
            sd_dep_delay=sd(DEP_DELAY,na.rm=T),
            count=n(),
            bad_delays=quantile(DEP_DELAY,.9,na.rm=T)) %>%  #defined the bad delay
  as.data.frame()

# %in% operator: check if an element belongs to a vector.
1:2 %in% seq(1:5)
seq(1:5) %in% 1:3



top_airports <- filter(time_airport_stats,
                       ORIGIN_CITY_NAME %in% busiest$ORIGIN_CITY_NAME,
                       FL_DATE >= ymd("2020-01-01"),
                       FL_DATE <= ymd("2020-01-31"))


select_airport <- filter(top_airports,ORIGIN_CITY_NAME %in% c("Atlanta, GA"))
# choices=c("Average Departure Delay"="avg_dep_delay",
#           "Std. Dev. of Departure Delay"="sd_dep_delay",
#           "Bad Delays (90th perc.)"="bad_delays")

# Set up y labels for timeplot
# plot_ylabs <- c(avg_dep_delay="Average Departure Delay (Minutes)",
#                 sd_dep_delay="Std. Dev. of Departure Delay (Minutes)",
#                 bad_delays="Bad Delays \n(90th percentile for delay in minutes)")

# plot_ylabs["avg_dep_delay"]


############
## Plot 1 ##
############
# Make a line plot of the top 10 busiest airport, then highlight the one you selected.
ggplot()+
  geom_line(aes_string(x="FL_DATE",y="sd_dep_delay",
                       group="ORIGIN_CITY_NAME"),
            data=top_airports)+
  geom_line(aes_string(x="FL_DATE",y="sd_dep_delay",
                       group="ORIGIN_CITY_NAME",
                       color="ORIGIN_CITY_NAME"),
            data=select_airport,size=2)+
  theme_bw() +
  labs(title="Flights in January 2020", x="Date",
       y="Std. Dev. of Departure Delay (minutes)",
       caption="Source: Bureau of Transportation Statistics")+
  guides(color=guide_legend("City"))




############
## Plot 2 ##
############

# prepare the dataset needed.
# (1) We need the state outlines data:  map_data("state")
# (2) We need the location of the airports: search for such data in google
# Refer to https://openflights.org/data.html     download the airports.dat
airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header=FALSE)

airports <- airports %>%
  select(V5, V7, V8) %>%
  rename(AirportCode = V5,
         lat=V7,
         long=V8)

airport_all <- left_join(time_airport_stats, airports,
                         by=c("ORIGIN"="AirportCode")) %>%
  filter(count>10)


# subset the data, then make the plot
airport_map_data <- airport_all %>%
    filter(!str_sub(ORIGIN_CITY_NAME,
                    str_length(ORIGIN_CITY_NAME)-1,
                   str_length(ORIGIN_CITY_NAME) ) %in% c("AK", "HI", "PR", "VI") ) %>%
  filter( FL_DATE >= ymd("2020-01-01"),
          FL_DATE <= ymd("2020-01-31")) %>%
  group_by(ORIGIN, ORIGIN_CITY_NAME,long,lat) %>%
  summarize(avg_dep_delay = weighted.mean(avg_dep_delay,weights=count,na.rm=T),
            sd_dep_delay = weighted.mean(sd_dep_delay,weights=count,na.rm=T),
            bad_delays =  weighted.mean(bad_delays,weights=count,na.rm=T),
            count = sum(count))
#?weighted.mean

head(airport_map_data)
ggplot() +
  geom_path(aes(x=long, y=lat, group=group), data=map_data("state"),
            color="gray50") +
  geom_point(aes_string(x="long", y="lat", fill="avg_dep_delay",
                        size="count"), color="gray30", shape=21,
             data=airport_map_data) +
  coord_map() +
  theme_map() +
  theme(legend.position="bottom")+
  scale_fill_gradient2(low="darkblue", mid="white",high="darkred",
                       midpoint = 0)





### (24.2) Shiny app ###

## (1) Necessary elements in the app

## (2) Main app material

ylabs <- c("Average Departure Delay"="avg_dep_delay",
           "Std. Dev. of Departure Delay"="sd_dep_delay",
           "Bad Delays (90th perc.)"="bad_delays")

library(RColorBrewer)
?brewer.pal

plotcolor<- brewer.pal(n=10,name = "Paired")

names(plotcolor) <- sort()

### Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "Flight Delays App"),
  sidebarLayout(
    
    # Sidebar typically used to house input controls
    sidebarPanel(
      # put your input controls here!
      selectInput(inputId="stat", label="Select a Departure Delay Statistic",
                  choices=c("Average Departure Delay"="avg_dep_delay",
                            "Std. Dev. of Departure Delay"="sd_dep_delay",
                            "Bad Delays (90th perc.)"="bad_delays")),
      
      selectInput(inputId="city", label="Select Airport to Highlight",
                  choices=busiest$ORIGIN_CITY_NAME, multiple = TRUE),
      
      ## Add the date range input: 
      ## https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
      ## https://shiny.rstudio.com/reference/shiny/0.14/dateRangeInput.html
      
      dateRangeInput(inputId = "dates", label = "Pick the date range", 
                     min = "2020-01-01", max = "2020=01-31",
                     start = "2020-01-02", end = "2020-01-30",
                     format = "mm/dd/yy")
      
    ) , 
    
    # Main panel typically used to display outputs
    # To display multiple tabs, use "Tabsets"
    # refer to https://shiny.rstudio.com/articles/layout-guide.html
    
     mainPanel(
       #plotOutput(outputId="timeplot"),
       #plotOutput(outputId="mapplot")
       
       tabsetPanel(
         tabPanel("Line Plot", plotOutput(outputId="timeplot")),
         tabPanel("Map Plot", plotOutput(outputId="mapplot"))
       )
     )
  )
)


### Define server behavior for application here
server <- function(input, output) {
  output$timeplot <- renderPlot({
    
    top_airports <- filter(time_airport_stats,
                           ORIGIN_CITY_NAME %in% busiest$ORIGIN_CITY_NAME,
                           FL_DATE >= min(ymd(input$dates)),
                           FL_DATE <= max(ymd(input$dates)))
    
    select_airport <- filter(top_airports,ORIGIN_CITY_NAME %in% input$city)
    
    
    ggplot()+
      geom_line(aes_string(x="FL_DATE",y=input$stat,
                           group="ORIGIN_CITY_NAME"),
                data=top_airports)+
      geom_line(aes_string(x="FL_DATE",y=input$stat,
                           group="ORIGIN_CITY_NAME",
                           color="ORIGIN_CITY_NAME"),
                data=select_airport,size=2)+
      theme_bw() +
      labs(title="Flights in January 2020", x="Date",
           y=names(ylabs)[ylabs==input$stat],
           caption="Source: Bureau of Transportation Statistics")+
      guides(color=guide_legend("City"))
  })
  
  output$mapplot <- renderPlot({
    airport_map_data <- airport_all %>%
      filter(!str_sub(ORIGIN_CITY_NAME,
                      str_length(ORIGIN_CITY_NAME)-1,
                      str_length(ORIGIN_CITY_NAME) ) %in% c("AK", "HI", "PR", "VI") ) %>%
      filter( FL_DATE >= min(ymd(input$dates)),
              FL_DATE <= max(ymd(input$dates))) %>%
      group_by(ORIGIN, ORIGIN_CITY_NAME,long,lat) %>%
      summarize(avg_dep_delay = weighted.mean(avg_dep_delay,weights=count,na.rm=T),
                sd_dep_delay = weighted.mean(sd_dep_delay,weights=count,na.rm=T),
                bad_delays =  weighted.mean(bad_delays,weights=count,na.rm=T),
                count = sum(count))
    
    ggplot() +
      geom_path(aes(x=long, y=lat, group=group), data=map_data("state"),
                color="gray50") +
      geom_point(aes_string(x="long", y="lat", fill=input$stat,
                            size="count"), color="gray30", shape=21,
                 data=airport_map_data) +
      coord_map() +
      theme_map() +
      theme(legend.position="bottom")+
      scale_fill_gradient2(names(ylabs)[ylabs=input$stat], low="darkblue", mid="white",high="darkred",
                           midpoint = 0)
  })
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)
ggplot()
# Conditional panel
# https://shiny.rstudio.com/gallery/conditionalpanel-demo.html
