# -------------------------------------------------#
#                    Week 12--Day23                #
#                Aggregation Based Plots           #
#                       Plotly                     #
#                      Shiny App                   #
#--------------------------------------------------#

#====================#
# Part 21: Animation # 
#====================#
# We have separated this part in question 2 of homework 9 #

#=================#
# Part 22: Plotly # 
#=================#
# https://plotly.com/ggplot2/getting-started/
# https://plotly.com/r/
# install.packages("plotly")
library(tidyverse)
library(plotly)
### (23.1) Load the data
# Load the gapminder data that contain country name, year, population, 
# life expectancy, income, child mortality, CO2 emission and region (asia,
# europe, africa, americas). 

# To prepare this dataset, you can go to this website:
# https://www.gapminder.org/data/geo/
# download the corresponding datasets and clean it similarly as what has 
# been done in part 21. 


# This is a similar dataset as the gapminder data in R directly. 
# I removed the observation corresponding to continent 
# value is NA, and added variables for children's mortality and CO2 emission.
# child mortality: death of children under five years of age per 1,000 live births.
# co2 emissions: co2 emission from the burning of fossil fuels (metric tonnes of co2 per person).

load("gapminderdata.RData")
head(gapminderdata)


###Extract the 2019 information and remove missing values in four_regions
all2019 <- gapminderdata %>%


min(all2019$Population)
max(all2019$Population)


# Look at the bubble plot for Income and LifeExp with size by Population
# and color by four_regions:
p3 <- ggplot(all2019) +
  

p3
class(p3)
?ggplotly
ggplotly(p3)
class(ggplotly(p3))
## What are the differences between the regular ggplot object 
## and the plotly object? Anything that needs to be improved?
unique(all2019$four_regions)
class(all2019$four_regions)

all2019 <- all2019 %>% 
  mutate(four_regions=factor(four_regions,labels=c("Africa","Americas","Asia","Europe")))
head(all2019)  
all2019$four_regions

p4 <- 
  
  ggplotly(p4)

## Want to display more information in the box?
## Want to control what information to be displayed.
?ggplotly
ggplotly(p4,tooltip = c("label","y","size","text"))
# https://plotly.com/r/plotly-fundamentals/

# Another tool that can create a plotly object is through plot_ly
# Some comparison between ggplotly and plot_ly: https://jtr13.github.io/spring19/community_contribution_group17.html





#=====================#
# Part 23: Shiny Apps # 
#=====================#
### (23.1) What is a shiny app, what's the structure of a shiny app ###
## View examples of shiny app: https://shiny.rstudio.com/gallery/

### (23.2) Shiny app tutorial and examples ###

## (1) Watch the tutorial

# In this website: https://shiny.rstudio.com/tutorial/ 
# -(Required) The first 26 minutes introduced how to create a shiny app. 
# -(Optional) The 26-37 minutes introduce how to share a shiny app to the Rstudio cloud. 
# Some of the information in the video is a little bit different from the current Rstudio version, 
# for example you can always see a "Run App" button on upper side of the screen.


# Find out more about building applications with Shiny here: http://shiny.rstudio.com/
# Options in the input functions and others, see 
#  -https://shiny.rstudio.com/reference/shiny/1.4.0/
#  -the shinny cheat sheet: https://www.rstudio.com/resources/cheatsheets/ (Interactive web apps with shiny cheatsheet)



## (2) Built in Shiny app examples
# install.packages("shiny")
# install.pacakges("gridExtra")
library(shiny)
library(gridExtra)
library(tidyverse)
library(plotly)
# There're many built in shiny code examples 
# available, for example:
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/#Go%20Further
#runExample("01_hello") 
#runExample("10_download")

## (3) Shiny app structure and template ##
# Every shiny app is consists of the following three parts:
# -a. a user interface object
# -b. a server function
# -c. a call to the shinyApp function

# You may start any shiny app with the following template
library(shiny)
ui <- fluidPage(
  # Input() functions
  # Output() functions
)

server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)

## (4) Simple shiny app with the diamonds data ##
# Let's chooses one variable from the diamonds data, 
# then create a shiny app which displays the histogram of 
# that variable. The user can change how many bins are there
# in the graph. 
library(shiny)

# a. Create a static plot
ggplot(data=diamonds)+
  geom_histogram(aes(x=price),bins=10)+
  theme_bw()+
  labs(x="carat",
       title="The histogram of price (usd) in diamonds data")

# Tutorial: Display reactive output
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/

# Tutorial: Add control widgets
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/


# b. Build the shiny app
ui <- fluidPage(
  # Input() functions
  sliderInput(inputId = "bins",
              label="Choose the number of bins",
              min=1, max=50, value =30),
  
  # Output() functions
  plotOutput(outputId = "hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    ggplot(data=diamonds)+
      geom_histogram(aes(x=price),bins=input$bins)+
      theme_bw()+
      labs(x="carat",
           title="The histogram of price (usd) in diamonds data")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




### (23.3) Iris data shiny app ###
# Let's create a shinny app example that works on the iris data:
# (a) The user can choose one variable from the numerical 
#     variables in the iris data.
# (b) The app will show the histogram of this variable.
#     The user can choose how many bins are there in the plot.
# (c) The user can choose another variable, then app will display
#     the scatter plot between the two variables selected.
# (d) The user can check a box "Color by Species", to add the
#     species color to both the histogram and scatter plot.

## (1) Create the static plots
names(iris)
## Suppose first variable: Sepal.Length
##         second variable: Petal.Length
ggplot(data=iris)+
  geom_histogram(aes(x=Sepal.Length),bins=5)+
  theme_bw()+
  labs(x="Sepal Legnth (cm)",
       title="The histogram of Sepal Length in Iris Data")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(iris) +
  geom_point(aes(x=Petal.Length,
                 y=Sepal.Length))+
  labs(x="Petal Length",
       y= "Sepal Length",
       title="Relationship of Sepal Length with Petal Length") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))



## (2) Build the shiny app
ui <- fluidPage(
  # Input() functions
  
  # Output() functions

)

server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)


# Some helpful links:
# - You can view more advanced layout options in:
#   https://shiny.rstudio.com/articles/layout-guide.html


# - Image related syntax: refer to the lesson 2 in shiny app tutorial:
#   https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
# - How to align images: https://github.com/rstudio/shiny/issues/555


# - Want to choose a second variable, based on the value you chosed 
#   from the first variable?
#   You may build a dynamic UI that reacts to the user input, 
#   many options, we can use the render UI option
#   https://shiny.rstudio.com/articles/dynamic-ui.html
#
#   or lesson 4 in the tutorial 
#   https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/


#   If we need to display both plots. We can use multiple renderPlots, or
#   use the gridExtra pakcage and the function grid.arrange. 
#   https://stackoverflow.com/questions/34384907/how-can-put-multiple-plots-side-by-side-in-shiny-r


# Refer to the lesson 2 in shiny app tutorial:
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
#img(src = "iris.png", height = 120, width = 360,"center"), 
# the height and width may vary in different screen, etc
div(img(src = "iris.png", height = 120, width = 360), style="text-align: center;")
#https://github.com/rstudio/shiny/issues/555
#HTML('<center><img src="iris.png" width="400"></center>'),




