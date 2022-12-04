# -------------------------------------------------#
#                 Week 12--Day24                   #
#   Shiny App Example 2 (Iris data exploration)    #   
#--------------------------------------------------#

library(tidyverse)
library(shiny)
#install.packages("gridExtra")
library(gridExtra)

#=====================#
# Part 23: Shiny Apps # 
#=====================#

# Tutorial: Display reactive output
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/

# Tutorial: Add control widgets
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/


### (23.3) Iris data shiny app ###
# Let's create a shinny app example that works on the iris data:
# (a) The user can choose one variable from the numerical 
#     variables in the iris data.
# (b) The app will show the histogram of this variable.
#     The user can choose how many bins are there in the plot.
# (c) The user can choose another variable, then app will display
#     the scatter plot between the two variables selected.
# (d) The user can check a box "Color by Species", to add the
#     species color to scatter plot.

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
  titlePanel("Iris Data Explorer"),
  # Input() functions
  # Output() functions
  
   sidebarLayout( 
     sidebarPanel(#input() functions"),
     div(img(src = "iris.png", height = 80, width = 220), style="text-align: center;"),
     selectInput(inputId = "var1",
                 label = "Choose first variable:",
                 choices = c("Sepal.Length", 
                             "Sepal.Width", 
                             "Petal.Length", 
                             "Petal.Width"),
                 selected = "Sepal.Length"),
     sliderInput(inputId = "bins",
                 label = "Choose the number of bins:",
                 min = 1, max = 55, value = 25),
     selectInput(inputId = "var2",
                 label = "Choose second variable:",
                 choices = c("Sepal.Length", 
                             "Sepal.Width", 
                             "Petal.Length", 
                             "Petal.Width"),
                 selected = "Sepal.Length"),
     checkboxInput(inputId = "checkbox1", label = "Color by Species:", value = FALSE)
     ),
     mainPanel(
       plotOutput(outputId = "hist"),
       plotOutput(outputId = "scat")
     )
   )
)


server <- function(input, output) {
  output$hist <- renderPlot(
    ggplot(data=iris)+
      geom_histogram(aes_string(x=input$var1, fill=(if (input$checkbox1) {"Species"})),bins=input$bins)+
      theme_bw()+
      labs(x="Sepal Legnth (cm)",
           title=paste("The histogram of" , input$var1 ,"in Iris Data"))+
      theme(plot.title = element_text(hjust = 0.5))
  )
  output$scat <- renderPlot(
    ggplot(iris) +
      geom_point(aes_string(x=input$var1,
                     y=input$var2, color=(if (input$checkbox1) {"Species"})))+
      labs(x=input$var1,
           y= input$var2,
           title=paste("Relationship of" , input$var1 ,"with", input$var2)) +
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


# Some helpful links:
# Options in the input functions and others, see 
#  -https://shiny.rstudio.com/reference/shiny/1.4.0/


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
# div(img(src = "iris.png", height = 120, width = 360), style="text-align: center;")
#https://github.com/rstudio/shiny/issues/555
#HTML('<center><img src="iris.png" width="400"></center>'),
