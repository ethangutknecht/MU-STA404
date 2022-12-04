# -------------------------------------------------#
#                   Project 02                     #
#           STA404 - Ethan Gutknecht               #   
#--------------------------------------------------#
library(maps)
library(tidyverse)
library(shiny)
library(gridExtra)
library(readxl)
library(tidyr)
library(gapminder)

# Load Data from Other R File # -----------------------------------------------
setwd("C:/Users/Gutkn/Documents/STA404/Project02")
load("HighSchoolCompletion.RData")
load("MedianAnnualEarnings.RData")

# Create Shiny App # ----------------------------------------------------------
ui <- fluidPage(
  # Application title
  titlePanel(title = "An Interactive App For United States Education Statistics"),
  sidebarLayout(
    
    # Display controls/UI
    sidebarPanel(
      h4(strong("Education Plot Settings:")),
      # put your input controls here!
      selectInput(inputId="state", label="Region", selected="Ohio ",
                  choices=unique(highSchoolCompletion$State)),
      checkboxInput(inputId="levelLabel", 
                    label = "Bachelor's Degree or higher", 
                    value = FALSE),
      h4(strong("_______________________")),
      br(),
      h4(strong("Salary Plot Settings:")),
      selectInput(inputId="year", label="Year", selected="2018",
                  choices=unique(medianAnualEarnings$Year)),
      selectInput(inputId="sex", label="Sex", 
                  choices=unique(medianAnualEarnings$Sex))
    ),
    
    # Display Plots
    mainPanel(
      tabsetPanel(
        tabPanel("Education Plot", plotOutput(outputId="educationPlot")),
        tabPanel("Salary Plot", plotOutput(outputId="salaryPlot"))
      )
    )
  )
)


# Create Server # -------------------------------------------------------------
server <- function(input, output) {
  output$educationPlot <- renderPlot({
    load("HighSchoolCompletion.RData")
    
    highSchoolCompletion2 <-highSchoolCompletion %>%
      filter(State==input$state) %>% 
      filter(Level == (if (input$levelLabel) {"Percent with bachelor's or higher degree"}
                       else {"Percent with high school completion or higher"}))
    
    ggplot(data=highSchoolCompletion2)+
      geom_histogram(aes(x=Race, y=Percentage),
                     fill=(if (input$levelLabel) {"orange"}
                            else {"darkgreen"}),
                     stat = "identity") +
      labs(x="Race Of Population", 
           y="Population Percentage", 
           title= paste0("Percent Of Races That Completed A ",
                         (if (input$levelLabel) {"Bachelor's"}
                          else {"High School"}),
                        " Degree In ", input$state)) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text = element_text(size=14)) + 
      theme(axis.title = element_text(size=16)) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) 
  })
  output$salaryPlot <- renderPlot({
    load("MedianAnnualEarnings.RData")
    
    medianAnualEarnings2 <-medianAnualEarnings %>%
      filter(Sex==input$sex) %>% 
      filter(Year == input$year)
    
    ggplot(data=medianAnualEarnings2)+
      geom_histogram(aes(x=EducationLevel, y=Salary),
                     fill=(if (input$sex == "Male") {"lightblue"}
                           else {"lightpink"}),
                     stat = "identity") +
      labs(x="Education Level", 
           y="Salary (USD)", 
           title=paste0("Salaries Of ",
                        input$sex,
                        "s Across Different Education Levels In ",
                        input$year)) + 
      theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
      theme(axis.text = element_text(size=14)) + 
      theme(axis.title = element_text(size=16)) + 
      scale_y_continuous(limits = c(0, 150000))
    
  })
}


# Run the application # -------------------------------------------------------
shinyApp(ui = ui, server = server)

