#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(broom)
library(dplyr)
library(lubridate)

data <- read_csv("data.csv") %>%
    tidy()
data <- data %>%
    mutate(dateTime = ymd_hms(CreatedDate),
           Year = year(dateTime),
           Month = month(dateTime),
           Day = day(dateTime),
           Date = paste(Year, Month, Day, sep = "-") %>% ymd() %>% as.Date()) %>%
    select(-dateTime, -Day, -Month, -Year) %>%
    select(Date, everything())

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "tag",
                        label = "Select tags to view data:",
                        choices = c("Events", "PublicSector",    "Healthcare",      "Land",           
                                    "Legal" ,          "SpecialPurpose",  "MixedUse",        "Company",        
                                    "Investment",      "Hospitality",     "People",          "Finance",        
                                    "Analytics",       "CompaniesPeople", "Lease",           "Retail",         
                                    "Industrial",      "Development",     "Multifamily",     "Sale",           
                                    "Office",          "National") ,
                        selected = "Events"),
        
            checkboxGroupInput(inputId = "country",
                           label = "Select countries:",
                           choices = c("Canada", "US", "GB") ,
                           selected = "US"),
        
        
            sliderInput("TheDates",
                    "Dates:",
                    min = as.Date("2016-03-31","%Y-%m-%d"),
                    max = as.Date("2018-09-28","%Y-%m-%d"),
                    value=c(as.Date("2016-03-31"), as.Date("2017-03-31")),
                    timeFormat=("%Y-%m-%d")),
            
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )

)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
