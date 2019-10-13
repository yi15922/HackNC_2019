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
library(DT)

data <- read_csv("data.csv") %>%
    as.tibble()

data <- data %>%
    mutate(dateTime = ymd_hms(CreatedDate),
           Year = year(dateTime),
           Month = month(dateTime),
           Day = day(dateTime),
           Date = paste(Year, Month, Day, sep = "-") %>% ymd() %>% as.Date()) %>%
    select(-dateTime, -Day, -Month, -Year) %>%
    select(Date, everything()) %>%
    mutate(Title = as.character(Title), 
       Summary = as.character(Summary),
       Body = as.character(Body))

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
           dataTableOutput("table"),
           plotOutput("timeTagPlot")
        )
    )

)
# Define server logic required to draw a histogram
server <- function(input, output) {

        output$timeTagPlot <- renderPlot({
            sel_data <-
                data %>%
                    filter(
                        between(Date, input$TheDates[1], input$TheDates[2])
                )
       
            # output$table <- renderDataTable({
            #     datatable(sel_data)
            #     })
            
            output$timeTagPlot <- renderPlot({
                ggplot(data = sel_data, mapping = aes(x = Tag_National, y = Hits)) +
                    geom_col();


            })

    
    #ggplot(tagPop[month(tags) == month(TheDates)], mapping = aes(x = tags))
    
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
