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

textFreq <- data %>%
  select(Date, Title, StoryID) %>%
  unnest_tokens(word, Title) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

textFreq$word <- factor(textFreq$word, levels = textFreq$word[order(desc((textFreq$n)))])

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("tag", "Choose desired tags:",
                  list(`RealEstate` = list("Development", "Land", "Lease", "MixedUse", "Retail", "Sale", "SpecialPurpose"),
                       `Workforce` = list("Company", "CompaniesPeople", "Office"),
                       `Business` = list("Analytics", "Finance", "Investment"),
                       `PublicServices` = list("Healthcare", "Hospitality", "Multifamily", "People", "PublicSector"),
                       `Sectors/Misc` = list("Events", "Industrial", "Legal", "National")
                  ),
                  selected = list("Development", "Company", "Analytics", "Healthcare"),
                  multiple = TRUE),
      
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
      plotOutput("timeTagPlot")
    )
  )
  
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  output$plot <- renderPlot({
    
    textFreq %>%
      filter(n>= 75) %>%
      ggplot(aes(x = word, y = n)) +
      geom_col() +
      labs(x = "Words", y = "Frequency", title = "Most frequent words in Titles")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
