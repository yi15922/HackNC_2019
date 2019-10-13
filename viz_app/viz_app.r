library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("INSERT BETTER TITLE HERE!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
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
      
      #stored in input
      
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
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
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

shinyApp(ui = ui, server = server)
