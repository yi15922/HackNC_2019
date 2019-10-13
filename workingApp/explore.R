# home page

output$pageStub <- renderUI(
  #includeMarkdown("Data-exploration.Rmd")
  htmlTemplate("./www/Data-exploration.html")
)
