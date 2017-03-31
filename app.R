library(shiny)
library(shinyWidgets)

if (interactive()) {

  ui <- fluidPage(
    awesomeRadio(inputId = "somevalue", label = "", choices = c("A", "B", "C")),
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    output$value <- renderText({ input$somevalue })
  }
  shinyApp(ui, server)
}
