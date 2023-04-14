library(shiny)
library(hypothesis)

ui <- fluidPage(
  useHypothesis(),
  hypothesisOnOff(TRUE),
  br(),
  "Hello There General Kenobi"
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
