#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load required libraries
library(shiny)
library(ggplot2)
# Define UI
ui <- fluidPage(
  titlePanel("Interactive Histogram with ggplot2"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Choose a Variable:",
                  choices = c("MPG" = "mpg", "Horsepower" = "hp",
                              "Weight" = "wt"),
                  selected = "mpg") # Default selection
    ),
    mainPanel(
      plotOutput("histPlot") # Interactive plot
    )
  )
)
# Define server logic
server <- function(input, output) {
  output$histPlot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$var)) + # Uses user-selected
      variable
    geom_histogram(fill = "blue", color = "black", bins = 10) +
      labs(title = paste("Histogram of", input$var), x = input$var, y =
             "Count")
  })
}
# Run the app
shinyApp(ui = ui, server = server)