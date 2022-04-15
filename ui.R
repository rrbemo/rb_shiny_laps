library(shiny)
library(shinyWidgets)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    # Application title
    titlePanel("Lap times on tires per driver"),
    column(6,
           tags$h2("Driver 1"),
           radioGroupButtons("driver1",
                             label = "Driver 1",
                             choices = c("VER", "RIC", "NOR", "MAG", "MSC", "LEC"),
                             direction = "vertical"),
           plotlyOutput("plotlyd1")),
    column(6,
           )
  )
)