library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    # Application title
    titlePanel("Lap times on tires per driver"),
    fluidRow(
      column(6, tags$h2("Year"),
             radioGroupButtons("driver1",
                               label = "Season",
                               choices = c("2022"),
                               direction = "vertical")),
      column(6, tags$h2("Track"),
             radioGroupButtons("driver2",
                               label = "Grand Prix",
                               choices = c("Australia"),
                               direction = "vertical")
             )
      )
  ),
  fluidRow(plotlyOutput("plotlyd1") %>% withSpinner())
)