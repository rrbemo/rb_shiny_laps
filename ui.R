library(shiny)
library(shinycssloaders)
#library(shinydashboard)
library(shinyWidgets)
#library(shinyjs)
#library(shinythemes)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    # Application title
    titlePanel("F1 Lap Analysis"),
    fluidRow(fluidRow(plotlyOutput("plotlyd1") %>% withSpinner())),
    fluidRow(
      uiOutput("ui_year_options"),
      uiOutput("ui_track_options"),
      uiOutput("ui_session_options")
    )
  )
)