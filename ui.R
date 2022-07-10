library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
#library(shinyjs)
#library(shinythemes)
library(plotly)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "F1 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      uiOutput("ui_year_options"),
      uiOutput("ui_track_options"),
      uiOutput("ui_session_options"),
      menuItem("Time To Leader", tabName = "timeToLeader", icon = icon("chart-line")),
      menuItem("Car Data", tabName = "car_data", icon = icon("magnifying-glass-chart")),
      menuItem("Car Position", tabName = "pos_data", icon = icon("magnifying-glass-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "timeToLeader",
              h2("Time to leader"),
              fluidRow(
                # Application title
                box(width = 12,
                    plotlyOutput("plotly_time_to_leader") %>% withSpinner()
                )
              )
      ),
      tabItem(tabName = "car_data",
              h2("Car Data Analysis"),
              fluidRow(
                box(width = 12,
                    plotlyOutput("plotly_car_data") %>% withSpinner()
                )
              )#,
              #fluidRow(
              #  box(width = 12,
              #      uiOutput("ui_driver_to_offset"),
              #      uiOutput("ui_driver_offset")
              #  )
              #)
      ),
      tabItem(tabName = "pos_data",
              h2("Car Position Analysis"),
              fluidRow(
                box(width = 12,
                    plotlyOutput("plotly_pos_data") %>% withSpinner()
                )
              )
      )
    )
  )
)