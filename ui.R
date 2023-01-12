library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
#library(shinyjs)
#library(shinythemes)
library(plotly)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Ryan Bemowski's Shiny F1 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      uiOutput("ui_year_options"),     # Dynamic year inputs
      uiOutput("ui_track_options"),    # Dynamic track inputs
      uiOutput("ui_session_options"),  # Dynamic session options
      menuItem("Time To Leader", tabName = "timeToLeader", icon = icon("chart-line")),
      menuItem("Car Compare", tabName = "car_data", icon = icon("magnifying-glass-chart"))
      #menuItem("Car Position", tabName = "pos_data", icon = icon("magnifying-glass-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "timeToLeader",
              h2("Ryan Bemowski's F1 Time to leader"),
              fluidRow(
                # Application title
                box(width = 12,
                    plotlyOutput("plotly_time_to_leader") %>% withSpinner()
                )
              )
      ),
      tabItem(tabName = "car_data",
              h2("Ryan Bemowski's F1 car speed comparison per lap"),
              fluidRow(
                box(width = 12,
                    box(width = 5, 
                        uiOutput("ui_driver_one_options")),  #Car1 selection 
                    box(width = 5,
                        uiOutput("ui_driver_two_options")),  #Car2 selection,
                    box(width = 2,
                        uiOutput("ui_lap_options"))          #Lap number selection
                    )
              ),
              fluidRow(
                box(width = 12,
                    plotlyOutput("plotly_car_data") %>% withSpinner()
                )
              )#,
              #fluidRow(
              #  box(
              #    uiOutput("ui_driver_one"),
              #    uiOutput("ui_driver_one_offset")
              #  ),
              #  box(
              #    uiOutput("ui_driver_two"),
              #    uiOutput("ui_driver_two_offset") 
              #  )#,
                #box(width = 12,
                #    DTOutput('telemetry_table') 
                #)
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