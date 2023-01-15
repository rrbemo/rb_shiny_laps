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
              p("The time to leader plot shows relative position of each driver to the leader.
              The further down the driver's line falls, the further behind the leader they 
              are becoming. When lines cross, a position was exchanged. Pit stops, or in/out
              laps are also recorded here."),
              fluidRow(
                # Application title
                box(width = 12,
                    plotlyOutput("plotly_time_to_leader") %>% withSpinner()
                )
              )
      ),
      tabItem(tabName = "car_data",
              h2("Ryan Bemowski's F1 car speed comparison per lap"),
              p("The Car Speed Comparison plot shows the difference between two car's speed
                throughout a lap, or different laps. This can be especially helpful when 
                comparing the same driver to themselves on different laps or comparing 
                similarly quick drivers to eachother."),
              p("To keep the data as comparable as possible, the speed is plotted as a 
                proportion of the lap completed. This will help line up apexes, but will 
                distort how quickly one lap may have been completed compared to the other."),
              p("Note: Each time a new session is loaded, the telemetry data for all drivers 
                and laps must be loaded. This process can take a bit longer than expected. After 
                the initial session is loaded new driver and lap selections should load quickly."),
              fluidRow(
                box(width = 12,
                    column(width = 4,
                           uiOutput("ui_driver_one_options")),
                    column(width = 1,
                           uiOutput("ui_lap_one_options")),
                    column(width = 1,
                           checkboxInput("synclap", "Sync Lap?", value = TRUE)),
                    column(width = 4,
                           uiOutput("ui_driver_two_options")),
                    column(width = 2,
                        uiOutput("ui_lap_two_options"))          #Lap number selection
                )
              ),
              fluidRow(
                box(width = 12,
                    plotlyOutput("plotly_car_data") %>% withSpinner()
                )
              ),
              fluidRow(
                box(width = 12,
                    uiOutput("ui_car_lap_compare")
                    # column(width = 5,
                    #        style = "text-align:left;",
                    #        uiOutput("ui_driver_one_info")),
                    # column(width = 2,
                    #        style = "text-align:center;",
                    #        h1("Driver"),
                    #        p("Compound(laps)"),
                    #        h2("Lap"),
                    #        h3("Lap Time"),
                    #        p("Sector 1"),
                    #        p("Sector 2"),
                    #        p("Sector 3"),
                    #        h3("Top Speed"),
                    #        h3("Average Speed"),
                    #        p("Min Speed")),
                    # column(width = 5,
                    #        style = "text-align:right;",
                    #        uiOutput("ui_driver_two_info"))
                    )
              )
              #,
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