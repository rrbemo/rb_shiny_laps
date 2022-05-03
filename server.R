library(tidyverse)
library(lubridate)
library(plotly)
library(jsonlite)

#all_laps <- read_csv("2022_australia.csv") 
#host_name <- "http://127.0.0.1:5000"
host_name <- "http://ryanbemowski.com"

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Build the UI
  output$ui_year_options <- renderUI({
    years <- fromJSON(paste0(host_name, "/fast_f1_years"))
    ui <- column(4, tags$h2("Year"),
                 radioGroupButtons("year_option",
                                   "Available Years",
                                   label = "Season",
                                   choices = years,
                                   direction = "vertical"))
    return(ui)
  })
  
  events <- reactive({
    req(input$year_option)
    events <- fromJSON(paste0(host_name, "/fast_f1_events/", input$year_option))
  })
  
  session_data <- reactive({
    req(input$session_option)
    session_data <- fromJSON(URLencode(paste0(host_name, "/fast_f1_laps/", input$year_option, "/", input$track_option, "/", input$session_option)))
  })
  
  output$ui_track_options <- renderUI({
    #data_url <- paste0(host_name, "/fast_f1_events/", input$year_options)
    #print(data_url)
    events <- events()
    ui <- column(4, tags$h2("Track"),
           radioGroupButtons("track_option", 
                             "Tracks for year",
                             label = "Grand Prix",
                             choices = events$EventName,
                             direction = "vertical"))
    return(ui)
  })
  
  output$ui_session_options <- renderUI({
    #data_url <- paste0(host_name, "/fast_f1_events/", input$year_options)
    #print(data_url)
    req(input$track_option)
    #sessions <- events() %>%
    #  filter(EventName == input$track_option,) %>%
    #  select(Session1, Session2, Session3, Session4, Session5) %>%
    #  gather("Session", "SessionName") %>%
    #  select(SessionName) %>%
    #  filter(SessionName != "None")
    ui <- column(4, tags$h2("Session"),
                 radioGroupButtons("session_option", 
                                   "Sessions in event",
                                   label = "Session",
                                   #choices = sessions$SessionName,
                                   choices = c("Race"),
                                   selected = "Race",
                                   direction = "vertical"))
    return(ui)
  })
  
  output$plotlyd1 <- renderPlotly({
    req(input$session_option)
    laps <- session_data()%>%
      mutate(PitInLap = ifelse(is.na(PitInTime),
                               FALSE,
                               TRUE)) %>%
      select(c(Driver, DriverNumber, LapNumber, Team, Sector1Time, Sector2Time, Sector3Time, LapStartTime, LapTime, TyreLife, Compound, PitInLap, TrackStatus)) %>%
      mutate(Sector1Time = ifelse(!is.na(Sector1Time),
                                  dmilliseconds(Sector1Time),
                                  NA),
             Sector2Time = ifelse(!is.na(Sector2Time),
                                  dmilliseconds(Sector2Time),
                                  NA),
             Sector3Time = ifelse(!is.na(Sector3Time),
                                  dmilliseconds(Sector3Time),
                                  NA),
             LapStartTime = ifelse(!is.na(LapStartTime),
                                   dmilliseconds(LapStartTime),
                                   NA),
             LapTime = ifelse(!is.na(LapTime),
                              dmilliseconds(LapTime),
                              NA),
             TyreLife = as.integer(as.character(TyreLife))) %>%
      mutate(LapEndTime = LapStartTime + LapTime)
    # Filter out slow and pit laps
    #  laps <- laps %>%
    #    filter(TrackStatus == "1")
    #    filter(LapTime <= min(laps$LapTime) * 1.2) #%>%
    #    filter(PitLap == FALSE)
    
    lap_end_times <- laps %>%
      group_by(LapNumber) %>%
      summarise(MinLapEndTime = min(LapEndTime, na.rm = TRUE))
    
    laps <- laps %>%
      left_join(lap_end_times, by = c("LapNumber")) %>%
      mutate(TimeToLeader = LapEndTime - MinLapEndTime)
    
    plot_ly(type = "scatter") %>%
      add_trace(name = ~Driver, data = laps, 
                x = ~LapNumber, y = ~TimeToLeader,
                legendgroup = ~Driver,
                mode = "lines",
                connectgaps = TRUE) %>%
      add_trace(name = ~Driver, 
                data = laps[laps$PitInLap == TRUE,],
                x = ~LapNumber, y = ~TimeToLeader,
                mode = "markers",
                legendgroup = ~Driver,
                marker = list(line = list(color = "black",
                                          width = 0.5))) %>%
      layout(yaxis = list(autorange = "reversed",
                          zeroline = FALSE),
             xaxis = list(zeroline = FALSE)) # Reverse the y axis so top car is ahead.
  })
  
  #observeEvent(events(), {
  #  updateGroupButtons(inputId = "track_options", choices = events()$EventName)
  #})
}

# TrackStatus values
#'1’: Track clear (beginning of session ot to indicate the end of another status)
#‘2’: Yellow flag (sectors are unknown)
#‘3’: ??? Never seen so far, does not exist?
#‘4’: Safety Car
#‘5’: Red Flag
#‘6’: Virtual Safety Car deployed
#‘7’: Virtual Safety Car ending (As indicated on the drivers steering wheel, on tv and so on; status ‘1’ will mark the actual end)