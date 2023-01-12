library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(jsonlite)

#all_laps <- read_csv("2022_australia.csv") 
host_name <- "http://127.0.0.1:5000"
#host_name <- "http://127.0.0.1"

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Build the UI
  output$ui_year_options <- renderUI({
    years <- fromJSON(paste0(host_name, "/fast_f1_years")) %>%
      sort(decreasing = TRUE)
    ui <- pickerInput("year_option",
                      label = "Season Year",
                      choices = years,
                      options = list(
                        'live-search' = TRUE
                      ))
    return(ui)
  })
  output$ui_track_options <- renderUI({
    #data_url <- paste0(host_name, "/fast_f1_events/", input$year_options)
    #print(data_url)
    events <- events()
    ui <- pickerInput("track_option", 
                      label = "Grand Prix",
                      choices = events$EventName,
                      selected = NA,
                      options = list(
                       'live-search' = TRUE
                      ))
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
    ui <- pickerInput("session_option", 
                      label = "Session",
                      #choices = sessions$SessionName,
                      choices = c("Race"),
                      selected = "Race")
    return(ui)
  })
  output$ui_driver_one_options <-renderUI({
    req(input$session_option)
    ui <- pickerInput("driver_one_option", #"First Driver",
                      label = "Driver One",
                      selected = "16",
                      choices = unique(session_data()$DriverNumber))
  })
  output$ui_driver_one_offset <- renderUI({
    ui <- sliderInput("driver_one_offset", "Telemetry Offset",
                      min = -100,
                      max = 100,
                      value = 0)
  })
  output$ui_driver_two_options <-renderUI({
    req(input$driver_one_option)
    ui <- pickerInput("driver_two_option", #"Second Driver",
                      label = "Driver Two",
                      selected = "55",
                      choices = unique(session_data()$DriverNumber))
  })
  output$ui_driver_two_offset <- renderUI({
    ui <- sliderInput("driver_two_offset", "Telemetry Offset",
                      min = -100,
                      max = 100,
                      value = 0)
  })
  output$ui_lap_options <- renderUI({
    req(input$driver_one_option)
    ui <- pickerInput("lap_option",
                      label = "Lap",
                      selected = "5",
                      choices = session_data()$LapNumber)
  })
  events <- reactive({
    req(input$year_option)
    events <- fromJSON(paste0(host_name, "/fast_f1_events/", input$year_option))
  })
  session_data <- reactive({
    req(input$session_option)
    the_url <- URLencode(paste0(host_name, "/fast_f1_laps/", input$year_option, "/", input$track_option, "/", input$session_option))
    session_data <- fromJSON(the_url)
  })
  car_data <- reactive({
    req(input$session_option)
    the_url <- URLencode(paste0(host_name, "/fast_f1_car_data/", input$year_option, "/", input$track_option, "/", input$session_option))
    session_data <- fromJSON(the_url) %>%
      mutate(RowNumber = row_number())
  })
  pos_data <- reactive({
    req(input$session_option)
    the_url <- URLencode(paste0(host_name, "/fast_f1_pos_data/", input$year_option, "/", input$track_option, "/", input$session_option))
    session_data <- fromJSON(the_url)%>%
      mutate(RowNumber = row_number())
  })
  telemetry <- reactive({
    #req(input$driver_one)
    #req(input$driver_two)
    telemetry <- car_data() %>%
      group_by(CarNumber)
      
    ## My original attempt to combine car and position data
    # telemetry <- car_data() %>%
    #   full_join(pos_data(), by = c("CarNumber", "RowNumber")) %>%
    #   mutate(SessionTime_Pos = SessionTime.y,
    #          SessionTime_Car = SessionTime.x)
    # #%>%
     # filter(CarNumber %in% c(input$driver_one, input$driver_two))
  })
  
  output$plotly_time_to_leader <- renderPlotly({
    req(input$session_option)
    laps <- session_data() %>%
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
  output$plotly_car_data <- renderPlotly({
    req(input$session_option)
    req(input$driver_one_option)
    req(input$lap_option)
    
    car_number <- c(input$driver_one_option, input$driver_two_option) #, input$driver_two_option)
    lap_number <- input$lap_option
    print(input$session_option)
    print(car_number)
    print(lap_number)
    lap_df <- session_data() %>%
      filter(DriverNumber %in% car_number,
             LapNumber == lap_number)
    
    tel_df <- car_data() %>% #telemetry() %>%
      filter(CarNumber %in% car_number) %>% 
      select(c(CarNumber, SessionTime, Throttle, Brake, Speed)) %>%
      mutate(Brake = as.numeric(Brake) * 100)
    
    print(tel_df)
    
    df <- tel_df %>%
      filter((SessionTime >= lap_df$LapStartTime[lap_df$DriverNumber == car_number[1]] 
                & SessionTime < lap_df$Sector3SessionTime[lap_df$DriverNumber == car_number[1]]
                & CarNumber == car_number[1])
             | (SessionTime >= lap_df$LapStartTime[lap_df$DriverNumber == car_number[2]] 
                & SessionTime < lap_df$Sector3SessionTime[lap_df$DriverNumber == car_number[2]]
                & CarNumber == car_number[2])) %>%
      ## Compare by start lap time
     group_by(CarNumber) %>%
     mutate(TELEMETRY_NUMBER = row_number(),
            TELEMETRY_COUNT = n(),
            TELEMETRY_PROP = TELEMETRY_NUMBER/TELEMETRY_COUNT) %>%
     ungroup()
    
    #print(df)
    #%>%
     # filter(CarNumber == '20')

    plot_ly(data = df, type = "scatter") %>%
      add_trace(name = ~CarNumber, 
                x = ~TELEMETRY_PROP, y = ~Speed,
                mode = "lines",
                legendgroup = ~CarNumber,
                connectgaps = TRUE) %>%
      #add_trace(name = "Throttle", 
      #          x = ~SessionTime, y = ~Throttle,
      #          mode = "lines",
      #          legendgroup = ~CarNumber,
      #          connectgaps = TRUE) %>%
      #add_trace(name = "Brake", 
      #          x = ~SessionTime, y = ~Brake,
      #          mode = "lines",
      #          legendgroup = ~CarNumber,
      #          connectgaps = TRUE) %>%
      layout(yaxis = list(title = "Speed (kph)", 
                          zeroline = FALSE),
             xaxis = list(title = "Lap Progress",
                          tickformat = ".0%",
                          zeroline = FALSE)) # Reverse the y axis so top car is ahead.
  })
  output$plotly_pos_data <- renderPlotly({
    # TODO: Combine lap data with position data so we can see lap speed. 
    req(input$session_option)
    df <- pos_data() %>%
      select(c(CarNumber, SessionTime, X, Y, Z)) %>%
      filter(CarNumber == '20')
    
    plot_ly(data = df, type = "scatter") %>%
      add_trace(name = "Speed", 
                x = ~X, y = ~Y,
                mode = "markers",
                connectgaps = TRUE) %>%
      layout(yaxis = list(zeroline = FALSE),
             xaxis = list(zeroline = FALSE)) # Reverse the y axis so top car is ahead.
  })
  
  output$telemetry_table <- renderDT({
    telemetry()
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