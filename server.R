library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(colorspace)
library(jsonlite)

#all_laps <- read_csv("2022_australia.csv") 
#host_name <- "http://127.0.0.1:5000"
host_name <- "http://127.0.0.1"

# Colors were set to reflect TV broadcast colors of 2021 season: 
# https://www.reddit.com/r/formula1/comments/lfpyfp/f1_2021_team_colors_hex_codes/
team_color <- c("Ferrari" = "#DC0000",
                "Mercedes" = "#00D2BE",
                "Haas F1 Team" = "#CCCCCC",
                "Alfa Romeo" = "#900000",
                "Alpine" = "#0090FF",
                "AlphaTauri" = "#2B4562",
                "Aston Martin" = "#006F62",
                "Williams" = "#005AFF",
                "McLaren" = "#FF8700",
                "Red Bull Racing" = "#0600EF")
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Dynamic UI elements
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
                      label = "Car One",
                      selected = "16",
                      choices = unique(session_data()$DriverNumber))
  })
  output$ui_lap_one_options <- renderUI({
    req(input$driver_one_option)
    ui <- pickerInput("lap_one_option",
                      label = "Lap",
                      selected = "5",
                      choices = order(unique(session_data()$LapNumber)))
  })
  output$ui_driver_two_options <-renderUI({
    req(input$driver_one_option)
    ui <- pickerInput("driver_two_option", #"Second Driver",
                      label = "Car Two",
                      selected = "55",
                      choices = unique(session_data()$DriverNumber))
  })
  output$ui_lap_two_options <- renderUI({
    req(input$driver_one_option)
    #req(input$synclap)
    ui <- pickerInput("lap_two_option",
                      label = "Lap",
                      selected = "5",
                      choices = order(unique(session_data()$LapNumber)))
  })
  output$ui_car_lap_compare <- renderUI({
    req(car_lap_data_one)
    req(car_lap_data_two)
    req(car_lap_details_data_one)
    req(car_lap_details_data_two)
    
    row_size <- c("vs." = "h1",
                  "Compound (laps)" = "p",
                  "Lap" = "h2",
                  "Lap Time" = "h3",
                  "Sector 1" = "p",
                  "Sector 2" = "p",
                  "Sector 3" = "p",
                  "Top Speed (kph)" = "h3",
                  "Avg. Speed (kph)" = "h3",
                  "Low Speed (kph)" = "p")
    car1 <- car_lap_ui_data(car_lap_details_data_one(), car_lap_data_one())
    car2 <- car_lap_ui_data(car_lap_details_data_two(), car_lap_data_two())
    ui <- NULL
    for (name in names(row_size)) {
      ui_row <- fluidRow(width = 12,
                         column(width = 4,
                                style = "text-align:right;",
                                do.call(row_size[name], list(car1[name]))),
                         column(width = 4,
                                style = "text-align:center;",
                                do.call(row_size[name], list(name))),
                         column(width = 4,
                                style = "text-align:left;",
                                do.call(row_size[name], list(car2[name])))
                         )
      if (is.null(ui)) {
        ui <- ui_row
      } else {
        ui <- paste(ui, ui_row)
      }
    }
    tagList(HTML(ui))
  })
  
  ## Helper functions for Car/lap details data UI
  car_lap_ui_data <- function(car_lap_details, car_lap_tel) {
    c("vs." = paste(car_lap_details$Team, "-",
                    car_lap_details$DriverNumber,
                    car_lap_details$Driver),
      "Compound (laps)" = paste0(car_lap_details$Compound, "(", 
                                car_lap_details$TyreLife, ")"),
      "Lap" = car_lap_details$LapNumber,
      "Lap Time" = format_time(car_lap_details$LapTime),
      "Sector 1" = format_time(car_lap_details$Sector1Time),
      "Sector 2" = format_time(car_lap_details$Sector2Time),
      "Sector 3" = format_time(car_lap_details$Sector3Time),
      "Top Speed (kph)" = max(car_lap_tel$Speed),
      "Avg. Speed (kph)" = round(mean(car_lap_tel$Speed), 1),
      "Low Speed (kph)" = min(car_lap_tel$Speed)
    )
  }
  format_time <- function(ftime) {
    sprintf("%02d:%02.3f",
            minute(as.period(as.duration(milliseconds(ftime)))),
            second(as.period(as.duration(milliseconds(ftime)))))
  }
  
  ## Reactive functions
  events <- reactive({
    req(input$year_option)
    events <- fromJSON(paste0(host_name, "/fast_f1_events/", input$year_option))
  })
  session_data <- reactive({
    req(input$session_option)
    the_url <- URLencode(paste0(host_name, "/fast_f1_laps/", input$year_option, "/", input$track_option, "/", input$session_option))
    session_data <- fromJSON(the_url)
    #print(session_data)
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
  car_lap_data_one <- reactive({
    req(session_data)
    req(input$driver_one_option)
    req(input$lap_one_option)
    
    df <- car_lap_data(input$driver_one_option, 
                       input$lap_one_option) %>%
      mutate(group_name = paste0("Car One: ", input$driver_one_option, "(lap ", input$lap_one_option, ")"))
  })
  car_lap_details_data_one <- reactive({
    req(session_data)
    req(input$driver_one_option)
    req(input$lap_one_option)
    
    df <- car_lap_details_data(input$driver_one_option, 
                               input$lap_one_option)
  })
  car_lap_data_two <- reactive({
    req(session_data)
    req(input$driver_two_option)
    req(input$lap_two_option)
    
    df <- car_lap_data(input$driver_two_option, 
                       input$lap_two_option) %>%
      mutate(group_name = paste0("Car Two: ", input$driver_two_option, "(lap ", input$lap_two_option, ")"))
  })
  car_lap_details_data_two <- reactive({
    req(session_data)
    req(input$driver_two_option)
    req(input$lap_two_option)
    
    df <- car_lap_details_data(input$driver_two_option, 
                               input$lap_two_option)
  })
  ## Helper functions for reactive driver_lap functions
  car_lap_details_data <- function(car_num, lap_num) {
    lap_df <- session_data() %>%
      filter((DriverNumber == car_num &
                LapNumber == lap_num))
  }
  car_lap_data <- function(car_num, lap_num) {
    lap_df <- car_lap_details_data(car_num, lap_num)
    
    tel_df <- car_data() %>% #telemetry() %>%
      filter(CarNumber %in% car_num) %>% 
      select(c(CarNumber, SessionTime, Throttle, Brake, Speed)) %>%
      mutate(Brake = as.numeric(Brake) * 100)
    
    df <- tel_df %>%
      filter(SessionTime >= lap_df$LapStartTime[lap_df$DriverNumber == car_num]
             & SessionTime < lap_df$Sector3SessionTime[lap_df$DriverNumber == car_num]
             & CarNumber == car_num) %>%
      mutate(TELEMETRY_NUMBER = row_number(),
             TELEMETRY_COUNT = n(),
             TELEMETRY_PROP = TELEMETRY_NUMBER/TELEMETRY_COUNT)
  }
  
  output$plotly_time_to_leader <- renderPlotly({
    req(input$session_option)
    laps <- session_data() %>%
      mutate(PitInLap = ifelse(is.na(PitInTime),
                               FALSE,
                               TRUE)) %>%
      select(c(Driver, DriverNumber, LapNumber, Team, Sector1Time, Sector2Time, Sector3Time, LapStartTime, LapTime, TyreLife, Compound, PitInLap, TrackStatus)) %>%
      mutate(TraceColor = team_color[Team],
             TeamDriver = paste0(Team, ": ", Driver),
             Sector1Time = ifelse(!is.na(Sector1Time),
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
    
    #print(unique(laps[, c("TraceColor", "Team")]))
    
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
                color = ~I(TraceColor),
                text = ~TeamDriver,
                # hoverinfo = "text",
                hovertemplate = paste(
                  "%{text}",
                  "<extra>Lap %{x:,}</extra>"
                ),
                connectgaps = TRUE) %>%
      add_trace(name = ~Driver, 
                data = laps[laps$PitInLap == TRUE,],
                x = ~LapNumber, y = ~TimeToLeader,
                mode = "markers",
                color = ~I(TraceColor),
                legendgroup = ~Driver,
                marker = list(line = list(color = "black",
                                          width = 0.5))) %>%
      layout(yaxis = list(autorange = "reversed",
                          zeroline = FALSE),
             xaxis = list(zeroline = FALSE)) # Reverse the y axis so top car is ahead.
  })
  output$plotly_car_data <- renderPlotly({
    # req(input$session_option)
    # req(input$driver_one_option)
    # req(input$lap_one_option)
    
    # car_number <- c(input$driver_one_option, input$driver_two_option) #, input$driver_two_option)
    # lap_number <- c(input$lap_one_option, input$lap_two_option)
    # 
    # print(input$session_option)
    # print(car_number)
    # print(lap_number)
    # lap_df_one <- session_data() %>%
    #   filter((DriverNumber == car_number[1] &
    #            LapNumber == lap_number[1]))
    # lap_df_two <- session_data() %>%
    #   filter((DriverNumber == car_number[2] &
    #            LapNumber == lap_number[2]))
    # 
    # tel_df <- car_data() %>% #telemetry() %>%
    #   filter(CarNumber %in% car_number) %>% 
    #   select(c(CarNumber, SessionTime, Throttle, Brake, Speed)) %>%
    #   mutate(Brake = as.numeric(Brake) * 100)
    
    #print(tel_df)
    
    ## TODO: Need to find a way to store lap number so I can show the same care and different laps.
    # df_one <- tel_df %>%
    #   filter((SessionTime >= lap_df_one$LapStartTime[lap_df_one$DriverNumber == car_number[1]] 
    #             & SessionTime < lap_df_one$Sector3SessionTime[lap_df_one$DriverNumber == car_number[1]]
    #             & CarNumber == car_number[1])) %>%
    #   mutate(TELEMETRY_NUMBER = row_number(),
    #          TELEMETRY_COUNT = n(),
    #          TELEMETRY_PROP = TELEMETRY_NUMBER/TELEMETRY_COUNT,
    #          group_name = paste0("Driver:", car_number[1], " - Lap:", lap_number[1]))
    
    # df_two <- tel_df %>%
    #   filter((SessionTime >= lap_df_two$LapStartTime[lap_df_two$DriverNumber == car_number[2]] 
    #             & SessionTime < lap_df_two$Sector3SessionTime[lap_df_two$DriverNumber == car_number[2]]
    #             & CarNumber == car_number[2])) %>%
    #   mutate(TELEMETRY_NUMBER = row_number(),
    #          TELEMETRY_COUNT = n(),
    #          TELEMETRY_PROP = TELEMETRY_NUMBER/TELEMETRY_COUNT,
    #          group_name = paste0("Driver:", car_number[2], " - Lap:", lap_number[2]))
    req(car_lap_data_one)
    req(car_lap_data_two)
    req(car_lap_details_data_one)
    req(car_lap_details_data_two)
    
    car_one <- car_lap_data_one() %>%
      left_join(car_lap_details_data_one(), by = c("CarNumber" = "DriverNumber")) %>%
      mutate(TraceColor = team_color[Team])
    
    car_two <- car_lap_data_two() %>%
      left_join(car_lap_details_data_two(), by = c("CarNumber" = "DriverNumber")) %>%
      mutate(TraceColor = lighten(team_color[Team], 0.50))
                        
    df <- car_one %>%
      rbind(car_two) %>% 
      mutate(TeamDriver = paste0(Team, ": ", Driver))
    
    plot_ly(data = df, type = "scatter") %>%
      add_trace(name = ~group_name, 
                x = ~TELEMETRY_PROP, y = ~Speed,
                mode = "lines",
                legendgroup = ~group_name,
                color = ~I(TraceColor),
                text = ~TeamDriver,
                hovertemplate = paste(
                  "%{text}",
                  "<extra>speed %{y:,}</extra>"
                ),
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