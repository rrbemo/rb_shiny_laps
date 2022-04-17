library(tidyverse)
library(lubridate)
library(plotly)

all_laps <- read_csv("2022_australia.csv") 

# Define server logic required to draw a histogram
server <- function(input, output) {

  laps <- all_laps%>%
    mutate(PitInLap = ifelse(is.na(PitInTime),
                           FALSE,
                           TRUE)) %>%
    select(c(Driver, DriverNumber, LapNumber, Team, Sector1Time, Sector2Time, Sector3Time, LapStartTime, LapTime, TyreLife, Compound, PitInLap, TrackStatus)) %>%
    mutate(Sector1Time = ifelse(!is.na(Sector1Time),
                                seconds(hms(substr(Sector1Time, 8, 20))),
                                NA),
           Sector2Time = ifelse(!is.na(Sector2Time),
                                seconds(hms(substr(Sector2Time, 8, 20))),
                                NA),
           Sector3Time = ifelse(!is.na(Sector3Time),
                                seconds(hms(substr(Sector3Time, 8, 20))),
                                NA),
           LapStartTime = ifelse(!is.na(LapStartTime),
                                 seconds(hms(substr(LapStartTime, 8, 20))),
                                 NA),
           LapTime = ifelse(!is.na(LapTime),
                            seconds(hms(substr(LapTime, 8, 20))),
                            NA),
           TyreLife = as.integer(as.character(TyreLife)))
  # Filter out slow and pit laps
#  laps <- laps %>%
#    filter(TrackStatus == "1")
#    filter(LapTime <= min(laps$LapTime) * 1.2) #%>%
#    filter(PitLap == FALSE)
  
  lap_start_times <- laps %>%
    group_by(LapNumber) %>%
    summarise(FirstLapStartTime = min(LapStartTime, na.rm = TRUE))
  
  laps <- laps %>%
    left_join(lap_start_times, by = c("LapNumber")) %>%
    mutate(TimeToLeader = LapStartTime - FirstLapStartTime)
  
  #plot(laps$Sector1Time, laps$TyreLife)
  my_color <- c("red", "gold", "gray")
  my_color <- setNames(my_color, c("SOFT", "MEDIUM", "HARD"))
  
  output$plotlyd1 <- renderPlotly({
    d1_laps <- laps %>%
      filter(Driver == input$driver1)
    d2_laps <- laps %>%
      filter(Driver == input$driver2)
    
    plot_ly(type = "scatter") %>%
      add_trace(name = ~Driver, data = laps, 
                x = ~LapNumber, y = ~TimeToLeader,
                legendgroup = ~Driver,
                mode = "lines") %>%
      add_trace(name = ~Driver, data = laps[laps$PitInLap == TRUE,],
                x = ~LapNumber, y = ~TimeToLeader,
                legendgroup = ~Driver,
                marker = list(line = list(color = "black",
                                          width = 0.5))) %>%
      layout(yaxis = list(autorange = "reversed",
                          zeroline = FALSE),
             xaxis = list(zeroline = FALSE)) # Reverse the y axis so top car is ahead.
    
    # Show all drivers
    # Showing only 2 drivers
    #plot_ly(type = "scatter") %>%
    #  add_trace(name = ~Driver, data = d1_laps, 
    #            x = ~LapNumber, y = ~TimeToLeader, 
    #            mode = "lines") %>% 
    #  add_trace(name = ~Driver, data = d2_laps, 
    #            x = ~LapNumber, y = ~TimeToLeader, 
    #            mode = "lines") %>%
    #  add_trace(name = "Pit", data = d1_laps[d1_laps$PitInLap == TRUE,],
    #            x = ~LapNumber, y = ~TimeToLeader,
    #            marker = list(color = "black")) %>%
    #  add_trace(name = "Pit", data = d2_laps[d2_laps$PitInLap == TRUE,],
    #            x = ~LapNumber, y = ~TimeToLeader,
    #            marker = list(color = "black")) %>%
    #  layout(yaxis = list(autorange = "reversed")) # Reverse the y axis so top car is ahead.
      #layout(shapes = list(
      #  list(type = "rect",
      #       fillcolor = "yellow", line = list(color = "yellow"), opacity = 0.8,
      #       x0 = c(1), x1 = c(2), xref = "x", 
      #       y0 = 80, y1 = 150, yref = "y")))
#                line = list(dash = "dash")) 
    # generate bins based on input$bins from ui.R
#    plot_ly(alpha = 0.6, color = ~Compound, colors = my_color, xbins = list(size = 1)) %>%
#      add_histogram(alpha = 0.6, data = d1_laps[d1_laps$Compound == "HARD",], x = ~LapTime) %>%
#      add_histogram(alpha = 0.6, data = d1_laps[d1_laps$Compound == "MEDIUM",], x = ~LapTime) %>%
#      add_histogram(alpha = 0.6, data = d2_laps[d2_laps$Compound == "HARD",], x = ~LapTime) %>%
#      add_histogram(alpha = 0.6, data = d2_laps[d2_laps$Compound == "MEDIUM",], x = ~LapTime) %>%
#      layout(barmode = "overlay")
  })
}

# TrackStatus values
#'1’: Track clear (beginning of session ot to indicate the end of another status)
#‘2’: Yellow flag (sectors are unknown)
#‘3’: ??? Never seen so far, does not exist?
#‘4’: Safety Car
#‘5’: Red Flag
#‘6’: Virtual Safety Car deployed
#‘7’: Virtual Safety Car ending (As indicated on the drivers steering wheel, on tv and so on; status ‘1’ will mark the actual end)