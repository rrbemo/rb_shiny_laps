library(tidyverse)
library(lubridate)
library(plotly)

# Define server logic required to draw a histogram
server <- function(input, output) {
  all_laps <- read_csv("/home/bemo/Documents/Git/fast_f1/2022_australia.csv") 

  laps <- all_laps%>%
    mutate(PitLap = ifelse(is.na(PitInTime) & is.na(PitOutTime),
                           FALSE,
                           TRUE)) %>%
    select(c(Driver, DriverNumber, LapNumber, Team, Sector1Time, Sector2Time, Sector3Time, LapTime, TyreLife, Compound, PitLap)) %>%
    drop_na() %>%
    mutate(Sector1Time = seconds(hms(substr(Sector1Time, 8, 20))),
           Sector2Time = seconds(hms(substr(Sector2Time, 8, 20))),
           Sector3Time = seconds(hms(substr(Sector3Time, 8, 20))),
           LapTime = seconds(hms(substr(LapTime, 8, 22))),
           TyreLife = as.integer(as.character(TyreLife))) %>%
    filter(LapTime <= quantile(laps$LapTime, probs = 0.75),
           PitLap == FALSE)
  
  #plot(laps$Sector1Time, laps$TyreLife)
  my_color <- c("red", "gold", "gray")
  my_color <- setNames(my_color, c("SOFT", "MEDIUM", "HARD"))
  
  output$plotlyd1 <- renderPlotly({
    d1_laps <- laps %>%
      filter(Driver == input$driver1)
    
    # generate bins based on input$bins from ui.R
    plot_ly(alpha = 0.6, color = ~Compound, colors = my_color) %>%
      add_histogram(data = d1_laps[d1_laps$Compound == "HARD",], x = ~LapTime) %>%
      add_histogram(alpha = 0.8, data = d1_laps[d1_laps$Compound == "MEDIUM",], x = ~LapTime) %>%
      layout(barmode = "overlay")
  })
}