library(tidyverse)
library(lubridate)
library(plotly)

all_laps <- read_csv("/home/bemo/Documents/Git/fast_f1/2022_australia.csv") 

filter_laps <- all_laps %>%
  filter(Team == "Haas F1 Team")
  
laps <- filter_laps%>%
  mutate(PitLap = ifelse(is.na(PitInTime) & is.na(PitOutTime),
                         FALSE,
                         TRUE)) %>%
  select(c(Sector1Time, Sector2Time, Sector3Time, LapTime, TyreLife, Compound, PitLap)) %>%
  drop_na() %>%
  mutate(Sector1Time = seconds(hms(substr(Sector1Time, 8, 20))),
         Sector2Time = seconds(hms(substr(Sector2Time, 8, 20))),
         Sector3Time = seconds(hms(substr(Sector3Time, 8, 20))),
         LapTime = seconds(hms(substr(LapTime, 8, 22))),
         TyreLife = as.integer(as.character(TyreLife))) %>%
  filter(LapTime <= quantile(laps$LapTime, probs = 0.75),
         PitLap == FALSE)

medium_laps <- laps[laps$Compound == "MEDIUM",]
hard_laps <- laps[laps$Compound == "HARD",]
  

#plot(laps$Sector1Time, laps$TyreLife)
my_color <- c("red", "gold", "gray")
my_color <- setNames(my_color, c("SOFT", "MEDIUM", "HARD"))

plot_ly(laps, x = ~TyreLife, y = ~LapTime, color = ~Compound, colors = my_color) %>%
  layout(yaxis = list(range = c(0, max(laps$LapTime) * 1.02)))

plot_ly(alpha = 0.6, color = ~Compound, colors = my_color) %>%
  add_histogram(data = laps[laps$Compound == "MEDIUM",], x = ~LapTime) %>%
  add_histogram(data = laps[laps$Compound == "HARD",], x = ~LapTime) %>%
  layout(barmode = "overlay")
