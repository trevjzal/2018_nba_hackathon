
library(tidyverse)
library(data.table)

#-------- LOADS ------------
event_codes <- fread("Basketball Analytics/NBA Hackathon - Event Codes.txt", sep = "\t")
game_lineup <- fread("Basketball Analytics/NBA Hackathon - Game Lineup Data Sample (50 Games).txt", sep = "\t")
pbp_sample <- fread("Basketball Analytics/NBA Hackathon - Play by Play Data Sample (50 Games).txt", sep = "\t")



pbp_sample[Event_Msg_Type == 8]


# Let's first try and just build a timeline of players in lineups
pbp_lineups <- merge(game_lineup, pbp_sample,
                     by = c("Game_id", "Period"),
                     all.x = T,
                     suffixes = c("_Lineup", "_PBP"),
                     allow.cartesian = T) %>%
  # If a substitution occurs, swap Person_id's to Person2
  .[Event_Msg_Type == 8 & Person_id == Person1, Person_id := Person2]




pbp2 <- copy(pbp_sample)
pbp2[, Period_Event_Num := seq(.I) - 1, by = c("Game_id", "Period")]

hist(pbp_sample)
