
library(tidyverse)
library(data.table)

#-------- LOADS ------------
event_codes <- fread("Basketball Analytics/NBA Hackathon - Event Codes.txt", sep = "\t")
game_lineup <- fread("Basketball Analytics/NBA Hackathon - Game Lineup Data Sample (50 Games).txt", sep = "\t")
pbp_sample <- fread("Basketball Analytics/NBA Hackathon - Play by Play Data Sample (50 Games).txt", sep = "\t")



#' Let's first try and just build a timeline of players in lineups. 
#' I'd like to include a possession count if possible, so also including rebounds and turnovers
#' in addition to made shots, free throws, and substitutions.
pbp_lineups <- pbp_sample[Event_Msg_Type %in% c(1, 3, 4, 5, 8)] %>%
  merge(game_lineup, by = c("Game_id", "Period"),
        all.x = T, allow.cartesian = T, 
        suffixes = c("_Lineup", "_PBP")) %>% 
  merge(event_codes, by = c("Event_Msg_Type", "Action_Type"), all.x = T) %>% 
  # If a substitution occurs, swap Person_id's to Person2
  .[Event_Msg_Type == 8 & Person_id == Person1, Person_id := Person2] %>% 
  # Collapse the player ID's into a list by Team_Lineup
  .[, .(Persons_in_Team_id_Lineup = paste(Person_id, collapse = ", ")),
    .(Game_id, PC_Time, Period, Team_id_Lineup, Team_id_PBP, Event_Msg_Type, Action_Type, Option1)]



# Explore - calculate +/- for a single game/period
kGame <- pbp_sample$Game_id[1]
players_start <- game_lineup[Game_id == kGame & Period == 1]
  
ex_game_period <- pbp_sample[Game_id == kGame & Period == 1] %>% 
  .[Event_Msg_Type %in% c(1, 3, 4, 5, 8)]


players <- unique(game_period$Person1)
stint <- data.table(matrix(nrow = 0, ncol = length(players)))
names(stint) <- players



ex_game_period[Event_Msg_Type ==8, N_teams_per_player_subst := .N, .(Person1, Team_id)]





event_codes %>% View


pbp2 <- copy(pbp_sample)
pbp2[, Period_Event_Num := seq(.I) - 1, by = c("Game_id", "Period")]


merge(pbp_sample, event_codes, 
      by = c("Event_Msg_Type", "Action_Type"),
      all.x = T) %>% 
  .[, lapply(.SD, sum),
    .SDcols = c("Option1", "Option2", "Option3"),
    .(Event_Msg_Type, Event_Msg_Type_Description, Action_Type_Description)] %>% 
  View

