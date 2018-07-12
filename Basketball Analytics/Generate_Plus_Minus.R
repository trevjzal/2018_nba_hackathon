library(tidyverse)
library(data.table)

#-------- LOADS ------------
event_codes <- fread("Basketball Analytics/NBA Hackathon - Event Codes.txt", sep = "\t")
game_lineup <- fread("Basketball Analytics/NBA Hackathon - Game Lineup Data Sample (50 Games).txt", sep = "\t")
pbp_sample <- fread("Basketball Analytics/NBA Hackathon - Play by Play Data Sample (50 Games).txt", sep = "\t")

#--------- Initialize player matrix -------------
players <- unique(c(pbp_sample$Person1, pbp_sample$Person2))
gameperiods <- unique(game_lineup[, .(Game_id, Period)])

pmat_empty <- data.table(matrix(0, nrow = nrow(gameperiods), ncol = length(players)))
names(pmat_empty) <- players
pmat_empty <- cbind(gameperiods, pmat_empty)


#' @title CalculatePlusMinus
#' @description populates the player matrix for a specific game_id|period
#' @param pmat_row A single row of the initialized player matrix
#' @return The populated row of the player matrix
CalculatePlusMinus <- function(pmat_row){
  pmat_row_out <- copy(pmat_row)
  # Initialize active players
  active_players <- game_lineup[Game_id == pmat_row$Game_id & Period == pmat_row$Period, .(Team_id, Person_id)]
  
  game_period <- pbp_sample[Game_id == pmat_row$Game_id & Period == pmat_row$Period] %>% 
    .[Event_Msg_Type %in% c(1, 3, 
                            # 4, 5, # Rebound & Turnover, can be used to calculate possessions
                            6, # Keep fouls, use it to track the stint at the time of foul
                            8)]
  
  # Loop through pbp data to calculate +/-
  for(i in seq(nrow(game_period))){
    # If a foul occurs, save the lineup for future iterations
    if(game_period[i]$Event_Msg_Type == 6)
      foul_lineup <- active_players
    
    # If a field goal converted, add points to active player stint
    if(game_period[i]$Event_Msg_Type == 1){
      points <- game_period[i, Option1]
      scoring_team <- game_period[i, Team_id]
      offense_players <- active_players[Team_id == scoring_team]$Person_id
      defense_players <- setdiff(active_players$Person_id, offense_players)
      
      # Update offense and defense players in pmat
      for(j in offense_players){
        pmat_row_out[, c(j) := get(j) + points]
      }
      for(k in defense_players){
        pmat_row_out[, c(k) := get(k) - points]
      }
      rm(j,k)
    } # End if Field Goal converted
    
    # Free throws, assign points to foul_lineup players
    if(game_period[i]$Event_Msg_Type == 3){
      points <- game_period[i, Option1]
      scoring_team <- game_period[i, Team_id]
      offense_players <- foul_lineup[Team_id == scoring_team]$Person_id
      defense_players <- setdiff(foul_lineup$Person_id, offense_players)
      
      # Update offense and defense players in pmat
      for(j in offense_players){
        pmat_row_out[, c(j) := get(j) + points]
      }
      for(k in defense_players){
        pmat_row_out[, c(k) := get(k) - points]
      }
      rm(j,k)
    }
    
    # Substitution, change active players
    if(game_period[i]$Event_Msg_Type == 8){
      active_players[Person_id == game_period[i]$Person1,
                     Person_id := game_period[i]$Person2]
    }
    
  }
  rm(i)
  return(pmat_row_out)
}


#-------- Calculate +/- for every Game_id/Period -----------------
pmat <- copy(pmat_empty)
for(i in seq(nrow(pmat_empty))){
  message(paste0("Calculating +/- for row ", i, " out of ", nrow(pmat_empty), ". ", 
                 round(100*i/nrow(pmat_empty), digits = 3), "% completed"))
  pmat[i] <- CalculatePlusMinus(pmat_empty[i])
}
rm(i)

#-------- Aggregate to Game/Player level, use correct colnames -----------
output <- melt(pmat, id.vars = c("Game_id", "Period"), 
               variable.name = "Player_ID", variable.factor = FALSE, 
               value.name = "Player_Plus/Minus") %>% 
  .[, .(`Player_Plus/Minus` = sum(`Player_Plus/Minus`, na.rm = T)), .(Game_id, Player_ID)] %>% 
  setnames("Game_id", "Game_ID")

#------------- Save -------------
export_dir <- "Basketball Analytics/output"
if(!dir.exists(export_dir)) dir.create(export_dir)
fwrite(pmat, file.path(export_dir, "Team_Name_Q1_BBALL.csv"))


