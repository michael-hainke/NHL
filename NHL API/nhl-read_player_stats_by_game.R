### Packages

library(dplyr)
library(tidyr)
library(jsonlite)

# Set Parameters
players_filename = "2021-11-12_players.rds"
rosters_filename = "2021-11-11_rosters.rds"

## Get Game by Game Scoring Data ##
###################################

# Load player data
players <- readRDS(paste0("../Data/",players_filename))
rosters <- readRDS(paste0("../Data/",rosters_filename))

# get game by game data for each player
games <- NULL
for (i in 1:length(players$id)) {
 seasons <- filter(rosters, person.id == players$id[i]) %>% select(season)
 seasons <- unique(seasons)
 for (season in seasons$season) {
   print(paste0(players[players$id==players$id[i],"fullName"],": ",season,"-",season+1))
   url <- paste0("https://statsapi.web.nhl.com/api/v1/people/",players$id[i],"/stats?stats=gameLog&season=",season, season+1)
   tmp <- try(fromJSON(url), silent=TRUE)
   if(is.data.frame(tmp$stats[[2]][[1]])) {
     tmp <- flatten(tmp$stats[[2]][[1]]) %>%
            mutate(id = players$id[i],
                   fullName = players$fullName[i])
  
     if (length(games) == 0) { games <- tmp } else
     { # add any columns missing in players dataframe
       columns <- names(tmp[!names(tmp) %in% names(games)])
       if (length(columns) > 0) {
         for (col in 1:length(columns)) {
           games <- mutate(games, !!columns[col] := NA)
         }
       }
       # add any columns missing in tmp dataframe
       columns <- names(games[!names(games) %in% names(tmp)])
       if (length(columns) > 0) {
         for (col in 1:length(columns)) {
           tmp <- mutate(tmp, !!columns[col] := NA)
         }
       }
       games <- rbind(games, tmp) }
   }
 }
}

# save final data frame
saveRDS(games, file = paste0("../Data/",Sys.Date(),"_player_stats_by_game.rds"))
