### Packages

library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)

# Set Parameters
team_filename = "2021-11-11_teams.rds"

## Get Roster Data ##
#####################

# Load team data
df_team <- readRDS(paste0("../Data/",team_filename))

# Replace NAs with current year for last year of play for active teams
df_team <- df_team %>%
           mutate(lastYearOfPlay = case_when(is.na(lastYearOfPlay) ~ as.integer(year(Sys.Date())),
                                             TRUE ~ lastYearOfPlay))

# Loop through each team and each year of play to get all team rosters
roster <- NULL
for (id in 1:max(df_team$id)) {
  for (season in df_team$firstYearOfPlay[id]:df_team$lastYearOfPlay[id]) {
    print(paste0(df_team$name[id],": ",season,"-",season+1))
    tmp <- try(fromJSON(paste0("https://statsapi.web.nhl.com/api/v1/teams?teamId=",id,"&expand=team.roster&season=",season,season+1)), silent=TRUE)
    if (!grepl("error",tmp)) {
      tmp <- flatten(as.data.frame(tmp$teams$roster$roster)) %>%
             mutate(teamId = id,
                    name = df_team$name[id],
                    season = season)
      if (length(roster) == 0) {
      roster <- tmp } else { roster <- rbind(roster, tmp) }
    } else warning(paste0("Did not find ",df_team$name[id]," ",season))
  }
}

# Save final data frame
saveRDS(roster, file = paste0("../Data/",Sys.Date(),"_rosters.rds"))

## Get Player Data ##
#####################

# identify unique players and get all player data for each
player_ids <- unique(roster$person.id)

# fetch player data
players <- NULL
i = 1
for (id in player_ids) {
  print(paste0(i," of ",length(player_ids)," : ",roster[roster$person.id==id,"person.fullName"][1]))
  tmp <- try(fromJSON(paste0("https://statsapi.web.nhl.com/api/v1/people/",id)), silent=TRUE)
  if (!grepl("error",tmp)) {
    tmp <- flatten(tmp$people)
    
    if (length(players) == 0) { players <- tmp } else
    { # add any columns missing in players dataframe
      columns <- names(tmp[!names(tmp) %in% names(players)])
      if (length(columns) > 0) {
        for (col in 1:length(columns)) {
          players <- mutate(players, !!columns[col] := NA)
        }
      }
      # add any columns missing in tmp dataframe
      columns <- names(players[!names(players) %in% names(tmp)])
      if (length(columns) > 0) {
        for (col in 1:length(columns)) {
          tmp <- mutate(tmp, !!columns[col] := NA)
        }
      }
      players <- rbind(players, tmp) }
  } else warning(paste0("Did not find ", players[id]))
  i = i+1
}

# clean data
players <- players %>% mutate(birthDate = as.Date(birthDate))

# save final data frame
saveRDS(players, file = paste0("../Data/",Sys.Date(),"_players.rds"))

