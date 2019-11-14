### Packages

library(dplyr)
library(tidyr)
library(jsonlite)

## Get Team Data ##
#####################

teamids <- paste(c(1:58), collapse = ',')
teams <- fromJSON(paste0("https://statsapi.web.nhl.com/api/v1/teams?teamId=",teamids))

df_team <- teams$teams
write.csv(df_team,"teams.csv")  # manually add the missing start years, and also add end years
df_team <- read.csv("teams.csv")
saveRDS(df_team, file = '../Data/teams.rds')

## Get Roster Data ##
#####################

min_year = min(df_team$firstYearOfPlay)

roster <- NULL
for (id in 1:max(df_team$id)) {
  for (season in min_year:2018) {
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

saveRDS(df, file = '../Data/rosters.rds')

## Get Player Data ##
#####################

# identify unique players and get all player data for each
player_ids <- unique(rosters$person.id)

# fetch player data
players <- NULL
for (id in player_ids) {
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
  print(id)
}

saveRDS(players, file = '../Data/players.rds')

## Get Game by Game Scoring Data ##
###################################

# get game by game data for each play
games <- NULL
for (i in 1:length(players$id)) {
 print(i)
 seasons <- filter(rosters, person.id == players$id[i]) %>% select(season)
 seasons <- unique(seasons)
 for (season in seasons$season) {
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

saveRDS(games, file = '../Data/games.rds')
