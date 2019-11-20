### Packages

library(dplyr)
library(tidyr)
library(tibble)
library(jsonlite)

## Download the schedule of games ##
####################################

start_date <- '2007-01-01'
end_date   <- '2019-11-16'

schedule <- fromJSON(paste0("https://statsapi.web.nhl.com/api/v1/schedule?startDate=",start_date,"&endDate=",end_date))

df_games <- schedule$dates$games
games <- NULL
for (i in 1:length(df_games)) {
  if (!is.null(df_games[[i]])) {
    tmp <- flatten(df_games[[i]])

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

## clean and prep data
games <- mutate(games, gameDate = as.Date(substring(games$gameDate, 1, 10)))

saveRDS(games, file = '../Data/schedule.rds')

## Download play by play data ##
################################

## Load schedule data
games <- readRDS('../Data/schedule.rds')

## Filter to 2011 and later (first season that had coordinate data)
games <- filter(games, gameDate > '2011-09-01')

## initialize dataframes
df_final <- NULL

## loop through each game and download play data
for (i in 1:length(games$gamePk)) {

  print(i) # counter
  
  ## download play by play data
  link <- paste0("https://statsapi.web.nhl.com",games$link[i])
  df <- fromJSON(link)
  
  ## extract and merge play by play data into one dataframe
  df_result <- df$liveData$plays$allPlays$result
  df_about <- df$liveData$plays$allPlays$about
  df_coord <- df$liveData$plays$allPlays$coordinates
  df_team <- df$liveData$plays$allPlays$team
  df_plays <- cbind(df_result, df_about, df_coord, df_team, row.Names = FALSE)
  
  ## extract play by play data
  df_players <- df$liveData$plays$allPlays$players
  players <- NULL
  for (j in 1:length(df_players)) {
    if (!is.null(df_players[[j]])) {
    tmp <- flatten(df_players[[j]]) %>% select(player.fullName,playerType) %>% mutate(eventCode = df_result$eventCode[j])
    tmp1 <- filter(tmp, playerType == 'Assist') %>% mutate(playerType = paste0(playerType,row_number()))
    tmp <- rbind(filter(tmp, playerType != 'Assist'),tmp1)
    if (length(players) == 0) {
      players <- tmp } else { players <- rbind(players, tmp) }
    }
  }
  
  # this is to deal with row 6515 which has duplicate 'unknown' player values
  players <- distinct(players)
  
  if(!is.null(players)) { players <- spread(players, key=playerType, value=player.fullName) 
  
  df_plays <- left_join(df_plays, players, by = c("eventCode")) %>%
              mutate(gamePk = games$gamePk[i],                      # add game id
                     link = games$link[i],                          # add API link
                     gameType = games$gameType[i],                  # add game type (regular season etc.)
                     away_team = games$teams.away.team.name[i],     # add away team
                     home_team = games$teams.home.team.name[i]) %>% # add home team
              flatten()                                             # flatten any lists
  }
  
  ## add plays to final dataframe 
  if (df_plays > 1) {
  if (length(df_final) == 0) { df_final <- df_plays } else
                             { # add any columns missing in df_final dataframe
                               columns <- names(df_plays[!names(df_plays) %in% names(df_final)])
                               if (length(columns) > 0) {
                                 for (col in 1:length(columns)) {
                                   df_final <- mutate(df_final, !!columns[col] := NA)
                                 }
                               }
                               # add any columns missing in df_plays dataframe
                               columns <- names(df_final[!names(df_final) %in% names(df_plays)])
                               if (length(columns) > 0) {
                                 for (col in 1:length(columns)) {
                                   df_plays <- mutate(df_plays, !!columns[col] := NA)
                                 }
                               }
                               df_final <- rbind(df_final, df_plays) }
  }
}

# save file
saveRDS(df_final, '../Data/plays.rds')

# check list
missing_games <- games$gamePk[!games$gamePk %in% unique(df_final$gamePk)]
seasons       <- substring(missing_games, 1, 4)
table(seasons) # total of 251 missing games of 11292 (2.2%)

## Download shift data ##
#########################

df_shift <- NULL

## loop through each game and download play data
for (i in 1:length(games$gamePk)) {
    
  print(i) # counter
    
  ## download shift data
  link <- paste0("http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=",games$gamePk[i])
  df <- fromJSON(link)
  tmp <- df$data
  
  if (length(df_shift) == 0)
  { df_shift <- tmp } else
  { df_shift <- rbind(df_shift, tmp) }
   
}

# save file
saveRDS(df_shift, '../Data/shifts.rds')
