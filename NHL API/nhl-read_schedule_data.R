### Packages

library(dplyr)
library(tidyr)
library(tibble)
library(jsonlite)

## Download the schedule of games ##
####################################

start_date <- '2011-01-01'
end_date   <- '2022-12-31'

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

saveRDS(games, file = paste0("../Data/",Sys.Date(),"_schedule.rds"))
