## Packages

library(dplyr)
library(tidyr)
library(tibble)
library(jsonlite)

# Set Parameters
schedule_filename = "2021-11-14_schedule.rds"

## Download shift data ##
#########################

## Load schedule data
games <- readRDS(paste0('../Data/',schedule_filename))

df_shift <- NULL

## loop through each game and download play data
for (i in 1:length(games$gamePk)) {
    
  print(i) # counter
    
  ## download shift data
  link <- paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=",games$gamePk[i])
  df <- fromJSON(link)
  tmp <- df$data
  
  if (length(df_shift) == 0)
  { df_shift <- tmp } else
  { df_shift <- rbind(df_shift, tmp) }
   
}

# save file
saveRDS(df_shift, '../Data/2021-11-14_shifts_2021.rds')


# below is the old NHL API endpoint link
# link <- paste0("http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=",games$gamePk[i])
