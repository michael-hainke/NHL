### Packages

library(dplyr)
library(tidyr)
library(jsonlite)

## Get Team Data ##
###################

# get latest team data from NHL API
teamids <- paste(c(1:58), collapse = ',')
teams <- fromJSON(paste0("https://statsapi.web.nhl.com/api/v1/teams?teamId=",teamids))
teams = flatten(teams$teams)

# get updates to start year of play and also new last year of play
updates <- read.csv("../Data/team_years.csv")
for (i in 1:length(updates$id)) { teams[teams$id == updates$id[i] & teams$name == updates$name[i], "firstYearOfPlay"] = updates$firstYearOfPlay[i]
                                  teams[teams$id == updates$id[i] & teams$name == updates$name[i], "lastYearOfPlay"] = updates$lastYearOfPlay[i]
}

# save final data frame
saveRDS(teams, file = paste0("../Data/",Sys.Date(),"_teams.rds"))
