
# Packages
library(tidyverse) # data manipulation and plotting

# Load previously scraped API data
games <- rbind(readRDS('../Data/games.rds'),
               readRDS('../Data/19-11-16_games2019.rds'))

# Prep Data
df <- games %>%
      select(Player = fullName, stat.points, Season = season, date, Team = team.name) %>%
      group_by (Season, Player) %>%
      arrange(date) %>%
      mutate(Pts = cumsum(stat.points),
             Num_Games = row_number(),
             Player_Season = paste0(Player," ",Season)) %>%
      filter(Pts >= 40) %>%
      mutate(rank = row_number()) %>%
      filter(rank == 1) %>%
      ungroup() %>%
      select(Player_Season, Player, Season, Team, Pts, Num_Games) %>%
      arrange(Num_Games)

# Save Data
write.csv(df, '../Data/first_to_forty.csv')