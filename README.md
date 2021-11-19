# NHL

This repository contains scripts for a variety of NHL analyses.

## 1 - NHL API

These are a variety of scripts to get different NHL data using the NHL API.  All scripts are saving output data into the 'Data' folder.  For data files beginning with a date, this is the latest update date, run the script and change dates to get newer data if required.  Some of the scripts require updated data from another script, ie play by play and shift data needs the complete game schedule data.  See below for more details.

### nhl-read_schedule_data.R
Game schedules including date, teams, final score and game id (gamePk).  Script outputs to **'games.rds'** file, which is used for other API scripts below.

### nhl-read_team_data.R
Team data including name, venue, start and end years, division, conference, among others.  Some of the teams start and end years were not correct, so I join correct from 'team_years.csv'.  Script outputs to **'teams.rds'**, which is used to get player data below.

### nhl-read_player_data.R
Roster and Player data.  Uses full list of team data (from 'teams.rds') to get all the roster data for each team for each season in existence and save to **'rosters.rds'**.  Using all unique players from full roster table, retrieves player data including name, number, birthdate, height, weight, nationality, primary postions, among others and saves to **'players.rds'**.

### nhl-read_player_stats_by_game.R
Player game stats. Uses roster data ('roster.rds') and player data ('player.rds') to get boxscore player game stats, including game, date, goals, assists, pim, hits, blocks, plus minus, shooting %, etc. Queries all data for each game that each player has played and saves to **'player_stats_by_game.rds'**.

### nhl-read_play_by_play_data.R
Full play by play data for faceoffs, shots, goals, hits, etc. including players involved, location, description of play, time of play, opposing team etc.  This is the most complete play data available from NHL API, and as such is the core of the data used for something like an xG model. Uses schedule data ('games.rds') and queries all data for each play for each game.  Due to size of dataframe, this is split into files for each calendar year, **'plays_2020.rds'**.  The current year data is preceded by the last update date.

### nhl-read_shift_data.R
Shift data for each player, including player, team, shift number, start time, end time and duration.  This data can be used to determine 5v5, 5v4 etc.  Uses schedule data ('games.rds') and queries all shifts for each game.  Due to size of dataframe, this is split into files for each calendar year, **'shifts_2020.rds'**.  The current year data is preceded by the last update date.

## 2 - xG (Expected Goals) Model

This folder contains my first crack at building a simple xG model using publicly available data from the NHL API.  Model is a 4 feature logistic regression using a WOE transformation, with an AUC of approximately 72%.  Will document here more fully in the near future.

## 3 - Other Analyses

### Scoring Bar Charts
This is a script to create those cool animated bar charts showing changes over time.  I used to create some team level overall scoring year over year, to visualize the franchise scoring leaders over time

### First to Forty
A script to calculate the players who took the least number of games to get to 40 points in a season.
