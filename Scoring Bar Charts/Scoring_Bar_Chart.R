# Packages
library(tidyverse) # data manipulation and plotting
library(gganimate) # chart animation
library(viridis)   # colour palettes
library(gifski)    # image rendering
library(png)       # image rendering
library(rvest)     # web scraping

# Variables
path <- '//*[(@id = "div_skaters")]/table[1]'
names <- c('Player','Age','Pos','GP','G','A','Pts','+/-','PIM','Goals-EV','Goals-PP','Goals-SH','Goals-GW','Assists-EV','Assists-PP',
           'Assists-SH','S','S%','TOI','ATOI','OPS','DPS','PS')

team <- 'VAN'
team_name <- 'Vancouver Canucks'
seasons <- c(1971:2004,2006:2019)

team <- 'STL'
team_name <- 'St. Louis Blues'
seasons <- c(1968:2004,2006:2019)

# Scrape scoring for each year and combine
stats <- NULL
for (i in seasons) {
  
  url <- paste0("https://www.hockey-reference.com/teams/",team,"/",i,".html")
  
  df <- url %>%
    read_html() %>%
    html_nodes(xpath=path) %>%
    html_table()
  
  df <- as.data.frame(df[[1]])
  df <- df[2:(nrow(df)-1),2:24]
  colnames(df) <- names
  df <- mutate(df, season = paste0(i-1,'-',i))
  
  if (length(stats) == 0)
  { stats <- df } else
  { stats <- rbind(stats, df) }
  
}

# Remove Asterisks
stats$Player <- gsub("[*]","",stats$Player)

# VAN ONLY - Add middle initials to distinguish both Greg Adams
stats <- mutate(stats, Player = case_when(Player == 'Greg Adams' & Pts == 6 ~ 'Greg C Adams',
                                          Player == 'Greg Adams' ~ 'Greg D Adams',
                                          TRUE ~ Player))

# Save data
saveRDS(stats, file = paste0(team,'_Scoring.rds'))

# Summarize total career points by year for each player
plot_df <- select(stats, Player, Pts, season) %>%
           group_by(Player) %>%
           mutate(Total_Pts = cumsum(Pts),
                  First_Season = min(season),
                  Last_Season = max(season),
                  Career_Pts = max(Total_Pts)) %>%
           select(-Pts) %>%
           spread(key = season, value = Total_Pts, fill = 0)

plot_df <- gather(plot_df, season, Total_Pts, 5:dim(plot_df)[2]) %>%
           mutate(Total_Pts = case_when(season > Last_Season ~ Career_Pts,
                                        TRUE ~ Total_Pts)) %>%
           select(Player, season, Total_Pts)

# Filter data to include only top 10 players for each year
plot_df <- group_by(plot_df, season) %>%
           mutate(rank = rank(-Total_Pts),
                  Value_rel = Total_Pts/Total_Pts[rank==1],
                  Value_lbl = paste0(" ",Total_Pts)) %>%
           group_by(season) %>% 
           filter(rank <=10) %>%
           ungroup()

# Create Static Plot
staticplot = ggplot(plot_df, aes(rank, group = Player, 
                                       fill = as.factor(Player), color = as.factor(Player))) +
                    scale_fill_viridis(discrete=TRUE) +
                    scale_color_viridis(discrete=TRUE) +
                    geom_tile(aes(y = Total_Pts/2,
                                  height = Total_Pts,
                                  width = 0.9), alpha = 0.8, color = NA) +
                    geom_text(aes(y = 0, label = paste(Player, " ")), vjust = 0.2, hjust = 1, color = "black") +
                    geom_text(aes(y=Total_Pts,label = Value_lbl, hjust=0), color = "black") +
                    coord_flip(clip = "off", expand = FALSE) +
                    scale_y_continuous(labels = scales::comma) +
                    scale_x_reverse() +
                    guides(color = FALSE, fill = FALSE) +
                    theme(axis.line=element_blank(),
                          axis.text.x=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          legend.position="none",
                          panel.background=element_blank(),
                          panel.border=element_blank(),
                          panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),
                          panel.grid.major.x = element_line( size=.1, color="grey" ),
                          panel.grid.minor.x = element_line( size=.1, color="grey" ),
                          plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
                          plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
                          plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
                          plot.background=element_blank(),
                          plot.margin = margin(2,2, 2, 4, "cm"))

# Animated Plot
anim = staticplot + transition_states(season, transition_length = 4, state_length = 3) +
       view_follow(fixed_x = TRUE)  +
       labs(title = paste0(team_name," All Time Point Leaders"),  
            subtitle  =  "Season : {closest_state}",
            caption  = "Total Regular Season Pts | Data Source: www.hockey-reference.com")

# Render Plot
animate(anim, fps = 10, duration = 45, width = 600, height = 400, end_pause = 100, detail = 1, rewind = FALSE,
        renderer = gifski_renderer(paste0(team,"_alltime.gif")))


