library(tidyverse)
library(worldfootballR)

#Load current Data
current_data <-read_csv("base_data.csv",
                        col_types = cols(...1 = col_skip()))

current_urls <- current_data %>%
  select(Game_URL)%>%
  distinct(Game_URL)

#Get all season match URLs
epl_2024_urls <- fb_match_urls(country = "ENG", gender = "M", season_end_year = 2024, tier="1st")
new_urls <- setdiff(epl_2024_urls,current_urls$Game_URL)

#get new matches data
advanced_match_stats <- fb_advanced_match_stats(match_url = new_urls, stat_type = "summary", team_or_player = "team")
#create new data
current_data <- rbind(current_data,advanced_match_stats)

write.csv(current_data,'base_data.csv')

current_data <- current_data %>% 
  group_by(Team) %>% 
  mutate(rnk = row_number(Matchweek))
current_data <- rank_data %>% 
  group_by(Team) %>%
  mutate(RevRnk = rev(rnk))

###Get Current Table
current_table <- fb_season_team_stats(country = "ENG", gender = "M", season_end_year = "2024", tier = "1st", stat_type = "league_table")%>%
  select(Squad,Rk)
###Adjust Team Names to constant
team_names <- current_data%>%
  select(Team)%>%
  distinct(Team)
team_names$Team <- sort(team_names$Team)
current_table$Team <- team_names$Team


write.csv(current_table,'current_table.csv')

write.csv(rank_data,"currentseasonsummaries.csv")

