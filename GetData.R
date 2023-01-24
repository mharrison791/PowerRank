library(tidyverse)
library(worldfootballR)

#Load current Data
current_data <-read_csv("currentseasonsummaries.csv",
                        col_types = cols(...1 = col_skip(), X = col_skip()))
current_urls <- current_data %>%
  select(Game_URL)

#Get all season match URLs
epl_2023_urls <- fb_match_urls(country = "ENG", gender = "M", season_end_year = 2023, tier="1st")

new_urls <- setdiff(epl_2023_urls,current_urls$Game_URL)

#get new matches data
advanced_match_stats <- fb_advanced_match_stats(match_url = new_urls, stat_type = "summary", team_or_player = "team")
#create new data
current_data <- rbind(current_data,advanced_match_stats)
write.csv(current_data,"currentseasonsummaries.csv")

Season_H2H <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2023, tier = "1st")
write.csv(Season_H2H,"results.csv")
