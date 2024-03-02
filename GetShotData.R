library(tidyverse)
library(worldfootballR)

#Load current Data
current_data <-read_csv("base_data_shots.csv",col_types = cols(...1 = col_skip()))

current_urls <- current_data %>%
  select(GameURL)%>%
  distinct(GameURL)

#Get all season match URLs
epl_2024_urls <- fb_match_urls(country = "ENG", gender = "M", season_end_year = 2024, tier="1st")
new_urls <- setdiff(epl_2024_urls,current_urls$GameURL)

shot_df <- data.frame()

#get new matches data and add URL
for (i in new_urls){
  d <- fb_match_shooting(match_url = i)
  d$GameURL <- i
  shot_df <- rbind(shot_df,d)
  print(i)
}

#create new data
current_data <- rbind(current_data,shot_df)

write.csv(current_data,"base_data_shots.csv")

