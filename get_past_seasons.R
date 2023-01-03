library(tidyverse)
library(worldfootballR)


all_tables <- data.frame()

years <- seq(2002,2022,1)

for(i in years){
  df <- fb_season_team_stats("ENG", "M", i, "1st", "league_table")
  df <- subset(df,select=4:15)
  all_tables <- rbind(all_tables,df)
  print(i)
}

all_tables <- all_tables %>%
  mutate(WP = W/MP)

xposition.lm <-lm(Rk ~ GF + GA, data = all_tables)
summary(xposition.lm)




write.csv(all_tables,'epl_past_seasons.csv')