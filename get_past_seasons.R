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

all_tables_with_expected_stats <- data.frame()

years_x <- seq(2018,2022,1)

for(i in years_x){
  df <- fb_season_team_stats("ENG", "M", i, "1st", "league_table")
  #df <- subset(df,select=4:15)
  all_tables_with_expected_stats <- rbind(all_tables_with_expected_stats,df)
  print(i)
}

all_tables <- all_tables %>%
  mutate(WP = W/MP)

all_tables_with_expected_stats <- all_tables_with_expected_stats %>%
  mutate(WP = W/MP)

xposition.lm <-lm(Rk ~ GF + GA + WP, data = all_tables)
summary(xposition.lm)

xposition.lm.x <- lm(Rk ~ xG + xGA + WP, data = all_tables_with_expected_stats)
summary(xposition.lm.x)

#define weights to use
wt <- 1 / lm(abs(xposition.lm$residuals) ~ xposition.lm$fitted.values)$fitted.values^2

weighted_model <- lm(Rk ~ GF + GA + WP, data = all_tables, weights=wt)
summary(weighted_model)

#create residual vs. fitted plot
plot(fitted(xposition.lm), resid(xposition.lm), xlab='Fitted Values', ylab='Residuals')


write.csv(all_tables,'epl_past_seasons.csv')