library(tidyverse)
library(lubridate)
library(reactablefmtr)
library(scales)
library(worldfootballR)


temppal <- c('#36a1d6', '#76b8de', '#a0bfd9', '#ffffff', '#d88359', '#d65440', '#c62c34')
temppal_rev <- c('#c62c34','#d65440', '#d88359', '#ffffff','#a0bfd9', '#76b8de', '#36a1d6')

Season_H2H <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2023, tier = "1st")
epl_2023_urls <- fb_match_urls(country = "ENG", gender = "M", season_end_year = 2023, tier="1st")
advanced_match_stats <- fb_advanced_match_stats(match_url = epl_2023_urls, stat_type = "summary", team_or_player = "team")

advanced_match_stats_Columns <- advanced_match_stats%>%
  select(Game_URL,Team,Home_Away, Sh, SoT, SCA_SCA, Prog_Passes)


image_url <- 'https://images.fotmob.com/image_resources/logo/teamlogo/'

df <- Season_H2H %>%
  drop_na(Wk)%>%
  drop_na(HomeGoals)%>%
  mutate(Wk = as.numeric(Wk))%>%
  arrange(Wk)

icons_df <- df %>%
  distinct(Home)%>%
  arrange(Home)

icons_df$Ids <- c('9825', '10252','8678', '9937', '10204', '8455', '9826','8668','9879','8463','8197','8650','8456','10260','10261','10203','8466','8586','8654','8602')
icons_df$sourceurl <- image_url
icons_df$iconurl <- paste0(icons_df$sourceurl,icons_df$Ids,".png")

df$MatchID <- seq.int(nrow(df))

pivot_df <- df %>%
  pivot_longer(cols = c('Home', 'Away'), 
               names_to = "Location",
               values_to = "Team")

pivot_df <- left_join(pivot_df,icons_df, by = c("Team"="Home"))

pivot_df$Date <- ymd(pivot_df$Date)

pivot_df <- pivot_df %>%
  mutate(Points = ifelse((Location == "Home" & HomeGoals > AwayGoals), 3, 
                         ifelse(Location == "Away" & AwayGoals > HomeGoals,3,
                                ifelse(HomeGoals == AwayGoals,1,0))))

pivot_df <- pivot_df %>%
  mutate(Actual_GD = ifelse(Location == "Home",HomeGoals - AwayGoals,
                            AwayGoals - HomeGoals))%>%
  mutate(x_GD = ifelse(Location == "Home",`Home_xG` - `Away_xG`,
                       `Away_xG` - `Home_xG`))

pivot_df <- pivot_df %>%
  mutate(Actual_GF = ifelse(Location == "Home",HomeGoals,
                            AwayGoals))%>%
  mutate(Actual_GA = ifelse(Location == "Home", AwayGoals,
                            HomeGoals))%>%
  mutate(Actual_xG = ifelse(Location == "Home",`Home_xG`,
                            `Away_xG`))%>%
  mutate(Actual_xGA = ifelse(Location == "Home", `Away_xG`,
                             `Home_xG`))

list_of_teams <- unique(pivot_df$Team)

binded_df <- data.frame()

for (i in list_of_teams){
  d <- pivot_df %>%
    filter(Team == i)
  d$GameNumber <- seq.int(nrow(d))
  d$RecentGames <- rev(d$GameNumber)
  binded_df <- rbind(binded_df,d)
}

binded_df <- binded_df %>%
  group_by(Team) %>%
  mutate(cum_Actual_Goals = cumsum(Actual_GF))%>%
  mutate(cum_XGF = cumsum(Actual_xG))

write.csv(binded_df, 'completeresults.csv')

#### All Games
all_games_df <- binded_df %>%
  group_by(Team)%>%
  summarise(MP = n(),
    Total_GF = sum(Actual_GF),
            Total_GA = sum(Actual_GA),
            Total_xG = sum(Actual_xG),
            Total_xGA = sum(Actual_xGA),
            Points = sum(Points),
            GD = sum(Actual_GD),
            xGD = round(sum(x_GD),1),
            Differential = sum(sum(Actual_GD-x_GD)))%>%
  mutate(Season_xG = (Total_xG/MP)*38)%>%
  mutate(Season_xGA = (Total_xGA/MP)*38)%>%
  mutate(xPosition_season = round(8.133+(-0.181*Season_xG)+(Season_xGA*0.228),1))%>%
  select(Team,xPosition_season)%>%
  ungroup()


#### Get most recent games
most_recent_games <- 6

ranking_df <- binded_df %>%
  filter(RecentGames <= most_recent_games)%>%
  group_by(Team)%>%
  summarise(Total_GF = sum(Actual_GF),
            Total_GA = sum(Actual_GA),
            Total_xG = sum(Actual_xG),
            Total_xGA = sum(Actual_xGA),
            Points = sum(Points),
            GD = sum(Actual_GD),
            xGD = round(sum(x_GD),1),
            Differential = sum(sum(Actual_GD-x_GD)))%>%
  mutate(Rank_GF = rank(desc(Total_GF), ties.method = 'min'))%>%
  mutate(Rank_GA = rank(Total_GA, ties.method = 'min'))%>%
  mutate(Rank_xG = rank(desc(Total_xG), ties.method = 'min'))%>%
  mutate(Rank_xGA = rank(Total_xGA, ties.method = 'min'))%>%
  mutate(Rank_Points = rank(desc(Points), ties.method = 'min'))%>%
  mutate(Rank_GD = rank(desc(GD), ties.method = 'min'))%>%
  mutate(Rank_xGD = rank(desc(xGD), ties.method = 'min'))%>%
  mutate(Rank_Differential = rank(desc(Differential), ties.method = 'min'))%>%
  mutate(Power_Calc = Rank_Differential+
           Rank_xGD+
           Rank_GD+
           Rank_Points+
           Rank_xGA+
           Rank_xG+
           Rank_GA+
           Rank_GF)%>%
  mutate(Power_Ranking = rank(Power_Calc, ties.method = 'min'))%>%
  mutate(Season_xG = (Total_GF/6)*38)%>%
  mutate(Season_xGA = (Total_GA/6)*38)%>%
  mutate(xPosition_form = round(8.133 +(Season_xG*-0.181)+(Season_xGA *0.228),1))

ranking_df <- left_join(ranking_df,all_games_df, by = c("Team"="Team"))

prev_ranking_df <- binded_df %>%
  filter(RecentGames > 1 & RecentGames <= most_recent_games + 1)%>%
  group_by(Team)%>%
  summarise(Total_GF = sum(Actual_GF),
            Total_GA = sum(Actual_GA),
            Total_xG = sum(Actual_xG),
            Total_xGA = sum(Actual_xGA),
            Points = sum(Points),
            GD = sum(Actual_GD),
            xGD = round(sum(x_GD),1),
            Differential = sum(sum(Actual_GD-x_GD)))%>%
  mutate(Rank_GF = rank(desc(Total_GF), ties.method = 'min'))%>%
  mutate(Rank_GA = rank(Total_GA, ties.method = 'min'))%>%
  mutate(Rank_xG = rank(desc(Total_xG), ties.method = 'min'))%>%
  mutate(Rank_xGA = rank(Total_xGA, ties.method = 'min'))%>%
  mutate(Rank_Points = rank(desc(Points), ties.method = 'min'))%>%
  mutate(Rank_GD = rank(desc(GD), ties.method = 'min'))%>%
  mutate(Rank_xGD = rank(desc(xGD), ties.method = 'min'))%>%
  mutate(Rank_Differential = rank(desc(Differential), ties.method = 'min'))%>%
  mutate(Power_Calc = Rank_Differential+
           Rank_xGD+
           Rank_GD+
           Rank_Points+
           Rank_xGA+
           Rank_xG+
           Rank_GA+
           Rank_GF)%>%
  mutate(Prev_Power_Ranking = rank(Power_Calc, ties.method = 'min'))%>%
  select(Team,Prev_Power_Ranking)

ranking_df <- left_join(ranking_df,prev_ranking_df, by = c("Team"="Team"))

#####Plot

binded_df %>%
  ggplot(aes(Wk,group = 1))+
  geom_line(aes(y = cum_Actual_Goals, color = "var0"))+
  geom_line(aes(y = cum_XGF, color = "var1"))+
  facet_wrap( ~ Team)+
  theme_dark()

joined_df <- left_join(ranking_df,icons_df, by = c("Team"="Home"))
joined_df

table_df <- joined_df %>%
  mutate(Change = paste0("(",Power_Ranking - Prev_Power_Ranking,")"))%>%
  select(Power_Ranking,Change,iconurl,Team,Points,Total_GF,Total_xG,Total_GA,Total_xGA, GD, xGD,Differential, xPosition_form, xPosition_season)%>%
  arrange(Power_Ranking)


reactable(
  table_df,
  theme = fivethirtyeight(header_font_size = 10),
  pagination = FALSE,
  columnGroups = list(
    colGroup(name = "Actual", columns = c("Total_GF","Total_GA","GD")),
    colGroup(name = "Expected", columns = c("Total_xG","Total_xGA","xGD")),
    colGroup(name = "Relative Position", columns = c("xPosition_form", "xPosition_season"))
  ),
  columns = list(
    Power_Ranking = colDef(name = "Power Rank (Change)",
                           align = "center",
                           cell = merge_column(table_df, "Change", merged_position = "right"),
                           maxWidth = 100),
    Change = colDef(show = FALSE),
    iconurl = colDef(name = '',
                     maxWidth = 60,
                     cell = embed_img(
                       height = 20,
                       width = 20)
    ),
    Team = colDef(name='',
                  align = 'center',
                  maxWidth = 150
    ),
    Points = colDef(
      maxWidth = 60,
      name = "Points",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal)
    ),
    Total_GF = colDef(
      maxWidth = 60,
      name = "GF",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal)
    ),
    Total_xG = colDef(
      maxWidth = 60,
      name = "GF",
      align = "center",
      style= color_scales(table_df,
                          colors = temppal)
    ),
    Total_GA = colDef(
      maxWidth = 60,
      name = "GA",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal_rev)
    ),
    Total_xGA = colDef(
      maxWidth = 60,
      name = "GA",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal_rev)
    ),
    GD = colDef(
      maxWidth = 60,
      name = "GD",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal)
    ),
    xGD = colDef(
      maxWidth = 60,
      name = "GD",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal)
    ),
      Differential = colDef(
        name = "Goal Differential",
        align = "center",
        cell = data_bars(table_df,
                         text_position = "outside-end",
                         fill_color = temppal,
                         number_fmt = number_format(accuracy = 0.1))
      ),
    xPosition_form = colDef(
      maxWidth = 80,
      name = "Relative Position",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal_rev)
    ),
    xPosition_season = colDef(
      maxWidth = 80,
      name = "Season Expectation",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal_rev)
    )
    )
  )%>%
  add_title("Premier League Power Rankings - Last 6 games", margin = margin(0, 0, 10, 0)) %>%
  add_source("Table created by: Matt Harrison with {reactablefmtr} •  Data: fbref.com", font_size = 12)