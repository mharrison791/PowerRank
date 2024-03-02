library(tidyverse)
library(lubridate)
library(reactablefmtr)
library(scales)
library(worldfootballR)



temppal <- c('#540202', '#AB3131', '#EDE0A6', '#799163','#507B58')
temppal_rev <- c('#507B58','#799163', '#EDE0A6', '#AB3131','#540202')

Season_H2H <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2024, tier = "1st")


image_url <- 'https://images.fotmob.com/image_resources/logo/teamlogo/'

df <- Season_H2H %>%
  drop_na(Wk)%>%
  drop_na(HomeGoals)%>%
  mutate(Wk = as.numeric(Wk))%>%
  arrange(Wk)

icons_df <- df %>%
  distinct(Home)%>%
  arrange(Home)

icons_df$Ids <- c('9825', '10252', '8678', '9937', '10204','8191', '8455', '9826','8668','9879','8650','8346','8456','10260','10261','10203','8657','8586','8654','8602')
icons_df$sourceurl <- image_url
icons_df$iconurl <- paste0(icons_df$sourceurl,icons_df$Ids,".png")
write.csv(icons_df,"icons.csv")

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

pivot_df <- pivot_df %>% 
  group_by(Team) %>% 
  mutate(rnk = row_number(Wk))
pivot_df <- pivot_df %>% 
  group_by(Team) %>%
  mutate(RevRnk = rev(rnk))

pivot_df <- pivot_df %>%
  group_by(Team) %>%
  mutate(cum_Actual_Goals = cumsum(Actual_GF))%>%
  mutate(cum_XGF = cumsum(Actual_xG))%>%
  mutate(W = if_else(Points==3,1,0))

#write.csv(pivot_df, 'completeresults.csv')

#### All Games
all_games_df <- pivot_df %>%
  group_by(Team)%>%
  summarise(MP = n(),
    Total_GF = sum(Actual_GF),
    Total_GA = sum(Actual_GA),
    Total_xG = sum(Actual_xG),
    Total_xGA = sum(Actual_xGA),
    Points = sum(Points),
    GD = sum(Actual_GD),
    xGD = round(sum(x_GD),1),
    Differential = sum(sum(Actual_GD-x_GD)),
    WP = sum(W/MP))%>%
  mutate(Season_xG = (Total_xG/MP)*38)%>%
  mutate(Season_xGA = (Total_xGA/MP)*38)%>%
  mutate(xPosition_season = round(18.687+(0.008*Season_xG)+(Season_xGA*0.052)+(WP*-29.141),1))%>%
  select(Team,xPosition_season)%>%
  ungroup()


#### Get most recent games
most_recent_games <- 6

ranking_df <- pivot_df %>%
  filter(RevRnk <= most_recent_games)%>%
  group_by(Team)%>%
  summarise(Total_GF = sum(Actual_GF),
            Total_GA = sum(Actual_GA),
            Total_xG = sum(Actual_xG),
            Total_xGA = sum(Actual_xGA),
            Points = sum(Points),
            GD = sum(Actual_GD),
            xGD = round(sum(x_GD),1),
            Differential = sum(sum(Actual_GD-x_GD)),
            WP = sum(W/most_recent_games))%>%
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
  mutate(Season_GF = (Total_GF/6)*38)%>%
  mutate(Season_GA = (Total_GA/6)*38)%>%
  mutate(xPosition_form = round(14.706 +(Season_GF*-0.038)+(Season_GA *0.120)+(WP*-22.299),1))

ranking_df <- left_join(ranking_df,all_games_df, by = c("Team"="Team"))

prev_ranking_df <- binded_df %>%
  filter(Wk > 1 & RecentGames <= Wk + 1)%>%
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

###Get Current Table
current_table <- fb_season_team_stats(country = "ENG", gender = "M", season_end_year = "2024", tier = "1st", stat_type = "league_table")%>%
  select(Squad,Rk)

ranking_df <- left_join(ranking_df,current_table, by = c("Team"="Squad"))

write.csv(ranking_df,"ranking_dataset.csv")
#####Plot

binded_df %>%
  ggplot(aes(Wk,group = 1))+
  geom_line(aes(y = cum_Actual_Goals, color = "var0"))+
  geom_line(aes(y = cum_XGF, color = "var1"))+
  facet_wrap( ~ Team)+
  theme_dark()

joined_df <- left_join(ranking_df,icons_df, by = c("Team"="Home"))

table_df <- joined_df %>%
  mutate(Change = paste0("(",Prev_Power_Ranking - Power_Ranking,")"))%>%
  select(Power_Ranking,Change,iconurl,Team,Points,Total_GF,Total_xG,Total_GA,Total_xGA, GD, xGD,Differential, xPosition_form, xPosition_season)%>%
  arrange(Power_Ranking)


power_ranking_table <- reactable(
  table_df,
  theme = fivethirtyeight(header_font_size = 10),
  pagination = FALSE,
  columnGroups = list(
    colGroup(name = "Actual", columns = c("Total_GF","Total_GA","GD")),
    colGroup(name = "Expected", columns = c("Total_xG","Total_xGA","xGD")),
    colGroup(name = "Relative Performance", columns = c("xPosition_form", "xPosition_season"))
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
        maxWidth = 125,
        align = "center",
        cell = data_bars(table_df,
                         text_position = "outside-end",
                         fill_color = temppal,
                         number_fmt = number_format(accuracy = 0.1))
      ),
    xPosition_form = colDef(
      maxWidth = 100,
      name = "Relative Position",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal_rev)
    ),
    xPosition_season = colDef(
      maxWidth = 100,
      name = "Season Expectation",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal_rev)
    )
    )
  )%>%
  add_title("Premier League Power Rankings - Last 6 games", margin = margin(0, 0, 10, 0)) %>%
  add_source("Table created by: Matt Harrison with {reactablefmtr} •  Data: fbref.com", font_size = 12)

power_ranking_table
power_ranking_table %>%
  save_reactable_test("table.html")

webshot2::webshot('table.html', "rtableSnapshot.png",vwidth = 1200, vheight = 800)


df_with_percentiles <- ranking_df%>%
  mutate(percent_GF = rank(Total_GF)/length(Total_GF))%>%
  mutate(percent_GA = rank(desc(Total_GA))/length(Total_GA))%>%
  mutate(percent_xG = rank(Total_xG)/length(Total_xG))%>%
  mutate(percent_xGA = rank(desc(Total_xGA))/length(Total_xGA))%>%
  mutate(percent_points = rank(Points)/length(Points))%>%
  mutate(percent_GD = rank(GD)/length(GD))%>%
  mutate(percent_xGD = rank(xGD)/length(xGD))%>%
  mutate(percent_Diff = rank(Differential)/length(Differential))%>%
  select(Team,Total_GF,Total_GA,Total_xG,Total_xGA,GD,Points,xGD,Differential,
         percent_GF,percent_GA,percent_xG,percent_xGA,percent_GD,percent_points,percent_xGD,percent_Diff)

team_percentiles <- df_with_percentiles%>%
  select(Team,percent_GF,percent_GA,percent_xG,percent_xGA,percent_GD,percent_points,percent_xGD,percent_Diff)%>%
  rename("Goals For" = percent_GF)%>%
  rename("Goals Against" = percent_GA)%>%
  rename("Expected Goals For" = percent_xG)%>%
  rename("Expected Goals Against" = percent_xGA)%>%
  rename("Points" = percent_points)%>%
  rename("Goal Difference" = percent_GD)%>%
  rename("Expected Goal Difference" = percent_xGD)%>%
  rename("Differential" = percent_Diff)%>%
  pivot_longer(!Team, names_to = "Dimension", values_to = "Percentile")

Team_Metrics <- df_with_percentiles %>%
  select(Team,Total_GF,Total_GA,Total_xG,Total_xGA,GD,Points,xGD,Differential)%>%
  rename("Goals For" = Total_GF)%>%
  rename("Goals Against" = Total_GA)%>%
  rename("Expected Goals For" = Total_xG)%>%
  rename("Expected Goals Against" = Total_xGA)%>%
  rename("Points" = Points)%>%
  rename("Goal Difference" = GD)%>%
  rename("Expected Goal Difference" = xGD)%>%
  rename("Differential" = Differential)%>%
  pivot_longer(!Team, names_to = "Dimension", values_to = "Metric")

team_data <- left_join(Team_Metrics,team_percentiles, by=c("Team"="Team","Dimension"="Dimension"))

t <- "Everton"
single_team <- team_data%>%
  filter(Team==t)

team <- reactable(
  single_team,
  theme = fivethirtyeight(header_font_size = 10),
  pagination = FALSE,
  columns = list(
    Team = colDef(show = FALSE),
    Dimension = colDef(
      maxWidth = 175,
      name= ""
    ),
    Metric = colDef(maxWidth = 60,
                    name = "Total",
                    align = "center"),
    Percentile = colDef(
      cell = data_bars(single_team,
                       text_position = "inside-end", 
                       number_fmt = scales::percent,
                       fill_color = temppal,
                       max_value = 1)
    )
  )
)

icon_selection <- icons_df%>%
  filter(Home==t)%>%
  mutate(html = paste0("<img src='",iconurl,"'width='50' height='50'>"))

single_team <- team %>%
  add_title(
    title = html(icon_selection$html),
    align = 'left'
  )%>%
  add_subtitle(
    subtitle = icon_selection$Home
  )

single_team %>%
  save_reactable_test(paste0(t,".html"))

webshot2::webshot(paste0(t,".html"), paste0(t,"rtableSnapshot.png"),vwidth = 600, vheight = 400)

ggplot(single_team, aes(x=Dimension, y=Percentile)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Metric), hjust=0, color="black", size=3.5)+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_minimal()+
  theme(axis.text.y=element_text(hjust=0,vjust=0),
        axis.title.y = element_blank())+
  coord_flip()

