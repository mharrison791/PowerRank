library(tidyverse)
library(lubridate)
library(reactablefmtr)
library(scales)
#library(worldfootballR)


temppal <- c('#36a1d6', '#76b8de', '#a0bfd9', '#ffffff', '#d88359', '#d65440', '#c62c34')
Season_H2H <- read_csv("~/R/Football/Fbref ddata/CurrentSeason_Results.txt")


image_url <- 'https://images.fotmob.com/image_resources/logo/teamlogo/'

df <- Season_H2H %>%
  drop_na(Wk)%>%
  drop_na(Score)

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

pivot_df <- pivot_df %>%
  mutate(Home_Goals_Actual = substr(Score, 1, 1))%>%
  mutate(Away_Goals_Actual = substr(Score,3,3))%>%
  rename("Home xG" = xG...6)%>%
  rename("Away xG" = xG...8 )

pivot_df$Home_Goals_Actual <- as.numeric(pivot_df$Home_Goals_Actual)
pivot_df$Away_Goals_Actual <- as.numeric(pivot_df$Away_Goals_Actual)
pivot_df$Date <- ymd(pivot_df$Date)

pivot_df <- pivot_df %>%
  mutate(Points = ifelse((Location == "Home" & Home_Goals_Actual > Away_Goals_Actual), 3, 
                         ifelse(Location == "Away" & Away_Goals_Actual > Home_Goals_Actual,3,
                                ifelse(Home_Goals_Actual == Away_Goals_Actual,1,0))))

pivot_df <- pivot_df %>%
  mutate(Actual_GD = ifelse(Location == "Home",Home_Goals_Actual - Away_Goals_Actual,
                            Away_Goals_Actual - Home_Goals_Actual))%>%
  mutate(x_GD = ifelse(Location == "Home",`Home xG` - `Away xG`,
                       `Away xG` - `Home xG`))

pivot_df <- pivot_df %>%
  mutate(Actual_GF = ifelse(Location == "Home",Home_Goals_Actual,
                            Away_Goals_Actual))%>%
  mutate(Actual_GA = ifelse(Location == "Home", Away_Goals_Actual,
                            Home_Goals_Actual))

pivot_df <- pivot_df %>%
  mutate(Actual_xG = ifelse(Location == "Home",`Home xG`,
                            `Away xG`))%>%
  mutate(Actual_xGA = ifelse(Location == "Home", `Away xG`,
                             `Home xG`))

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
            xGD = sum(x_GD),
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
  mutate(Power_Ranking = rank(Power_Calc, ties.method = 'min'))


#####Plot

binded_df %>%
  ggplot(aes(Wk))+
  geom_line(aes(y = cum_Actual_Goals, color = "var0"))+
  geom_line(aes(y = cum_XGF, color = "var1"))+
  facet_wrap( ~ Team)+
  theme_dark()

joined_df <- left_join(ranking_df,icons_df, by = c("Team"="Home"))

table_df <- joined_df %>%
  select(Power_Ranking,Team,iconurl,Points,Total_GF,Total_xG,Total_GA,Total_xGA, GD, xGD,Differential)%>%
  arrange(Power_Ranking)


reactable(
  table_df,
  theme = fivethirtyeight(),
  pagination = FALSE,
  columnGroups = list(
    colGroup(name = "Actual", columns = c("Total_GF","Total_GA","GD")),
    colGroup(name = "Expected", columns = c("Total_xG","Total_xGA","xGD"))
  ),
  columns = list(
    Power_Ranking = colDef(name = "Power Rank",
                           align = "center",
                           maxWidth = 60),
    Team = colDef(name='',
                  align = 'center'),
    iconurl = colDef(name = '',
                     cell = embed_img(
                       height = 25,
                       width = 25)
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
      name = "Goals For",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal)
    ),
    Total_xG = colDef(
      maxWidth = 60,
      name = "xGoals For",
      align = "center",
      style= color_scales(table_df,
                          colors = temppal)
    ),
    Total_GA = colDef(
      maxWidth = 60,
      name = "Goals Against",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal)
    ),
    Total_xGA = colDef(
      maxWidth = 60,
      name = "xGoals Against",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal)
    ),
    GD = colDef(
      maxWidth = 60,
      name = "Goal Difference",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal)
    ),
    xGD = colDef(
      maxWidth = 60,
      name = "xGoal Difference",
      align = "center",
      style = color_scales(table_df,
                           colors = temppal)
    ),
      Differential = colDef(
        name = "Goal Differential",
        align = "center",
        cell = data_bars(table_df,
                         fill_color = temppal,
                         number_fmt = number_format(accuracy = 0.1))
      )
    )
  )