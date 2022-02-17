library(tidyverse)
library(mosaic)
library(ggplot2)

LOL <- read.csv("C:/Users/jdots/OneDrive/STA 235/STA_235_Fall_2021-R-Sessions-and-Scripts/LOL.csv", sep=";")

options(scipen = 999)

LOL <- na.omit(LOL)

LOL_df <- LOL %>%
  select(t1_earnedgold,
         t1_golddiffat15,
         t1_earned.gpm,
         t1_damagetochampions,
         t1_kills,
         t1_deaths,
         t1_assists,
         t1_barons,
         t1_elementaldrakes,
         t1_heralds,
         t1_towers,
         t1_inhibitors,
         t1_visionscore,
         t1_minionkills,
         t1_monsterkills,
         t1_monsterkillsenemyjungle,
         t1_xpat15,
         t2_xpat25,
         t1_csdiffat15,
         t1_result,
         t1_killsat15,
         league,
         playoffs,
         gamelength
  )

LOL_df <- LOL_df%>%
  rename("Gold" = t1_earnedgold,
         "Dmg" = t1_damagetochampions,
         "Dragon" = t1_elementaldrakes,
         "Herald" = t1_heralds,
         "GoldPerMin"= t1_earned.gpm,
         "GoldDiff_15" = t1_golddiffat15,
         "CounterJungle" = t1_monsterkillsenemyjungle,
         "CSDiff" = t1_csdiffat15,
         "Baron" = t1_barons,
         "Towers" = t1_towers,
         "Inhibitors" = t1_inhibitors,
         "VisionScore" = t1_visionscore,
         "Kills_15" = t1_killsat15,
         "Win" = t1_result,
         "Kills" = t1_kills,
         "Assists" = t1_assists,
         "Deaths" = t1_deaths)

summary_LOL_df <- LOL_df %>%
  group_by(league) %>%
  summarize(total_count = n(),
            mean_Gold = mean(Gold),
            median_Gold = median(Gold))

ggplot(LOL_df, aes(x = Gold)) +
  geom_histogram()

ggplot(LOL_df, aes(x = Gold)) +
  geom_histogram(bins = 50)

LCK_LOL_df <- LOL_df %>%
  filter(league == "LCK" & Gold >30000) %>%
  summarize(total_count = n(),
            mean_Gold = mean(Gold),
            median_Gold = median(Gold))

LOL_df <- LOL_df %>%
  mutate(KDA = (Kills+Assists)/Deaths)

Top3_LOL_df <- LOL_df %>%
  group_by(league,Win)%>%
  filter( league == "LCK" | league == "LCS" | league == "LEC") %>%
  summarize(total_count = n(),
            mean_Gold = mean(Gold),
            median_Gold = median(Gold))

ggplot(data = Top3_LOL_df) + 
  geom_col(mapping = aes(x= league, y= mean_Gold)) 

ggplot(data = Top3_LOL_df) + 
  geom_col(mapping = aes(x= league, y= mean_Gold)) +
  facet_wrap(~Win)

ggplot(data = Top3_LOL_df) + 
  geom_col(mapping = aes(x= league, y= mean_Gold, fill = league)) +
  facet_wrap(~Win)

ggplot(data = Top3_LOL_df) + 
  geom_col(mapping = aes(x= league, y= mean_Gold,  fill = league)) + 
  facet_wrap(~Win) + 
  labs(title="Average Gold by League", 
       y="Average Gold",
       x = "League",
       fill = "League")



