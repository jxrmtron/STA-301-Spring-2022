library(tidyverse)
library(ggplot2)

LOL <- read.csv("C:/Users/jdots/OneDrive/STA 235/STA_235_Fall_2021-R-Sessions-and-Scripts/LOL.csv", sep=";")

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
         "Kills" = t1_kills)



ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold )) + 
  geom_point()

ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold, color = league )) + 
  geom_point()

ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold, color = "steelblue" )) + 
  geom_point()

ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold, color = league, size = playoffs )) + 
  geom_point()

ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold, color = league, size = playoffs, alpha = gamelength )) + 
  geom_point()

ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold, size = playoffs, alpha = gamelength )) + 
  geom_point() +
  facet_wrap(~ league, nrow = 2)

ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold, size = gamelength, alpha = gamelength, color = league )) + 
  geom_point() +
  facet_wrap(~ playoffs)

LOL_df <- LOL_df %>%
  filter( league == "LCK" | league == "LCS" | league == "LPL" | league == "LEC"
  )

ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold, size = gamelength, alpha = gamelength, color = league )) + 
  geom_point() +
  facet_wrap(~ playoffs)

plot1 <- ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold, size = gamelength, alpha = gamelength, color = league )) + 
  geom_point() +
  facet_wrap(~ playoffs)


plot1 + 
  labs(
  title = "Kills against Gold",
  x="Kills",
  y="Gold"
)

LOL_df$playoffs[LOL_df$playoffs==1] <- "Playoffs"

LOL_df$playoffs[LOL_df$playoffs==0] <- "No Playoffs"

plot1 <- ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold, size = gamelength, alpha = gamelength, color = league )) + 
  geom_point() +
  facet_wrap(~ playoffs)


plot1 + 
  labs(
    title = "Kills against Gold",
    x="Kills",
    y="Gold"
  )
