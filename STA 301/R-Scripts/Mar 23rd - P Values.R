library(mosaic)
library(tidyverse)
library(ggplot2)

LOL <- read.csv("C:/Users/jdots/OneDrive/STA 235/STA_235_Fall_2021-R-Sessions-and-Scripts/LOL.csv", sep=";")

#Getting rid of scientific notation
options(scipen = 999)

#Omitting NA
LOL <- na.omit(LOL)


#Selecting Variables
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

#Renaming Variables
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


nflip(n=25, prob=0.5)

sim_flips = do(10000)*nflip(n=25, prob=0.5)

ggplot(sim_flips) + 
  geom_histogram(aes(x=nflip), binwidth=1)

sum(sim_flips >= 19)

sum(sim_flips >= 19)/10000

lm1 <- lm(Gold ~ Kills, data=LOL_df)

summary(lm1)



