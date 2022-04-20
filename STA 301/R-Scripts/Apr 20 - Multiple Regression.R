library(tidyverse)
library(mosaic)
library(moderndive)
library(effectsize)

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





# Relationship between Gold and Kills
ggplot(LOL_df,aes(x=Kills, y=Gold)) +
  geom_point() +
  geom_smooth(method='lm')

#Relationship of cofounder Baron
ggplot(LOL_df) +
  geom_jitter(aes(x=Baron, y=Gold), width=0.05)

#Relationship of cofounder Dragon
ggplot(LOL_df) +
  geom_jitter(aes(x=Dragon, y=Gold), width=0.05)

#Also correlated with Kills so it should be included in our model
ggplot(data=LOL_df) +
  geom_jitter(aes(x=Baron, y= Kills))

cor(LOL_df$Baron, LOL_df$Kills)

ggplot(data=LOL_df) +
  geom_jitter(aes(x=Dragon, y= Kills))

cor(LOL_df$Dragon, LOL_df$Kills)

#Multiple Regression for Dragon, Baron, and Kills
lol_multiple = lm(Gold ~ Dragon + Baron + Kills, data=LOL_df)

summary(lol_multiple)

#Difference between categorical and numerical variables (Dragon can act as both. You dont have to
#know why, just trust me)
lol_multiplef = lm(Gold ~ factor(Dragon) + Baron + Kills, data=LOL_df)

summary(lol_multiplef)

#Baseline Offset model used to predict
23765.24 + (1374.62*2 ) + 4641.17  + (20*374.29)   

#Predict function used to predict
predict(lol_multiple, list(Baron = 1,
                           Dragon = 2,
                           Kills = 20))

lol_multiple1 = lm(Gold ~ Dragon + Baron + Kills + CounterJungle, data=LOL_df)

summary(lol_multiple1)

ggplot(LOL_df) + 
  geom_point(aes(x=fitted(lol_multiple1), y= Gold), alpha=0.2)

#Standardizing your coefficients
standardize_parameters(lol_multiple)

standardize_parameters(lol_multiple1)



