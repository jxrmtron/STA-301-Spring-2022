library(tidyverse)
library(mosaic)
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

# Plot Gold Valuation vs Kill Count
ggplot(LOL_df, aes(x = Kills, y = Gold)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = " Kills", 
       y = "Gold",
       title = "Scatterplot of relationship of Gold and Kills")

# Add some noise the the points so that they don't overlap
ggplot(LOL_df, aes(x = Kills, y = Gold)) +
  geom_jitter() +
  geom_smooth(method='lm') +
  labs(x = " Kills", 
       y = "Gold",
       title = "Scatterplot of relationship of Gold and Kills")


#Look at the correlation
cor(LOL_df$Gold, LOL_df$Kills)


#Building Model
lm1 <- lm(Gold ~ Kills, data=LOL_df)

#Summary Statistics Table
summary(lm1)

#Note the R^2 is equal to the correlation squared
cor(LOL_df$Gold, LOL_df$Kills)^2

#Adding predictions to dataset
LOL_df <- LOL_df %>%
  mutate(predictions = predict(lm1, newdata = LOL_df))

#Fitted
fitted(lm1) 
#Residuals
resid(lm1) 

#Adding them to dataset
LOL_df <- LOL_df %>%
  mutate(predicted = fitted(lm1),
         residuals = resid(lm1))



# Confidence interval
confint(lm1)


