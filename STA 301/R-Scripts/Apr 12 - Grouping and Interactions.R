library(tidyverse)
library(mosaic)

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
#Filter for Top 3 leagues
Top3_LOL_df <- LOL_df %>%
  filter( league == "LCK" | league == "LCS" | league == "LEC")

#Check the class of our variable
class(Top3_LOL_df$Baron)

#Change the class from integer to factor
Top3_LOL_df$Baron <- factor(Top3_LOL_df$Baron)

#Relationship between Gold and Baron
ggplot(Top3_LOL_df) + 
  geom_boxplot(aes(x=Baron, y=Gold)) 

#Mean of Gold by Baron
mean(Gold ~ Baron, data=Top3_LOL_df)

#Difference in those means
diffmean(Gold ~ Baron, data=Top3_LOL_df,only.2 = FALSE)

#First Model
Baron_model <- lm(Gold ~ Baron, data=Top3_LOL_df)

#Summary Output
summary(Baron_model)

#Relationship between Gold Baron
ggplot(Top3_LOL_df) + 
  geom_boxplot(aes(x=league, y=Gold), 
               width=0.1) 
#Class of our Herald variable
class(Top3_LOL_df$Herald)

#Factoring the Herald variable
Top3_LOL_df$Herald <- factor(Top3_LOL_df$Herald)

#Relationship between Gold and Herald (categorical cofounder)
ggplot(Top3_LOL_df) + 
  geom_boxplot(aes(x=Herald, y=Gold)) 

#Relationship between Gold and CounterJungle (numerical cofounder)
ggplot(LOL_df, aes(x = CounterJungle, y = Gold)) +
  geom_point() +
  geom_smooth(method='lm')

#Baron Herald model with no interaction
Baron_Herald = lm(Gold ~ Baron + Herald, data = Top3_LOL_df)

summary(Baron_Herald)

#Formula for killing one baron and one herald
29061+11963+3568-3971

#Visualizing the interaction between Herald and Baron
ggplot(Top3_LOL_df) + 
  geom_boxplot(aes(x=Baron, y=Gold)) +
  facet_wrap(~Herald)

#Baron and Herald model with interaction term
Baron_Herald = lm(Gold ~ Baron + Herald + Baron:Herald, data = Top3_LOL_df)
summary(Baron_Herald)

#Predict function
predict(Baron_Herald, list(Baron = "1",
                             Herald = "2"))

#ANOVA
library(effectsize)
eta_squared(Baron_Herald, partial=FALSE)
