library(ggplot2)
library(tidyverse)

 LOL <- read.csv("C:/Users/jdots/OneDrive/STA 235/STA_235_Fall_2021-R-Sessions-and-Scripts/LOL.csv", sep=";")
 
 LOL <- na.omit(LOL)
 
 # Choosing Variables
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
          "Kills" = t1_kills)
 
 
 
 #Line plot
 ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold )) + 
   geom_line() +
   labs(x="Kills", 
        y="Gold", 
        title="Kills Against Gold") 
 
 #Line plot with data points
 ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold )) + 
   geom_line() +
   labs(x="Kills", 
        y="Gold", 
        title="Kills Against Gold")  +
   geom_point(aes(x = Kills, y = Gold))
 
 #Histograms
 ggplot(LOL_df) +
   geom_histogram(aes(x=Gold))
 
 ggplot(LOL_df) +
   geom_histogram(aes(x=Gold, color = 'snow'), binwidth = 1)
 
 #Filtering Data
 LOL_df <- LOL_df %>%
   filter( league == "LCK" | league == "LCS" | league == "LPL" | league == "LEC"
   )
 
 
 ggplot(LOL_df) +
   geom_histogram(aes(x=Gold), color = 'snow', binwidth = 1000) + 
   facet_wrap(~league)
 
 ggplot(LOL_df) + 
   geom_boxplot(aes(x=league, y=Gold)) +
   facet_wrap(~Win) 
 
 ggplot(LOL_df) + 
   geom_boxplot(aes(x=league, y=Gold)) +
   facet_wrap(~Win) +
   coord_flip()
 
#Summary Statistics
 #Mean 
LOL_df %>% 
  summarize(mean_Gold = mean(Gold))

mean(~Gold, data = LOL_df)

mean(LOL_df$Gold)

#Mean and Median   
LOL_df %>% 
  summarize(mean_Gold = mean(Gold),
               median_Gold = median(Gold))
LOL_df %>% 
  summarise(
    avg_Gold = mean(Gold),
    avg_Kills = mean(Kills),
    avg_Gold = mean(Dmg))
   
library(mosaic)
favstats(~Gold, data = LOL_df)
#Standard Deviation   
LOL_df %>% 
  summarize(sd_Gold = sd(Gold))
   
# Interquartile range (IQR)
LOL_df %>% 
  summarize(iqr_Gold = IQR(Gold))
   
# Range
max(LOL_df$Gold) - min(LOL_df$Gold)

 
 