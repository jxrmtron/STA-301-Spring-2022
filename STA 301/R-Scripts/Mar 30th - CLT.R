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

#Central Limit Theorem
LOL_df_resample <- resample(LOL_df,100)

ggplot(LOL_df_resample) + 
  geom_histogram(aes(x=Gold), bins=20)

N = 10
sim1 = do(10000) * mean(~Gold, data=sample(LOL_df_resample, size=N))

ggplot(sim1) + 
  geom_histogram(aes(x=mean), color = 'snow', bins=30) 


#De Moivre's Equation
sigma = sd(~Gold, data=LOL_df_resample)
sigma 

N = 100
sim2 = do(10000) * sd(~Gold, data=sample(LOL_df_resample, size=N))

mean(sim2$sd)

sigma/sqrt(N)

#Prop Test
LOL_Baron <- LOL_df %>%
  filter(Baron == 1 | Baron == 0)

prop(Win ~ Baron, data=LOL_Baron) %>% 
  round(3)

diffprop(Win ~ Baron, data=LOL_Baron) %>% 
  round(3)

prop.test(Win ~ Baron, data=LOL_Baron)

boot_Prop= do(10000) * diffprop(Win ~ Baron, 
                                  data=resample(LOL_Baron,100))
confint(boot_Prop) 


# LM Confints

lm1 = lm(Gold ~ Baron, data=LOL_Baron)
coef(lm1) 

boot_Baron = do(10000) * lm(Gold ~ Baron, 
                            data=resample(LOL_Baron))
confint(boot_Baron)

lm1 %>% 
  confint() %>% 
  round(2)
 

