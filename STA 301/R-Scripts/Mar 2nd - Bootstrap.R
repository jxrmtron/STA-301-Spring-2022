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

#Gold Distribution
ggplot(LOL_df) + 
  geom_histogram(aes(x = Gold), color = 'linen', binwidth=1000)
#Mean Gold
mean(~Gold, data=LOL_df) %>% 
  round(2)
#Resampling the data and getting a mean
LOL_df_boot = resample(LOL_df)
mean(~Gold, data=LOL_df_boot)
#Bootstrap on the mean of Gold
LOL_df_bootstrap = do(10000) * mean(~Gold, data=resample(LOL_df))

#Distribution of means from bootstrap
ggplot(LOL_df_bootstrap) + 
  geom_histogram(aes(x=mean))

#SD of means
sd(~mean, data=LOL_df_bootstrap)

#Confidence Interval
confint(LOL_df_bootstrap, level=0.95)

#Margin of Error
LOL_df_bootstrap %>% 
  summarise(
    lower = quantile(mean, 0.025),
    upper = quantile(mean, 0.975),
    margin_of_error = (upper-lower)/2)

2 * sd(~mean, data=LOL_df_bootstrap)

#Proportion of Blue Team that won
prop(~Win, data=LOL_df)
#Bootstrap on proportion of blue team that won
boot_Win = do(10000) * prop(~Win, data=resample(LOL_df))

#Distribution of win proportion from bootstrap
ggplot(boot_Win) + 
  geom_histogram(aes(x=prop_1))
# confidence Interval
confint(boot_Win, level = 0.95)

# Looking at Win counts
xtabs(~Win, data = LOL_df)

#Boxplot of gold distribution separated by whether the team won or not
ggplot(LOL_df, aes(x=factor(Win), y=Gold)) + 
  geom_boxplot()

#Mean gold separated by whether the team won or not
mean(Gold ~ Win, data=LOL_df)

#Difference in the means
diffmean(Gold ~ Win, data=LOL_df) 

#Bootstraping the mean difference
Gold_boot = do(1000) * diffmean(Gold ~ Win, data=resample(LOL_df))

#Distribution of our diffmeans
ggplot(Gold_boot) + 
  geom_histogram(aes(x=diffmean), color = 'gold')

#Confidence Interval for diffmean
confint(Gold_boot)  
#SD of diffmeans
sd(~diffmean, data=Gold_boot)
#Diffmean margin of error
2 * sd(~diffmean, data=Gold_boot)


# Win proportions
xtabs(~Win, data = LOL_df) %>% 
  prop.table() %>% 
  round(2) 
#Creating a Binary Variable
LOL_Baron <- LOL_df %>%
  filter(Baron == 1 | Baron == 0)

LOL_Baron <- LOL_Baron %>%
  mutate(Nashor = ifelse(Baron == 1, "Yes", "No"))

#Win proportions based  on if the team got baron or not
prop(Win ~ Nashor, data = LOL_Baron)

#Difference in proportions
diffprop(Win ~ Baron, data=LOL_Baron)

#Bootstrap on diffprops
Baron_boot = do(1000) * diffprop(Win ~ Baron, data=resample(LOL_Baron))

#Distribution of diffprops
ggplot(Baron_boot) + 
  geom_histogram(aes(x=diffprop), color = 'greenyellow')

#Confidence Interval
confint(Baron_boot)   


#Gold vs Kills
ggplot(LOL_df, aes(x = Kills, y = Gold)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = " Kills", 
       y = "Gold",
       title = "Scatterplot of relationship of Gold and Kills")

#Gold vs Kills model
lm1 <- lm(Gold ~ Kills, data=LOL_df)

#Regression Output
summary(lm1)
confint(lm1)

#Model Bootstrap
lm1_boot = do(10000) * lm(Gold ~ Kills, data = resample(LOL_df))

#Distribution of Kills Coeffecient Estimate
ggplot(lm1_boot) + 
  geom_histogram(aes(x=Kills), color = 'forestgreen', fill = 'gray90')

#Confidence Intervals on parameters of model
confint(lm1_boot)   
