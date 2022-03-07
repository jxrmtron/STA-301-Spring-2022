library(tidyverse)
library(mosaic)
library(ggplot2)

LOL <- read.csv("C:/Users/jdots/OneDrive/STA 235/STA_235_Fall_2021-R-Sessions-and-Scripts/LOL.csv", sep=";")

LOL <- na.omit(LOL)

options(scipen = 999)

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

#Line Plot
ggplot(data = LOL_df, mapping = aes(x = Kills, y = Gold )) + 
  geom_line() +
  labs(x="Kills", 
       y="Gold", 
       title="Kills Against Gold") 

#Histogram
ggplot(LOL_df) +
  geom_histogram(aes(x=Gold)) +
  labs(x="Gold", 
       title="Distribution of Gold")

#Boxplots
ggplot(LOL_df) + 
  geom_boxplot(aes(x=league, y=Gold)) +
  facet_wrap(~Win) +
  labs(x="League", 
       y="Gold", 
       title="Gold Distribution by League Parsed by Win")

#Summary Statistics

#Mean
LOL_df %>% 
  summarize(mean_Gold = mean(Gold))

mean(~Gold, data = LOL_df)

mean(LOL_df$Gold)

#Summarize function
LOL_df %>% 
  summarize(mean_Gold = mean(Gold),
            median_Gold = median(Gold))

#Standard Deviation   
LOL_df %>% 
  summarize(sd_Gold = sd(Gold))

# Interquartile range (IQR)
LOL_df %>% 
  summarize(iqr_Gold = IQR(Gold))

# Range
max(LOL_df$Gold) - min(LOL_df$Gold)


#All in one
favstats(~Gold, data = LOL_df)




#Data Wrangling

summary_LOL_df <- LOL_df %>%
  group_by(league) %>%
  summarize(total_count = n(),
            mean_Gold = mean(Gold),
            median_Gold = median(Gold))

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

LOL_Baron <- LOL_df %>%
  filter(Baron == 1 | Baron == 0)

LOL_Baron <- LOL_Baron %>%
  mutate(Nashor = ifelse(Baron == 1, "Yes", "No"))

#Linear Regression 
ggplot(LOL_df, aes(x = Kills, y = Gold)) +
  geom_point() +
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

# Confidence interval
confint(lm1)


#Bootstrapping 
set.seed(1234)

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

#Model Bootstrap
lm1_boot = do(10000) * lm(Gold ~ Kills, data = resample(LOL_df))

#Distribution of Kills Coeffecient Estimate
ggplot(lm1_boot) + 
  geom_histogram(aes(x=Kills), color = 'forestgreen', fill = 'gray90')

#Confidence Intervals on parameters of model
confint(lm1_boot)   


