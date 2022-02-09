#Initial Libraries we were told to load
library(nycflights13)
library(ggplot2)
library(dplyr)
#look at the flights data frame
glimpse(flights)
#Load the data frame
View(Flights)
#This requires paring down the data from all 336,776 
#flights that left NYC in 2013, to only the 714 Alaska 
#Airlines flights that left NYC in 2013. We do this so
#our scatterplot will involve a manageable 714 points
alaska_flights <- flights %>%
  filter(carrier == "AS")
#Look at the newly created data frame alaska_flights
View(alaska_flights)
#Scatterplot of dep_delay vs arr_delay
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay )) + 
  geom_point()
#Scatterplot of dep_time vs arr_time
ggplot(data = alaska_flights, mapping = aes(x = dep_time, y = arr_time )) + 
  geom_point()
#Scatterplot of dep_delay vs arr_delay with transparency
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay )) + 
  geom_point(alpha = 0.2)
#Scatterplot of dep_delay vs arr_delay with jitter
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay )) + 
  geom_jitter(width = 30, height = 30)
#Load Weather Data Frame
View(weather)
#look at weather data frame
glimpse(weather)
#Bring up the help file
?weather
# Pair down the data from only the Newark airport in the relevant range January
# 1st to 15th
early_january_weather <- weather %>%
  filter(origin == "EWR", month == 1, day <= 15)
# Line Graph of Time in hours vs Temp
ggplot(data = early_january_weather, 
       mapping = aes(x = time_hour, y = temp)) +
  geom_line()
#Mapping the temp on a histogram
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram()
#Mapping the temp on a histogram with white borders
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(color = "white")
#Mapping the temp on a histogram with white borders and a steel blue fill
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(color = "white", fill = "steel blue")
#Mapping the temp on a histogram with white borders and a steel blue fill
# and adjusting the number of bins
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(bins = 40, color = "white", fill = "steel blue")
#Mapping the temp on a histogram with white borders and a steel blue fill
# and adjusting the bin width
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 10, color = "white", fill = "steel blue")
geom_histogram(bins = 40, color = "white", fill = "steel blue")
#Mapping the temp on a histogram with white borders and a steel blue fill
# and adjusting the bin width and separating by months using facets
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 5, color = "white", fill = "steel blue") +
  facet_wrap(~month, nrow = 4)
#Create a box plot of hourly temperatures split between the 12 months
ggplot(data = weather, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot()
#Barplot of the different count of apples and oranges without count
ggplot(data = fruits, mapping = aes(x = fruit)) +
  geom_bar()
#Barplot of the different count of apples and oranges with count
ggplot(data = fruits_counted, mapping = aes(x = fruit, y = number)) +
  geom_col()
# Mapping the number of flights by carrier using a bar plot (no precount)
ggplot(data = flights, mapping = aes(x = carrier)) + 
  geom_bar()
# Example of Mapping the number of flights by carrier using a bar plot (precount)
ggplot(data = flights, mapping = aes(x = carrier, y = number)) + 
  geom_col()
# Mapping the number of flights by carrier using a bar plot with a fill by each 
#  airpot (no precount)
ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) + 
  geom_bar()
# Mapping the number of flights by carrier using a bar plot with dodge (no precount)
ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) + 
  geom_bar(position = "dodge")
# Mapping the number of flights by carrier using a bar plot with dodge  (no precount)
# preservation of size of bars
ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) + 
  geom_bar(position = position_dodge(preserve = "single"))
# Mapping a Faceted Barplot by origin
ggplot(data = flights, mapping = aes(x = carrier)) +
  geom_bar() +
  facet_wrap(~origin, ncol = 1)

