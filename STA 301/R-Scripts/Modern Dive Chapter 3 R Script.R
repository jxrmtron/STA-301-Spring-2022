#Load Packages
library(dplyr)
library(ggplot2)
library(nycflights13)
#Filter the flights by destanation Portland, Oregon and pull up resulting table
portland_flights <- flights %>%
  filter(dest == "PDX")
View(portland_flights)
#Filter the flights by origin JFK for destanations Seattle or Burlington
# and in the months of October,November, December
btv_sea_flights_fall <- flights %>%
  filter(origin == "JFK", (dest == "BTV" | dest == "SEA"), month >= 10)
View(btv_sea_flights_fall)
#Filter that did NOT go to Burlington or Seattle
not_BTV_SEA <- flights %>%
  filter(!(dest == "BTV" | dest == "SEA"))
View(not_BTV_SEA)
#Filter that did  go to Burlington or Seattle or PDX or BDL or SFO
many_airports <- flights %>%
  filter((dest == "BTV" | dest == "SEA" | dest == "SFO" | dest == "PDX" |
          dest == "BDL"))
View(many_airports)
#EASIER way Filter that did  go to Burlington or Seattle or PDX or BDL or SFO
many_airports <- flights %>%
  filter(dest %in% c("BTV", "SEA", "SFO", "PDX", "BDL"))
#Summarize the mean and standard deviation of the temperatures
summary_temp <- weather %>%
  summarize(mean = mean(temp, na.rm = TRUE), std_dev = sd(temp, na.rm = TRUE))
summary_temp
#Summarize the mean and standard deviation of the temperatures grouped by month
summary_monthly_temp <- weather %>%
  group_by(month) %>%
  summarize(mean = mean(temp, na.rm = TRUE), std_dev = sd(temp, na.rm = TRUE))
summary_monthly_temp

diamonds
#Load the diamonds data frame by cut
diamonds %>%
  group_by(cut)
#Load the diamonds data frame by cut and find average prive
diamonds %>%
  group_by(cut) %>%
  summarize(average_price = mean(price))
#Ungroup a data set
diamonds %>%
  group_by(cut) %>%
  ungroup()
#Summarize the number of flights that left from each ariport
by_origin <- flights %>%
  group_by(origin) %>%
  summarize(count = n())
by_origin
#Summarize the number of flights that left from each airport by month
by_origin_monthly <- flights %>%
  group_by(origin, month) %>%
  summarize(count = n())
by_origin_monthly
#Create a new variable of temperature in celsius 
weather <- weather %>%
  mutate(temp_in_c = (temp - 32)/1.8)
#Summary of mean temps in Farenheiht and Celsius grouped by month
summary_monthly_temp <- weather %>%
  group_by(month) %>%
  summarize(mean_temp_in_F = mean(temp, na.rm = TRUE),
            mean_temp_in_c = mean(temp_in_c, na.rm = TRUE))
summary_monthly_temp
#create a new Gain variable in flights data frame
flights <- flights %>%
  mutate(gain = dep_delay - arr_delay) 
#Summary Statistics for Gain
gain_summary <- flights %>%
  summarize(
    min = min(gain, na.rm = TRUE),
    q1 = quantile(gain, 0.25, na.rm = TRUE),
    median = quantile(gain, 0.5, na.rm = TRUE),
    q3 = quantile(gain, 0.75, na.rm = TRUE),
    max = max(gain, na.rm = TRUE),
    mean = mean(gain, na.rm = TRUE),
    sd = sd(gain, na.rm = TRUE),
    missing = sum(is.na(gain))
  )
gain_summary
#Histogram of gain 
ggplot(data = flights, mapping = aes(x = gain)) + 
  geom_histogram(color = "white", bins = 20)

#Mutate multiple variables at once
flights <- flights %>% 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )
#Suppose we are interested in determining the most frequent destination airports
#for all domestic flights departing from New York City in 2013:
freq_dest <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n())
freq_dest
#Sort by least to most number of flights
freq_dest %>%
  arrange(num_flights)
#Sort by most to least number of flights
freq_dest %>%
  arrange(desc(num_flights))
#Joining flights and airlines datasets by key variable "carrier"
flights_joined <- flights %>%
  inner_join(airlines, by = "carrier")
#Join Data Sets with different key variables: dest and faa
flights_with_airport_names <- flights %>%
  inner_join(airports, by = c("dest" = "faa"))
View(flights_with_airport_names)
#Join Data Sets with different key variables and summaries: dest and faa
named_dests <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  rename(airport_name = name)
View(named_dests)
#Join by multiple key variables: year month day hour origin
flights_weather_joined <- flights %>%
  inner_join(weather, by = c("year", "month", "day", "hour", "origin"))
View(flights_weather_joined)
#Take a glimpse with only selected variables
flights %>%
  select(carrier, flight)
#Take a glimpse with only selected variables within a specified range
flight_arr_times <- flights %>%
  select(month:day, arr_time:sched_arr_time)   
flight_arr_times
#Reorder the Columns without discarding variables using everything
flights_reorder <- flights %>%
  select(year, month, day, minute, time_hour, everything())
glimpse(flights_reorder)
#Used to select variables that meet conditons
flights %>% select(starts_with("a"))
flights %>% select(ends_with("delay"))
flights %>% select(contains("time"))
#Select variables and rename them
flights_time_new <- flights %>% 
  select(dep_time, arr_time) %>% 
  rename(departure_time = dep_time, arrival_time = arr_time)
glimpse(flights_time_new)
#Return only the top n of a variable
named_dests %>% top_n(n = 10, wt = num_flights)
#Return only the top n of a variable in descinding order
named_dests  %>% 
  top_n(n = 10, wt = num_flights) %>% 
  arrange(desc(num_flights))

