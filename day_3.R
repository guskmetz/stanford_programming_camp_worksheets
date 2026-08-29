# Stanford Econ PhD Programming Camp Day 3
# This is a script file to help you follow along
#install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)
library(stargazer)
library(AER)   # coeftest() via lmtest, vcovHC() via sandwich
library(lfe)   # felm()

########################### Tidy data ########################### 

table1
table2
table3
table4a
table4b
table5


# plot cases by country over time
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))


# Recreate the plot showing change in cases over time using `table2` instead of `table1`. What do you need to do first?
table2 %>%
  filter(type == "cases") %>%
  ggplot(aes(x = year, y = count, color = country)) + 
  geom_point() + 
  geom_line()



# fix table 4a
table4a %>%
  pivot_longer(c(`1999`, `2000`), 
               names_to = "year",
               values_to = "cases")

# fix table 4b
table4b %>%
  pivot_longer(c(`1999`, `2000`), 
               names_to = "year",
               values_to = "population")

# fix table 2
table2 %>%
  pivot_wider(names_from = "type", 
              values_from = "count")

# why are `pivot_longer()` and `pivot_wider()` not perfectly symmetrical?
# carefully consider the following example:


stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return") %>%
  mutate(year = as.numeric(year))


# why does this code fail?  (run it and read the error)

table4a %>% 
  pivot_longer(c(1999, 2000), 
               names_to = "year", 
               values_to = "cases")

# now fix it:




# fix up table 3


# fix up table 5
table5 %>%
  unite(col = full_year, century, year, sep = "")

table5 %>%
  mutate(full_year = paste0(century, year)) %>%
  select(-century, -year)

########################### Review dplyr ########################### 

# Please load the `nycflights13` package
# Take a look at the `flights` data set
data(flights)


flights %>%
  glimpse()

# Find all flights that:

# 1.  Had an arrival delay of two or more hours
flights %>%
  filter(arr_delay )

# 2.  Flew to Houston (IAH or HOU)
flights %>%
  filter(dest )

# 3.  Arrived more than two hours late, but didn’t leave late
flights %>%
  filter(arr_delay ,
         dep_delay )


# Explore a bit:

# 1. What does a cancelled flight look like?

# 2. Make a data set called `not_cancelled` that contains all non-cancelled flights
# an example using is.na()
not_cancelled <-
  flights %>%
  filter(!is.na(air_time))

# the same thing using the tidyr function drop_na()
not_cancelled <-
  flights %>%
  drop_na(air_time)
# or arrival time

# 3. Make a data set called `cancelled` that contains all cancelled_flights
cancelled <-
  flights %>%
  filter(is.na(arr_time))


# Use `arrange()` to:

# 1. Sort flights to find the most delayed flights. 
# 2. Find the flights that left earliest
# 3. Sort flights to find the fastest (highest speed) flights


# Make a chart showing the fraction of canceled flights by airlines. Some tips:
# use geom_col()
# fct_reorder() may come in handy
# what is mapped to the color aesthetic?

flights %>%
  mutate(is_cancelled = is.na(arr_time)) %>%
  group_by( ) %>%
  summarise(frac_cancelled = ) %>%
  ggplot(aes(y = fct_reorder( ), x = )) + 
  geom_col()




########################### Relational Data ########################### 

# join on the airlines dataset and remake the chart
# (now with the airline's full name instead of the carrier code)

# FILL IN HERE


# 1. Make a data set called `nyc_flights` that contains for each day
# the mean and max departure delay by carrier/origin airport as well as the fraction of
# all flights delayed. Filter to carrier/day/airports that have at least 10 flights

nyc_flights <-
  flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(year, month, day, carrier, origin) %>%
  summarise(mean_delay = mean(dep_delay, na.rm = T),
            max_delay  = max(dep_delay, na.rm = T),
            frac_delay = mean(dep_delay > 0, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  filter(n >= 10)


# 2. Make a data set called `nyc_weather` that contains for each day and origin airport, 
# the mean, max, and min temp and wind speed and the total precipitation
nyc_weather <-
  weather %>% 
  group_by(year, month, day, origin) %>%
  summarise(mean_temp = mean(temp, na.rm = T),
            max_temp = max(temp, na.rm = T),
            min_temp = min(temp, na.rm = T),
            mean_wind_speed = mean(wind_speed, na.rm = T),
            max_wind_speed = max(wind_speed, na.rm = T),
            min_wind_speed = min(wind_speed, na.rm = T),
            total_precip = sum(precip, na.rm = T)) %>%
  ungroup()
# Then join these two data sets together into a data set called `nyc_weather_delays`

nyc_weather_delays <-
  nyc_flights %>%
  inner_join(nyc_weather, 
             by = join_by("year", "month", "day", "origin"))

########################### Regressions ########################### 

weather_delay_model <- 
  lm(mean_delay ~ mean_temp + max_wind_speed + total_precip,
     data = nyc_weather_delays)


weather_delay_model <- 
  nyc_weather_delays %>%
  lm(mean_delay  ~ mean_temp + max_wind_speed + total_precip,
     data = .)

summary(weather_delay_model)


# colour is set INSIDE geom_point(), so geom_smooth() never sees it:
# one regression line through all the data
nyc_weather_delays %>%
  left_join(airlines, by = "carrier") %>%
  ggplot(aes(y = mean_delay, x = total_precip)) +
  geom_point(aes(color = name), alpha = 0.2) +
  geom_smooth(method = "lm", se = F, formula = "y ~ x") +
  labs(x = "Total Precipitation",
       y = "Average Delay ",
       color = "Airline")


# now colour is also given to geom_smooth(), which splits the fit:
# one regression line per airline
nyc_weather_delays %>%
  left_join(airlines, by = "carrier") %>%
  ggplot(aes(y = mean_delay, x = total_precip)) +
  geom_point(aes(color = name), alpha = 0.2) +
  geom_smooth(aes(color = name), method = "lm", se = F, formula = "y ~ x") +
  labs(x = "Total Precipitation",
       y = "Average Delay ",
       color = "Airline")


coeftest(weather_delay_model, 
         vcov = vcovHC, 
         type = "HC1")



logit_data <-
  nyc_weather_delays %>%
  mutate(any_delay = )

logit_results <- glm( ~ , 
                      data = logit_data, 
                      family = binomial)

probit_results <- glm( ~ , 
                       data = logit_data,
                       family = binomial(link = "probit" ))

summary(logit_results)


weather_delay_model_fe <- 
  felm( ~  | , nyc_weather_delays)

summary(weather_delay_model_fe, robust = T)


# Come up with a "better" model of flight delays


