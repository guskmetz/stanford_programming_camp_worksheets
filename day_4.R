# Stanford Econ PhD Programming Camp Day 4
# This is a script file to help you follow along

## load libraries
library(tidyverse)
library(nycflights13)
library(tictoc)
library(furrr)
library(repurrrsive)

#################### Control flow #################### 


x <- 8

if (x >= 10) {
  
  print("x is greater than or equal to 10")
  
}

if (x >= 10) {
  
  print("x is greater than or equal to 10")
  
} else {
  
  print("x is less than 10")
  
}


if (x >= 10) {
  
  print("x is greater than or equal to 10")
  
} else if (x > 5) {
  
  print("x is greater than 5, but less than 10")
  
} else {
  
  print("x is less than 5")
  
}


# Write a sequence of `if`, `else`, and `ifelse` that will correctly return the result of a rock - paper - scissors game given the values of 
# 
# * `player_1` is one of `c("rock", "paper", "scissors")` 
# * `player_2` is one of `c("rock", "paper", "scissors")`
# 
# use the `sample()` function to generate random values for `player_1` and `player_2` to check your code


for (i in 1:10) {
  print(i)
}


for (i in 1:5) {
  for (j in c('a', 'b', 'c', 'd', 'e')) {
    print(paste(i,j))
  }
}


output_vector <- c()

tic()
for (i in 1:1000) {
  for (j in c('a', 'b', 'c', 'd', 'e')) {
    temp_output <- paste(i, j)
    output_vector <- c(output_vector, temp_output)
  }
}
toc()



output_matrix <- matrix(nrow = 1000, ncol = 5)
j_vector <- c('a', 'b', 'c', 'd', 'e')

tic()
for (i in 1:1000) {
  for (j in 1:5) {
    temp_j_value <- j_vector[j]
    temp_output <- paste(i, temp_j_value)
    output_matrix[i, j] <- temp_output
  }
}
toc()



z <- 1

while(z > 0.1){
  z <- runif(1)
  cat(z, "\n")
}







nyc_flights <-
  flights %>%
  group_by(carrier, year, month, day, origin) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = T),
            max_dep_delay = max(dep_delay, na.rm = T),
            frac_delayed = sum(dep_delay > 0, na.rm = T) / n(),
            frac_cancelled = sum(is.na(dep_time )) / n(),
            n = n()) %>%
  filter(n >= 10)


nyc_weather <-
  weather %>%
  group_by(year, month, day, origin) %>% 
  summarise(temp   = mean(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE),
            wind   = mean(wind_speed, na.rm = TRUE),
            max_wind = max(wind_speed, na.rm = TRUE),
            precip = sum(precip, na.rm = TRUE))

nyc_weather_delays <-
  nyc_flights %>%
  inner_join(nyc_weather, by = c("year", "month", "day", "origin"))


#################### Functions #################### 

fahr_to_cel <- function(temp) {
  cel <- ((temp - 32) * (5 / 9))
  return(cel)
}

fahr_to_cel(32)
fahr_to_cel(100)


# write a function `rock_paper_scissors()` that "plays" rock paper scissors
# 
# * The inputs should be what `player_1` and `player_2` played
# * It should return the winner


# Convert your for loop for extracting betas to a function where the input is the iteration number
# 
# It should return a tibble with the beta and the iteration number

betas <-
  tibble(n = 1:100,
         beta = 0)


for(i in 1:100){
  temp_model <-
    lm(dep_delay ~ precip,
       sample_frac(nyc_weather_delays, 0.25, replace = T)) %>%
    broom::tidy()
  betas$beta[i] <- temp_model$estimate[2]
}


betas %>%
  ggplot(aes(x = beta)) + 
  geom_histogram()

#################### Functionals #################### 


calc_beta <- function(n){
  temp_model <-
    lm(dep_delay ~ precip,
       sample_frac(nyc_weather_delays, 0.25, replace = T)) %>%
    broom::tidy()
  return(tibble(beta = temp_model$estimate[2],
                n = n))
}


calc_beta(1)


tic()
for(i in 1:10){
  beta(i)
}
toc()

tic()
map_dfr(1:10, beta)
toc()
# How many star ships has Luke been in?



# Replace `map()` with appropriately typed function

# How many starships has each character been in?
map_(sw_people, ~ length(.x$starships))

# What color is each character's hair?
map_(sw_people, ~.x[["hair_color"]])


# Is the character male?
map_(sw_people, ~.x[["gender"]] == "male")



# for each element extract the named/numbered element
map_chr(sw_people, "hair_color")

map(sw_people, "starships") %>% map_int(length)


# Use map to run your function 100 times 


#################### Parallel Computing #################### 


plan(multisession, workers = 2)

future_map(c("hello", "world"), ~.x)

tic()
nothingness <- future_map(c(2, 2, 2), ~Sys.sleep(.x))
toc()

# What are the speed gains from parallelizing your bootstrap procedure?
tic()
future_map_dfr(1:10, calc_beta)
toc()



