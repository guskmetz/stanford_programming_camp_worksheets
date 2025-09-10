# Stanford Econ PhD Programming Camp Day 2
# This is a script file to help you follow along

## load libraries
library(readr)
library(ggplot2)
library(dplyr)


# import data
babynames <- read_csv(file = "data/babynames.csv")

# inspect the data
babynames  

# what happens if we plot the data?

ggplot(babynames) +
  geom_line(mapping = aes(x = year, y = prop))


ggplot(babynames, aes(x = year, y = prop)) +
  geom_line()





# try a different geom?


# select

select(babynames)


biblical_names <- c("Abraham", "Sarah")

# make a plot of the number of babies born with your name over time

my_name_data <- filter(babynames, name == "Shifra")

ggplot(data = my_name_data, 
       mapping = aes(x = year, y = n)) + 
  geom_point() + 
  geom_line() +
  labs(x = "Year",
       y = "Number of Babies")

ggplot() 













# filter
babynames_mary <- filter(babynames, name == "Mary")

babynames_mary_sarah <- filter(babynames, name %in% c("Mary", "Sarah"))


# make a plot of your name's popularity over time (or linguistically similar name)
ggplot(data = babynames_mary, aes(x = year, y = prop)) + 
  geom_line()  +
  facet_wrap("sex")

ggplot(data = babynames_mary_sarah, aes(x = year, y = prop, color = sex, linewidth = name)) + 
  geom_line() 

# which name had the highest ever number/proportion of babies? 
# HINT: use the max() function wisely
filter(babynames, prop == max(prop))


# use boolean operators to return only rows that contain:
# 1. Boys names Leslie
ggplot(filter(babynames, name == "Barbie"), aes(x = year, y = n)) + 
  geom_line()

# 2. Names that were used by exactly 5 or 6 children in 1880
filter(babynames, 
       year == 1880, 
       n == 5 | n == 6)

filter(babynames, 
       year == 1880, 
       n %in% seq(5, 10, by = 0.5))



filter(babynames, 
       year == 1880, 
       n <= 10)

filter(babynames, 
       year == 1880, 
       n %in% c(5, 6))


# 3. Names that are one of Anakin, Leia, Luke

filter(babyname, name == "Sea" | name == "Anemone")

# Why is the name Shifrah not in babynames?

arrange(babynames, n)

# Goal: create a data set called top_5_M_2015 that contains the 5 most 
#popular boy baby names from 2015

names_2015 <- filter(babynames, year == 2015, sex == "M")

ordered_names_2015 <- arrange(names_2015, -n)

top_5_M_2015 <- head(ordered_names_2015)

# Pipes

# Using pipes make a plot of your name's popularity over time

babynames %>%
  filter(name == "Shifra") %>%
  ggplot(aes(x = year, y = n, color = sex)) +
  geom_line()





# summarise()

babynames %>%
  summarise(total = , 
            max = )

# group_by()

babynames %>%
  group_by(sex) %>%
  summarise(total = sum(n))

# 1. For each year calculate the number of distinct names by sex. Plot these time series.
babynames %>%
  group_by(year, sex) %>%
  summarise(n = n_distinct(name)) %>%
  ggplot(aes(x = year, y = n, color = sex)) + 
  geom_line()


# 2. Plot the share of babies with a name among the top 10 names over time by sex
babynames %>%
  group_by(year, sex) %>%
  arrange(-n) %>%
  slice_head(n = 10) %>%
  summarise(frac = sum(prop)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = frac, color = sex), data  = .) + 
  geom_line()


# plot the time series of number of babies born with the top 6 names of all time by sex
# each name it's own plot
top_6_names <-
  babynames %>%
  group_by(sex) %>%
  arrange(-n) %>%
  distinct(sex, name) %>%
  slice_head(n = 6)


babynames %>%
  filter(name %in% top_6_names$name) %>%
  ggplot(aes(x = year, y = n, color = sex)) + 
  geom_line() +
  facet_wrap("name")


# mutate()
babynames %>%
  mutate(percent = round(prop * 100, 2))


# What names experience a dramatic rise/fall in popularity?


# Names that flash in the pan

# Step 1: find the names that experience a collapse in popularity
# 1. Filter to names that have at least 1% babies with that name (for a sex)
# 2. For each name calculate the percent change in prop over time
# 3. Take the top 10 most dramatic collapses over time
# Step 2: plot the popularity over time for those names


at_least_500_babies <-
  babynames %>%
  group_by(name, year) %>%
  summarise(n = sum(n)) %>%
  group_by(name) %>%
  summarise(max = max(n)) %>%
  ungroup() %>%
  filter(max > 500) %>%
  pull(name)


switching_names <-
  babynames %>%
  filter(name %in% at_least_500_babies,
         n > 500) %>%
  group_by(name, year) %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  count(name, sex) %>%
  filter(n %in% 60:70) %>%
  pull(name)

babynames %>%
  filter(name %in% switching_names) %>%
  ggplot(aes(x = year, y = n, color = sex)) + 
  geom_line() + 
  facet_wrap("name", scales = "free")


# Tidy data

table1
table2
table3
table4a
table4b

