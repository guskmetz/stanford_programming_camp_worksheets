# Stanford Econ PhD Programming Camp Day 1
# This is a script file to help you follow along

print("hello world")

42 + 1

log(pi)

LETTERS


############# NUTS AND BOLTS ############# 

# object assignment

x <- 42
y <- "abc"

z <- c(x,
       y)

really_long_and_painful_to_type <- 42

help(seq)

seq(1, 10, 1)

seq(from = 1,
    to = 10,
    by = 1)

## Your turn!

# Create a sequence of even numbers from 1 to 10 
# and assign it to an object called `even`
even <- seq(2, 10, 2)

# Create a sequence of odd numbers from 1 to 10 
# in two ways: using `seq()` and using your already 
# created object `even`

odd <- even - 1


# Use `sum()` and `mean()` to calculate the average value
sum(odd)
mean(odd)

# fill in to code here to install and load libraries
# best practice is to put this at the top of your code file
install.packages("ggplot2")
install.packages("AER")

library(ggplot2)
library(AER)


############# GRAMMAR OF GRAPHICS ############# 

# load in data set
data("CigarettesSW")

# inspect the data
head(CigarettesSW)
tail(CigarettesSW)
summary(CigarettesSW)
help("CigarettesSW")


# building a plot of cigarette taxes vs consumption
ggplot()

ggplot(, aes(x = , y = )) 


ggplot(CigarettesSW, aes(x = , y = , color = )) + 
  geom_point()


ggplot() + 
  geom_point() +
  labs(x = "Average Excise Tax",
       y = "Per Capita Cigarette Consumption (Packs)",
       color = "Year",
       title = "Excise Taxes and Cigarette Consumption")

# How would you change this code so that different years 
# are represented by different shapes


# Try changing the plot so that instead of points 
# each observation is represented by the state abb `geom_text()`
# what are the aesthetics for geom_text?
ggplot(data = CigarettesSW, aes(x = tax, y = packs, label = state)) + 
  #geom_text() +
  labs(x = "Average Excise Tax",
       y = "Per Capita Cigarette Consumption (Packs)",
       color = "Year",
       title = "Excise Taxes and Cigarette Consumption") +
  theme_economist()



# Install and load the package `ggthemes`
install.packages("ggthemes")
library(ggthemes)

# Try **adding** a theme to your plot such as `theme_stata()` or
# `theme_economist()`


# How has the distribution of taxes changed over time?
ggplot(data = CigarettesSW, 
       aes(x = tax, fill = year),
) + 
  geom_histogram(position = "identity", 
                 alpha = 0.5,
                 bins = 10) 

ggplot() + 
  geom_histogram() +
  labs(x = "Average Excise Tax",
       y = "Count(States)",
       fill = "Year",
       title = "Distribution of Excise Taxes over Time")

ggplot(CigarettesSW, aes(x = tax, fill = year)) + 
  geom_density(alpha = 0.5, adjust = 2) +
  labs(x = "Average Excise Tax",
       y = "Count(States)",
       fill = "Year",
       title = "Distribution of Excise Taxes over Time")
+ 
  scale_x_log10()


# Bar Plots!
data(BankWages)

bw <- BankWages

ggplot(data = bw, aes(x = job, fill = gender)) +
  geom_bar(position = "fill") +
  labs(x = "Job",
       y = "Count",
       fill = "Gender")

# Faceting

ggplot(data = bw, aes(x = job, fill = minority)) +
  geom_bar() +
  labs(x = "Job",
       y = "Count",
       fill = "Minority") + 
  facet_wrap("gender")

# Save a chart
p_minority_gender <-
  ggplot(data = bw, aes(x = job, fill = factor(education))) +
  geom_bar() +
  labs(x = "Job",
       y = "Count",
       fill = "Minority") + 
  facet_wrap("gender")

p_minority_gender

ggsave(p_minority_gender, filename = "test.pdf",
       width = 6.5, height = 4, units = "in")
