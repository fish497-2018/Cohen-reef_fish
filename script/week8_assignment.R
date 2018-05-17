#load the packages needed
library(dplyr)
library(ggplot2)

#read in the data
reef_fish <- read.csv('data/RLSreeffishdataset.csv')
View(reef_fish)

#start looking for a trend in data
ggplot(reef_fish, aes(Depth)) +
  geom_histogram()
ggplot(reef_fish, aes(Diver, Family)) +
  geom_point()
