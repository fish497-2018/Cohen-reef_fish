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
ggplot(reef_fish, aes(Diver, Total)) +
  geom_point() +
  facet_wrap("Family")

#by site
ggplot(reef_fish, aes(Total)) +
  geom_histogram() +
  facet_wrap("Site")

CG10_site <- select(reef_fish, SiteCode, Diver, Family, Total) %>% 
  filter(SiteCode == "CG10")

#create a graph with the new data set
ggplot(CG10_site, aes(Diver, Total)) + 
  geom_point() 

ggplot(CG10_site, aes(Diver, Total, color = Diver)) + 
  geom_point() +
  facet_wrap("Family")

