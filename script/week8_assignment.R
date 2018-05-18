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

CG10_site_large <- select(reef_fish, SiteCode, Diver, Family, Total) %>% 
  filter(SiteCode == "CG10") %>% 
  filter(Total > 7)

#create a graph with the new data set
ggplot(CG10_site, aes(Diver, Total)) + 
  geom_point() +
  labs(x = "Diver", y = "Total_Fish_Count")

ggplot(CG10_site_large, aes(Diver, Total)) + 
  geom_point() +
  labs(x = "Diver", y = "Total_Fish_Count")

ggplot(CG10_site, aes(Diver, Total, color = Diver)) + 
  geom_point() +
  facet_wrap("Family") +
  labs(x = "Diver", y = "Total_Fish_Count", color = "Divers")

ggplot(CG10_site_large, aes(Diver, Total, color = Diver)) + 
  geom_point() +
  facet_wrap("Family") +
  labs(x = "Diver", y = "Total_Fish_Count", color = "Divers")

#Another site to see how it compares
CG11_site <- select(reef_fish, SiteCode, Diver, Family, Total) %>% 
  filter(SiteCode == "CG11") 

CG11_site_large <- select(reef_fish, SiteCode, Diver, Family, Total) %>% 
  filter(SiteCode == "CG11") %>% 
  filter(Total > 7)

ggplot(CG11_site, aes(Diver, Total, color = Diver)) + 
  geom_point() +
  facet_wrap("Family") +
  labs(x = "Diver", y = "Total_Fish_Count", color = "Divers")

ggplot(CG11_site_large, aes(Diver, Total, color = Diver)) + 
  geom_point() +
  facet_wrap("Family") +
  labs(x = "Diver", y = "Total_Fish_Count", color = "Divers")

