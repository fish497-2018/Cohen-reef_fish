#load the packages needed
library(dplyr)
library(ggplot2)
library(tidyr)

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
ggplot(CG10_site, aes(Diver, Total, color = Diver)) + 
  geom_point() +
  labs(x = "Diver", y = "Total_Fish_Count")

ggplot(CG10_site, aes(Diver, color = Diver)) + 
  geom_bar() +
  labs(x = "Diver", y = "Entries")

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

ggplot(CG11_site, aes(Diver, Total, color = Diver)) + 
  geom_point() +
  labs(x = "Diver", y = "Total_Fish_Count")

ggplot(CG11_site, aes(Diver, color = Diver)) + 
  geom_bar() +
  labs(x = "Diver", y = "Entries")

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

install.packages("rmarkdown")

ggplot(CG11_site, aes(Family, Total, color = Family)) + 
  geom_point() +
  facet_wrap("Diver") +
  labs(x = "Family", y = "Total_Fish_Count", color = "Family")

ggplot(CG10_site, aes(Family, Total, color = Family)) + 
  geom_point() +
  facet_wrap("Diver") +
  labs(x = "Family", y = "Total_Fish_Count", color = "Family")

#we will make a new dataframe with tidyr to show the data in a new way
new_data <- reef_fish %>% 
  select(SiteCode, Family, Depth) %>% 
  filter(SiteCode == "CG10") %>% 
  filter(Family == "Scorpididae" | Family == "Pomacentridae" | Family == "Plesiopidae" | Family == "Enoplosidae") %>% 
  mutate("avg_Depth") %>% 
  group_by(Family) %>% summarise(avg_Depth, mean(Depth))
  
new_data2 <- reef_fish %>%
  select(SiteCode, Family, Depth) %>%
  filter(Family == "Scorpididae" | Family == "Pomacentridae" | Family == "Plesiopidae" | Family == "Enoplosidae") %>% 
  group_by(Family, SiteCode) %>%
  summarise(avg_Depth = mean(Depth)) %>%
  spread(SiteCode, avg_Depth) %>% 
  select(CG10, CG11)

  
Scorp_depth <- filter(new_data, Family == "Scorpididae") %>% 
  summarize(avg_depth = mean(Depth))
head(Scorp_depth)
Poma_depth <- filter(new_data, Family == "Pomacentridae") %>% 
  summarize(avg_depth = mean(Depth))
head(Poma_depth)
Plesi_depth <- filter(new_data, Family == "Plesiopidae") %>% 
  summarize(avg_depth = mean(Depth))
head(Plesi_depth)
Eno_depth <- filter(new_data, Family == "Enoplosidae") %>% 
  summarize(avg_depth = mean(Depth))
head(Eno_depth)

%>% 
  mutate("avg_Depth") %>% 
  |SiteCode == "CG11" 

summarise(new_data = c("Scorpididae"), avg_depth = mean(Depth))

family_depth = reef_fish(
  Site = c("CG10", "CG11"),
  Family = c("Scorpididae", "Pomacentridae", "Plesiopidae", "Enoplosidae"),
  Avg_Depth
  )
 


