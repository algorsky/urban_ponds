# Load packages
library(vegan)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)
library(ggtext)


ponds<- read_csv("data/ancillary/update_data.csv")
setNames(c('Summer Bottom DO','Summer Surface DO','Summer DOC', "Summer Chl a", "Summer SRP",
           'Floating Macrophyte','Winter Bottom DO','Winter Surface DO','Winter DOC', "Winter SRP",
           "Winter Chl a", "White Ice"))

df.dobottom<- ponds%>%
  filter(season == "winter")%>%
  select(pond, Bottom_DO)

df.dobottom.wide<- df.dobottom%>%
  group_by(pond)%>%
  mutate(row = row_number())%>%
  pivot_wider(names_from = pond,
              values_from = Bottom_DO)%>%
  select(-row)

ponds_winter<- ponds%>%
  filter(season == "winter")%>%
  select(pond, Bottom_DO, Top_DO, DOC, SRP, whiteice, chla_MicrogramsPerLiter)%>%
  mutate(row = row_number())%>%
  pivot_wider(names_from = pond,
              values_from = Bottom_DO)%>%
  select(-row)







#PCA Attempt 1
ponds<- read_csv("data/ancillary/summary_allponds.csv")
summary_allponds<- ponds[,c(1,2,3, 13,15, 18, 19,22,23,33,37:39)]
row.names(summary_allponds)<- summary_allponds$pond

myPR <- prcomp(summary_allponds[,-1], scale = TRUE)
biplot(myPR, scale = 0)

#Extract PC scores
ponds2<- cbind(summary_allponds, myPR$x[,1:2])
head(summary_allponds)

#PLOT
ggplot(ponds2, aes(PC1, PC2, col = pond, fill = pond))+
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5)+
  geom_point(shape = 21, col = "black")
