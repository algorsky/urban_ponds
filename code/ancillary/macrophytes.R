library(tidyverse)
library(patchwork)
library(lubridate)
library(dplyr)

data<- read_csv( "data/ancillary/allcomparedata.csv")
plants<- read_csv("data/ancillary/extra_macrophyte.csv")

data_join<- data%>%
  left_join(plants, by = "pond")%>%
  mutate(sampling = ifelse(month(DATE) == 2, 8, 
                           ifelse(DATE < as.Date("2022-01-18") & year(DATE) == 2022, 6,
                                  ifelse(DATE > as.Date("2022-01-18") & DATE < as.Date("2022-02-01"),
                                         7,
                                         ifelse(DATE < as.Date("2021-06-12"), 1,
                                                ifelse(DATE > as.Date("2021-06-12") & DATE < as.Date("2021-06-28"), 2,
                                                       ifelse(DATE > as.Date("2021-06-28") & DATE < as.Date("2021-07-11"), 3,
                                                              ifelse(DATE > as.Date("2021-07-11") & DATE < as.Date("2021-07-28"), 4, 5))))))))

winter_summary<- data_join%>%
  filter(season == "winter")%>%
  group_by(pond)%>%
  summarize(floating = mean(Floating),
            Bottom_DO = mean(Bottom_DO),
            Top_DO = mean(Top_DO), 
            ch4 = mean(CH4_mean),
            co2 = mean(CO2_mean))
data_winter<- data_join%>%
  filter(season == "winter")%>%
  mutate(sampling = ifelse(sampling == 8, 3, 
                           ifelse(sampling == 6, 1, 2)))
ggplot(winter_summary)+
  geom_point(aes(x = Top_DO, y = log(ch4)))+
  theme_bw()

floating<- plants%>%
  select(pond, Floating)

macrophyte<- ggplot(data_winter)+
  geom_point(aes(x = Floating, y = Bottom_DO), size = 2.5)+
  geom_path(aes(x = Floating, y = Bottom_DO, group = pond))+
  #ylab("Winter bottom water oxygen saturation (%)")+
  ylab("")+
  xlab("Summer max floating\n macrophyte coverage (%)")+
  geom_hline(yintercept = 100, linetype = "dashed")+
 # ggtitle("Macrophyte State")+
  #scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling\n       Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data_join)+
  geom_point(aes(x = Top_DO, y = Bottom_DO))+
  facet_wrap(~season, scales = "free")
