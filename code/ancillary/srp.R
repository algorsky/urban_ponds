library(tidyverse)
library(patchwork)
library(lubridate)
library(dplyr)

data<- read_csv( "data/ancillary/allcomparedata.csv")
srp<- read_csv( "data/nh4_no2_no3_srp/nh4_no2no3_srp.csv")|> 
  rename(DATE = date)%>%
  select(pond, DATE, NH4, NO2_NO3, SRP)

data_all<- data%>%
  left_join(srp, by = c("pond", "DATE"))
ggplot(data_all)+
  geom_point(aes(x = NH4, y = Bottom_DO, color = pond))+
  facet_wrap(~season, scales = "free")

season<- srp%>%
  group_by(season, pond)%>%
  summarize(SRP = mean(SRP),
            NH4 = mean(NH4),
            NO2_NO3 = mean(NO2_NO3))

ggplot(srp)+
  geom_point(aes(x = DATE, y = NO2_NO3, color = pond))
season_pivot<- season%>%
  select(season, pond, SRP, NO2_NO3)%>%
  pivot_wider(values_from = 3:4, names_from = season, names_glue = "{season}_{.value}")

data_join<- data%>%
  left_join(season_pivot, by = c("pond"))
ggplot(filter(data_all,season == "winter"))+
  geom_point(aes(x = NO2_NO3, y = Bottom_DO), size = 2.5)+
  geom_path(aes(x = NO2_NO3, y = Bottom_DO, group = pond))+
  ylab("Winter bottom water\n oxygen saturation (%)")+
  xlab(expression(paste('Winter NO2-NO3 (µg ','L'^-1, ')')))+
  geom_hline(yintercept = 100, linetype = "dashed")+
  # ggtitle("Macrophyte State")+
  #scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling\n       Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave('figures/draft/outliersummerNO2NO3.png', height = 8, width = 10, dpi = 500, units = 'in')
ggplot(filter(data_join,season == "winter" & summer_NO2_NO3 < 300))+
  geom_point(aes(x = summer_NO2_NO3, y = Bottom_DO), size = 2.5)+
  geom_path(aes(x = summer_NO2_NO3, y = Bottom_DO, group = pond))+
  ylab("Winter bottom water\n oxygen saturation (%)")+
  xlab(expression(paste('Summer mean NO2/NO3 (µg ','L'^-1, ')')))+
  geom_hline(yintercept = 100, linetype = "dashed")+
  # ggtitle("Macrophyte State")+
  #scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling\n       Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

srp_summer<-ggplot(filter(data_join,season == "winter"))+
  geom_point(aes(x = summer_SRP, y = Bottom_DO), size = 2.5)+
  geom_path(aes(x = summer_SRP, y = Bottom_DO, group = pond))+
  ylab("Winter bottom water\n oxygen saturation (%)")+
  xlab(expression(paste('Summer mean SRP (mg ','L'^-1, ')')))+
  geom_hline(yintercept = 100, linetype = "dashed")+
  # ggtitle("Macrophyte State")+
  #scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling\n       Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = SRP, y = Bottom_DO), size = 2.5)+
  xlab(expression(paste('Summer mean chl a (',mu,'g ','L'^-1, ')')))+
  ggtitle("Turbid State")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("Winter bottom water oxygen saturation (%)")+
  # scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
