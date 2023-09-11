library(tidyverse)
library(patchwork)
library(lubridate)
library(dplyr)
library(ggpmisc)
data<- read_csv( "data/ancillary/allcomparedata.csv")

summary_alldata_summer<- read_csv("data/ancillary/means/summaryall_mean.csv")|>
  filter(season == "summer")%>%
  select(pond, chla, DOC, secchi_m, RTRM, ch4, co2)%>%
  rename(chla_summer = chla)%>%
  rename(DOC_summer = DOC)%>%
  rename(secchi_summer = secchi_m)%>%
  rename(RTRM_summer = RTRM)%>%
  rename(CH4_summer = ch4)%>%
  rename(CO2_summer = co2)

summary_alldata_winter<- read_csv("data/ancillary/means/summaryall_mean.csv")|>
  filter(season == "winter")%>%
  select(pond, chla, DOC, secchi_m, RTRM, ch4, co2)%>%
  rename(chla_winter = chla)%>%
  rename(DOC_winter = DOC)%>%
  rename(secchi_winter = secchi_m)%>%
  rename(RTRM_winter = RTRM)%>%
  rename(CH4_winter = ch4)%>%
  rename(CO2_winter = co2)

data_join<- data%>%
  left_join(summary_alldata_summer, by = "pond")%>%
  left_join(summary_alldata_winter, by = "pond")%>%
  mutate(sampling = ifelse(month(DATE) == 2, "3", 
                           ifelse(DATE < as.Date("2022-01-18") & year(DATE) == 2022, "1",
                                  ifelse(DATE > as.Date("2022-01-18") & DATE < as.Date("2022-02-01"),
                                         "2", "0"))))
ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = chla_summer, y = Bottom_DO), size = 2)+
  xlab('chla')+
  ylab("Winter bottom water oxygen saturation (%)")+
  #  scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(filter(data_join, season == "winter" & sampling > 1))+
  geom_point(aes(x = (blackice/totice)*100, y = chla_winter, size = Bottom_DO))+
  xlab('Black Ice (%)')+
  ylab("Winter Chlorophyll a")+
  #  scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = Bottom_DO, y = chla_MicrogramsPerLiter))+
  geom_path(aes(x = Bottom_DO, y = chla_MicrogramsPerLiter, group = pond))
ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = Bottom_Temp, y = chla_MicrogramsPerLiter))+
  geom_path(aes(x = Bottom_Temp, y = chla_MicrogramsPerLiter, group = pond))

ggplot(filter(data_join, season == "winter" & sampling > 1))+
  geom_point(aes(x = blackice/totice, chla_MicrogramsPerLiter, size = Bottom_DO, color = sampling))+
  xlab('Black Ice (%)')+
  ylab(expression(paste('Chl a (',mu,'g ','L'^-1, ')')))+
  scale_color_manual(values = c("lightblue", "darkblue"), name = "Winter Sampling\n Event")+
  scale_size_continuous(name = "Winter bottom water\n oxygen saturation (%)")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave('figures/draft/winter_DO_chla_ice.png', height = 4, width = 6, dpi = 500, units = 'in')
ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = chla_winter, y = secchi_m, color = pond))+
  ylab('Secchi (m)')+
  xlab("Chlorophyll a")+
  #  scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave('figures/draft/secchi_chla.png', height = 8, width = 6, dpi = 500, units = 'in')
ggplot(data)+
  geom_point(aes(x = DATE, y = secchi_m, color = pond))+
  geom_line(aes(x = DATE, y = secchi_m, group = pond, color = pond))+
  facet_wrap(~season, scales = "free")


ggplot(data, aes(x = season, y = secchi_m))+
  geom_boxplot()

ggplot(data, aes(x = pond, y = secchi_m, fill = season))+
  geom_boxplot()


ggplot(filter(data, season == "summer"))+
  geom_point(aes(y = Bottom_DO, x = DOC))+
  facet_wrap(~season)

ggplot(filter(data, season == "winter"))+
  geom_point(aes(y = Bottom_DO, x = DOC))+
  facet_wrap(~season)


ggplot(filter(data, season == "summer"))+
  geom_point(aes(y = Bottom_DO, x = chla_MicrogramsPerLiter))+
  facet_wrap(~season)

ggplot(filter(data, season == "winter"))+
  geom_point(aes(y = Bottom_DO, x = chla_MicrogramsPerLiter))+
  facet_wrap(~season)

profile<-read_csv("data/ancillary/min_max.csv")
anoxia<- profile%>%
  group_by(season, pond)%>%
  summarize(n_days = n(),
    anoxia_top = sum(Top_DOmgL < 1),
    anoxia_bottom = sum(Bottom_DOmgL < 1))

ggplot(filter(profile, season == "winter"), aes(x = Bottom_DO, y = Top_DO))+
  geom_point(size = 3)+
  stat_poly_line(se = FALSE, color = "black") +
  ggtitle("Winter")+
  stat_poly_eq(use_label("R2"), size = 8) +
  ylab("Surface water oxygen saturation (%)")+
  xlab("Bottom water oxygen saturation (%)")+
  stat_poly_eq(use_label("R2"), size = 8) +
  theme_bw(base_size = 14)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

