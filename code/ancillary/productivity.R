library(tidyverse)
library(patchwork)
library(lubridate)
library(dplyr)

data<- read_csv( "data/ancillary/allcomparedata.csv")
summary_alldata<- read_csv("data/ancillary/means/summaryall_mean.csv")|>
  filter(season == "summer")%>%
  select(pond, chla, DOC, RTRM)

data_join<- data%>%
  left_join(summary_alldata, by = "pond")%>%
  mutate(sampling = ifelse(month(DATE) == 2, 8, 
                           ifelse(DATE < as.Date("2022-01-18") & year(DATE) == 2022, 6,
                                  ifelse(DATE > as.Date("2022-01-18") & DATE < as.Date("2022-02-01"),
                                         7,
                                         ifelse(DATE < as.Date("2021-06-12"), 1,
                                                ifelse(DATE > as.Date("2021-06-12") & DATE < as.Date("2021-06-28"), 2,
                                                       ifelse(DATE > as.Date("2021-06-28") & DATE < as.Date("2021-07-11"), 3,
                                                              ifelse(DATE > as.Date("2021-07-11") & DATE < as.Date("2021-07-28"), 4, 5))))))))

chla_DO<-ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = chla, y = Bottom_DO), size = 2.5)+
  geom_line(aes(x = chla, y = Bottom_DO, group = pond))+
  xlab(expression(paste('Summer mean chl a (',mu,'g ','L'^-1, ')')))+
 # ggtitle("Turbid State")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("Winter bottom water oxygen saturation (%)")+
 # scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
chla_CO2<-ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = chla, y = CO2_mean, color = as.factor(sampling)), size = 3)+
  xlab(expression(paste('Summer mean chl a (',mu,'g ','L'^-1, ')')))+
  ylab(expression(paste('Dissolved CO2 (',mu,'mol ','L'^-1, ')')))+
  scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
chla_CH4<-ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = chla, y = CH4_mean, color = sampling), size = 3)+
  xlab(expression(paste('Summer mean chl a (',mu,'g ','L'^-1, ')')))+
  ylab(expression(paste('Dissolved CH4 (',mu,'mol ','L'^-1, ')')))+
  scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
chla_DO/chla_CO2/chla_CH4 + plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')
ggsave('figures/draft/summer_winterchla.png', height = 10, width = 6, dpi = 500, units = 'in')

doc_do<-ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = DOC.y, y = Bottom_DO), size = 2.5)+
  geom_path(aes(x = DOC.y, y = Bottom_DO, group = pond))+
  xlab(expression(paste('Summer mean DOC (mg ','L'^-1, ')')))+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("")+
  ylab("Winter bottom water\n oxygen saturation (%)")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = DOC.y, y = Top_DO), size = 2)+
 # geom_smooth(aes(x = DOC.y, y = Bottom_DO), method = "lm", se = FALSE)+
  xlab(expression(paste('Summer mean DOC (mg ','L'^-1, ')')))+
  ylab("Winter surface water oxygen saturation (%)")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = DOC.x, y = (CO2_mean)), size = 2)+
  # geom_smooth(aes(x = DOC.y, y = Bottom_DO), method = "lm", se = FALSE)+
  xlab(expression(paste('Summer mean DOC (mg ','L'^-1, ')')))+
  ylab("Winter surface water oxygen saturation (%)")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

doc_co2<-ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = DOC.y, y = CO2_mean, color = sampling), size = 3)+
  xlab(expression(paste('Summer mean DOC (mg ','L'^-1, ')')))+
  ylab(expression(paste('Dissolved CO2 (',mu,'mol ','L'^-1, ')')))+
  scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling\n Event")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
doc_ch4<-ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = DOC.y, y = CH4_mean, color = sampling), size = 3)+
  xlab(expression(paste('Summer mean DOC (mg ','L'^-1, ')')))+
  ylab(expression(paste('Dissolved CH4 (',mu,'mol ','L'^-1, ')')))+
  scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling\n Event")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

(doc_do/doc_co2/doc_ch4) + plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')
ggsave('figures/draft/winter.y_summer.x.png', height = 4, width = 8, dpi = 500, units = 'in')


chla_DO + doc_do+ plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')


ggplot(filter(data_join))+
  geom_point(aes(x = secchi_m, y = chla_MicrogramsPerLiter))+
  facet_wrap(~season, scales = "free")+
  theme_bw()
ggplot(filter(data_join))+
  geom_point(aes(x = Top_DO, y = Bottom_DO))+
  facet_wrap(~season, scales = "free")+
  theme_bw()
ggplot(filter(data_join))+
  geom_point(aes(x = Top_Temp, y = Bottom_Temp))+
  facet_wrap(~season, scales = "free")+
  theme_bw()

ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = Bottom_DO, y = (blackice/totice)*100))+
  facet_wrap(~sampling)+
  theme_bw()
ggplot(filter(data_join))+
  geom_point(aes(x = Bottom_DO, y = log(CH4_mean)))+
  facet_wrap(~season)+
  theme_bw()

ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = chla, y = Bottom_DO, size = (log(CH4_mean))))+
  xlab(expression(paste('Summer mean chl a (',mu,'g ','L'^-1, ')')))+
  ylab("Winter bottom water oxygen saturation (%)")+
  geom_hline(yintercept = 100, linetype = "dashed")+
 # scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling\n       Event")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = DOC.x, y = Bottom_DO,  size = log(CH4_mean)))+
  xlab(expression(paste('Summer mean DOC (mg ','L'^-1, ')')))+
  ylab("Winter bottom water oxygen saturation (%)")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  #scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling\n       Event")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



options(scipen=10000)
ggplot(data_join)+
  geom_point(aes(x = Bottom_DO, y = (CH4_mean)))+
  scale_y_continuous(trans='log10') +
  geom_vline(xintercept = 100, linetype = "dashed")+
  facet_wrap(~season)+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data_join)+
  geom_point(aes(x = Depth, y = (CH4_mean)))+
  scale_y_continuous(trans='log10') +
  facet_wrap(~season)+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data_join)+
  geom_point(aes(x = Top_DO, y = (CO2_mean)))+
  scale_y_continuous(trans='log10') +
  geom_vline(xintercept = 100, linetype = "dashed")+
  facet_wrap(~season)+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


ggplot(filter(data_join, season == "summer"))+
  geom_point(aes(x = Floating, y = chla_MicrogramsPerLiter))

options(scipen=10000)
chla_DO_log<-ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = chla, y = Bottom_DO), size = 2.5)+
  geom_line(aes(x = chla, y = Bottom_DO, group = pond))+
  xlab(expression(paste('Summer mean chl a (',mu,'g ','L'^-1, ')')))+
  scale_x_continuous(trans='log10') +
  #ggtitle("Turbid State")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("Winter bottom water\n oxygen saturation (%)")+
  # scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


macrophyte + chla_DO_log + srp_summer + doc_do +plot_annotation(tag_levels = 'A')
ggsave('figures/draft/summer_productivity.png', height = 8, width = 12, dpi = 500, units = 'in')
