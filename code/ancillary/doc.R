library(tidyverse)
library(patchwork)
library(lubridate)
library(dplyr)

data<- read_csv( "data/ancillary/allcomparedata.csv")

ggplot(data)+
  geom_point(aes(x = Bottom_DO, y = DOC))+
  geom_vline(xintercept = 100, linetype = "dashed")+
  ylab("Dissolved Organic Carbon (mg/L)")+
  xlab("Bottom water oxygen saturation (%)")+
  facet_wrap(~season)+  
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())
#ggsave('figures/draft/doc_o2.png', height = 6, width = 12, dpi = 500, units = 'in')

ggplot(data)+
  geom_point(aes(x = DOC, y = CO2_mean))+
  xlab("Dissolved Organic Carbon (mg/L)")+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  facet_wrap(~season)+  
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())
#ggsave('figures/draft/doc_co2.png', height = 6, width = 12, dpi = 500, units = 'in')

co2<-ggplot(filter(data, pond!= "KP"))+
  geom_point(aes(x = Top_DO, y = CO2_mean, color = season), size = 2, alpha = 0.5)+
  geom_vline(xintercept = 100, linetype = "dashed", alpha = 0.5)+
  xlab("Surface water oxygen saturation (%)")+
  scale_color_manual(values = c("darkgreen", "blue"))+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())
ch4<-ggplot(filter(data, pond!= "KP"))+
  geom_point(aes(x = Top_DO, y = CH4_mean, color = season),size = 2, alpha = 0.5)+
  geom_vline(xintercept = 100, linetype = "dashed", alpha = 0.5)+
  scale_color_manual(values = c("darkgreen", "blue"))+
  xlab("Surface water oxygen saturation (%)")+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())
ch4 + co2 + plot_layout(guides = 'collect')
ggsave('figures/draft/gas_o2.png', height = 6, width = 12, dpi = 500, units = 'in')

ggplot((data_winter))+
  geom_point(aes(x = Top_DO, y = DIC, color = CH4_mean))+
  scale_color_viridis_c()+
  facet_wrap(~pond)

ggplot(data, aes(x = pond, y = DIC,fill = season,))+
  geom_boxplot()+
  theme_bw()

ggplot(filter(data_winter, sampling == 2), aes(x = max_depth_m, y = totice))+
  geom_point()+
  stat_poly_line(se = FALSE) +
  stat_poly_eq(use_label("R2"), size = 8) +
  theme_bw()
