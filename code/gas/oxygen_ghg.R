library(tidyverse)
library(patchwork)
library(lubridate)
library(dplyr)

plants<- read_csv("data/ancillary/max_macrophyte.csv")

ggplot((data_winter))+
  geom_point(aes(x = diffTemp, y = Bottom_DO))+
  facet_wrap(~sampling)


options(scipen=10000)
ggplot(data_winter)+
  scale_y_continuous(trans='log10') +
  geom_point(aes(x = diffTemp, y = CH4_mean))

ggplot(data_winter, aes( x = diffTemp))+
  geom_histogram()


ggplot(filter(data_join))+
  geom_point(aes(x = Bottom_DO, y = log(CH4_mean)))+
  facet_wrap(~season)+
  theme_bw()
options(scipen=10000)

do_CH4<-ggplot(data_winter)+
  geom_point(aes(x = Bottom_DO, y = (CH4_mean)), size = 2.5)+
  geom_path(aes(x = Bottom_DO, y = (CH4_mean), group = pond))+
  xlab("Winter bottom water oxygen saturation (%)")+
  geom_vline(xintercept = 100, linetype = "dashed")+
  scale_y_continuous(trans='log10') +
  ylab(expression(paste('Winter dissolved methane (',mu,'mol ','L'^-1, ')')))+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
do_CO2<-ggplot(data_winter)+
  geom_point(aes(x = Bottom_DO, y = (CO2_mean)), size = 2.5)+
  geom_path(aes(x = Bottom_DO, y = (CO2_mean), group = pond))+
  xlab("Winter bottom water oxygen saturation (%)")+
  geom_vline(xintercept = 100, linetype = "dashed")+
  ylab(expression(paste('Winter dissolved carbon dioxide (',mu,'mol ','L'^-1, ')')))+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

do_CH4 + do_CO2 + plot_annotation(tag_levels = 'A')
ggsave('figures/draft/winter_DO_GHG.png', height = 5, width = 11, dpi = 500, units = 'in')
