library(tidyverse)
library(devtools)
library(lubridate)
library(gridExtra)
library(patchwork)
library(scales)
library(patchwork)


## Load the data from Github
# Read in data 
gas_summer = read_csv('data/gas/tidy.summer21_01.csv')
gas_summer<- gas_summer%>%
  mutate(season = "summer")%>%
  mutate(pond = replace(pond, pond == "HP", "HP-UW"))

gas_winter = read_csv('data/gas/winter22/tidy.dat.out.winter22.csv')
gas_winter<- gas_winter%>%
  mutate(season = "winter")

gas_seasons<- rbind(gas_summer, gas_winter)
extra = read_csv('data/ancillary/extra.csv')
size = read_csv('data/ancillary/pond_size.csv')

depth<- extra%>%
  group_by(pond)%>%
  summarize(depth = mean(Zmax))

gas_seasons<-gas_seasons%>%
  left_join(depth, by = "pond")%>%
  mutate(pond = fct_reorder(pond, depth))

gd <- gas_seasons %>% 
  group_by(pond, season) %>%
  summarise(
    n = n(),
    CO2_mean = mean(dissolvedCO2)*1000000,
    CH4_mean = mean(dissolvedCH4)*1000000,
    N2O_mean = mean(dissolvedN2O)*1000000,
    CO2_sd = sd(dissolvedCO2)*1000000,
    CH4_sd = sd(dissolvedCH4)*1000000,
    N2O_sd = sd(dissolvedN2O)*1000000)
  

co2_plot<-ggplot(dplyr::filter(gas_seasons, pond != "KP"), aes(x = as.factor(pond), y = dissolvedCO2 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

co2_kp<-ggplot(dplyr::filter(gas_seasons, pond == "KP"), aes(x = as.factor(pond), y = dissolvedCO2 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

co2_figure<-co2_plot + co2_kp+ plot_layout(ncol=2,widths=c(4,1))

ch4_plot<-ggplot(dplyr::filter(gas_seasons, pond != "KP"), aes(x = as.factor(pond), y = dissolvedCH4 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

ch4_kp<-ggplot(dplyr::filter(gas_seasons, pond == "KP"), aes(x = as.factor(pond), y = dissolvedCH4 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

ch4_figure<-ch4_plot + ch4_kp + plot_layout(ncol=2,widths=c(4,1))

n2o_plot<-ggplot(gas_seasons, aes(x = as.factor(pond), y = dissolvedN2O *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Nitrous Oxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")


CO2_plot<-ggplot() + 
  geom_bar(data = gd, aes(x = as.factor(pond), y = CO2_mean, fill = season, alpha = 0.3), stat = "identity", position = "dodge")  +
  geom_point(data = gas_seasons,(aes(x = as.factor(pond), y = dissolvedCO2 *1000000)))+
  #geom_errorbar(aes(ymin=CO2_mean-CO2_sd, ymax=CO2_mean+CO2_sd), position = position_dodge(0.9), width = 0.25)+
  xlab("")+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 18)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
  
CH4_plot<-ggplot(gd, aes(x = as.factor(pond), y = CH4_mean, fill = season)) + 
  geom_bar(stat = "identity", position = "dodge")  +
  geom_errorbar(aes(ymin=CH4_mean-CH4_sd, ymax=CH4_mean+CH4_sd), position = position_dodge(0.9), width = 0.25)+
  xlab("")+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 18)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

N2O_plot<-ggplot(gd, aes(x = as.factor(pond), y = N2O_mean, fill = season)) + 
  geom_bar(stat = "identity", position = "dodge")  +
  geom_errorbar(aes(ymin=N2O_mean-N2O_sd, ymax=N2O_mean+N2O_sd), position = position_dodge(0.9), width = 0.25)+
  xlab("")+
  ylab(expression(paste('Dissolved Nitrous Oxide (',mu,'mol ','L'^-1, ')')))+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 18)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
  


plot<- co2 /ch4 /n2o

ggsave("figures/summer22gas.png", width = 12, height = 10.5, units = 'in', plot)

ggsave("figures/jasm/n20.png", width = 8, height = 6, units = 'in', n2o_plot)
ggsave("figures/jasm/co2.png", width = 10, height = 6, units = 'in', co2_figure)
ggsave("figures/jasm/ch4.png", width = 10, height = 6, units = 'in', ch4_figure)

