library(tidyverse)
library(devtools)
library(lubridate)
library(gridExtra)
library(patchwork)
library(scales)


## Load the data from Github
# Read in data 
gas = read_csv('data/gas/winter22/tidy.dat.out.winter22.csv')

gas<- gas%>%
  mutate(sampling = ifelse(day(date) %in% 12:14, 1,
                           ifelse(day(date) %in% 27:31, 2,
                                  ifelse(day(date) %in% 9:11, 3, NA))))

gd <- gas %>% 
  group_by(pond, date, sampling) %>%
  summarise(
    n = n(),
    CO2_mean = mean(dissolvedCO2)*1000000,
    CH4_mean = mean(dissolvedCH4)*1000000,
    N2O_mean = mean(dissolvedN2O)*1000000,
    CO2_sd = sd(dissolvedCO2)*1000000,
    CH4_sd = sd(dissolvedCH4)*1000000,
    N2O_sd = sd(dissolvedN2O)*1000000)
        
ggplot(dplyr::filter(gd, pond != "KP"))+
  geom_point(aes(x = as.factor(sampling), y = CO2_mean), color = "blue2")+
  geom_path(aes(x = as.factor(sampling), y = CO2_mean, group = pond), color = "blue2")+
  xlab("")+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  theme_bw(base_size = 12)
ggplot(dplyr::filter(gd, pond != "KP"))+
  geom_point(aes(x = as.factor(sampling), y = CH4_mean), color = "blue2")+
  geom_path(aes(x = as.factor(sampling), y = CH4_mean, group = pond), color = "blue2")+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  theme_bw(base_size = 12)
  #facet_wrap(~pond)

geom_point(aes(x = as.factor(date), y = dissolvedCH4*1000000), color = "red")+
  geom_point(aes(x = as.factor(date), y = dissolvedN2O*1000000), color = "green")+

gas$date = as.Date(gas$date, format =  "%m/%d/%y")
gas$doy = yday(gas$date)

gd <- gas %>% 
  group_by(pond) %>%
  summarise(
    CO2 = mean(dissolvedCO2)*1000000,
    CH4 = mean(dissolvedCH4)*1000000,
    N2O = mean(dissolvedN2O)*1000000
  )

averages<- gas %>%
  filter(dissolvedCH4 > 0)%>%
  filter(dissolvedCO2 > 0)%>%
  filter(dissolvedN2O > 0)%>%
  group_by(pond)%>%
  summarise(
    CO2 = mean(dissolvedCO2)*1000000,
    CH4 = mean(dissolvedCH4)*1000000,
    N2O = mean(dissolvedN2O)*1000000
  )

co2<-ggplot(gas)+
  stat_summary(aes(x = pond, y = dissolvedCO2 *1000000), fun = mean, geom = "bar")+
  stat_summary(aes(x = pond, y = dissolvedCO2 *1000000), fun.data = mean_se, geom = "errorbar")+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ch4<-ggplot(gas)+
  stat_summary(aes(x = pond, y = dissolvedCH4 *1000000), fun = mean, geom = "bar")+
  stat_summary(aes(x = pond, y = dissolvedCH4 *1000000), fun.data = mean_se, geom = "errorbar")+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

n2o<- ggplot(gas)+
  stat_summary(aes(x = pond, y = dissolvedN2O *1000000), fun = mean, geom = "bar")+
  stat_summary(aes(x = pond, y = dissolvedN2O *1000000), fun.data = mean_se, geom = "errorbar")+
  ylab(expression(paste('Dissolved Nitrous Oxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


library(patchwork)
plot<- co2 /ch4 /n2o

ggsave("figures/summer22gas.png", width = 12, height = 10.5, units = 'in', plot)

extra = read_csv('data/extra.csv')
size = read_csv('data/pond_size.csv')

coverage<- extra%>%
  group_by(pond_id)%>%
  summarize(depth = mean(Zmax),
            emergent = mean(Emergent))

explore<- cbind(averages, coverage, size)

ggplot(explore)+
  geom_point(aes(x = SA_ha, y = N2O))
