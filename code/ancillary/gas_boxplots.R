library(tidyverse)
library(patchwork)
gas_summer = read_csv('data/gas/tidy.summer21_01.csv')
gas_winter = read_csv('data/gas/tidy.dat.out.winter22.csv')

gas_summer<- gas_summer%>%
  mutate(season = "summer")%>%
  mutate(pond = replace(pond, pond == "HP", "HP-UW"))%>%
  mutate(dissolvedCO2 = dissolvedCO2 *1000000)%>%
  mutate(dissolvedCH4 = dissolvedCH4 *1000000)%>%
  mutate(dissolvedN2O = dissolvedN2O *1000000)
gas_winter<- gas_winter%>%
  mutate(season = "winter")%>%
  mutate(dissolvedCO2 = dissolvedCO2 *1000000)%>%
  mutate(dissolvedCH4 = dissolvedCH4 *1000000)%>%
  mutate(dissolvedN2O = dissolvedN2O *1000000)


#Combine
gas_seasons<- rbind(gas_summer, gas_winter)
gas_seasons<- gas_seasons%>%
  mutate(flags = ifelse(dissolvedCO2 < 0, "below CO2 detection",
                        ifelse(dissolvedN2O < 0, "below N2O detection", "")))%>%
  mutate(dissolvedCO2 = ifelse(flags == "below CO2 detection", 0, dissolvedCO2))%>%
  mutate(dissolvedN2O = ifelse(flags == "below N2O detection", 0, dissolvedN2O))%>%
  rename(dissolvedCH4_microMolar = dissolvedCH4)%>%
  rename(dissolvedCO2_microMolar = dissolvedCO2)%>%
  rename(dissolvedN2O_microMolar = dissolvedN2O)

  
write_csv(gas_seasons, "data/gas/ghg_ponds.csv")

g_L<- gas_seasons%>%
  mutate(ch4 = dissolvedCH4_microMolar * 16.043)%>%
  mutate(co2 = dissolvedCO2_microMolar* 16.043)%>%
  mutate(n2o = dissolvedN2O_microMolar* 16.043)

ggplot(dplyr::filter(g_L, season == "summer"))+
  geom_point(aes(x = date, y = ch4))
ggplot(dplyr::filter(g_L, season == "winter"))+
  geom_point(aes(x = date, y = ch4, color = pond))+
  geom_line(aes(x = date, y = ch4, color = pond))

summary_gL<- g_L%>%
  group_by(season)%>%
  summarize(ch4_mean = mean(ch4),
            co2_mean = mean(co2),
            ch4_min = min(ch4),
            ch4_max = max(ch4),
            co2_min = min(co2),
            co2_max = max(co2))

ggplot(data = dplyr::filter(gas_seasons, dissolvedCO2 *1000000 < 1000),aes(x = season, y = dissolvedCO2 *1000000, fill = season),  alpha = 0.3)+
  geom_boxplot()+
  geom_point(pch = 21, position = position_jitterdodge())

co2<-ggplot(data = gas_seasons,aes(x = as.factor(pond), y = dissolvedCO2 *1000000, fill = season),  alpha = 0.3)+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'M)')))+
  xlab("")+
  scale_fill_manual(values = c('red4','lightblue4'))+
  theme_bw(base_size = 13)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 15), legend.position = "none",
        axis.text.y = element_text(size = 15))

ch4<-ggplot((gas_seasons), aes(x = as.factor(pond), y = dissolvedCH4 *1000000, fill = season), alpha = 0.3)+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Methane (',mu,'M)')))+
  xlab("")+
  scale_fill_manual(values = c('red4','lightblue4'))+
  theme_bw(base_size = 13)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 15), legend.position = "none",
        axis.text.y = element_text(size = 15))

n2o<-ggplot(gas_seasons, aes(x = as.factor(pond), y = dissolvedN2O *1000000, fill = season), alpha = 0.3)+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Nitrous Oxide (',mu,'M)')))+
  xlab("")+
  scale_fill_manual(values = c('red4','lightblue4'))+
  theme_bw(base_size = 13)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 15), legend.position = "bottom",
        axis.text.y = element_text(size = 15))



co2/ch4/n2o

ggsave("figures/ghg_boxplot.png", width = 10, height = 12, units = 'in')
