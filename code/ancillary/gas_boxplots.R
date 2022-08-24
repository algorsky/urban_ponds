library(patchwork)
gas_summer = read_csv('data/gas/tidy.summer21_01.csv')
gas_winter = read_csv('data/gas/tidy.dat.out.winter22.csv')

gas_summer<- gas_summer%>%
  mutate(season = "summer")%>%
  mutate(pond = replace(pond, pond == "HP", "HP-UW"))
gas_winter<- gas_winter%>%
  mutate(season = "winter")%>%
  mutate(concentrations = dissolvedCH4 *1000000 )

#Combine
gas_seasons<- rbind(gas_summer, gas_winter)

co2<-ggplot(data = gas_seasons,aes(x = as.factor(pond), y = dissolvedCO2 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c('red4','lightblue4'))+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

ch4<-ggplot((gas_seasons), aes(x = as.factor(pond), y = dissolvedCH4 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c('red4','lightblue4'))+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

n2o<-ggplot(gas_seasons, aes(x = as.factor(pond), y = dissolvedN2O *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Nitrous Oxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c('red4','lightblue4'))+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

co2/ch4/n2o

ggsave("figures/gas_boxplot.png", width = 12, height = 10.5, units = 'in')
