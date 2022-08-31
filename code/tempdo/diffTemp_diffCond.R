library(tidyverse)
library(patchwork)
library(lubridate)
library(rLakeAnalyzer)
library(dplyr)

#Import data and add season column
temp_do = read_csv('data/tempdo/tempdo.csv') |> 
  mutate(season = case_when(month(DATE) <= 3 ~ 'winter',
                            month(DATE) > 3 ~ 'summer'))
#Calculate Max/Min dataset
max<- temp_do%>%
  group_by(pond, DATE)%>%
  summarize(Depth = max(Depth))
min<- temp_do%>%
  group_by(pond, DATE)%>%
  summarize(Depth = min(Depth))
max_profile<- max%>%
  left_join(temp_do, by = c("pond", "DATE", "Depth"))%>%
  select(pond, DATE, Temp_C, Conductivity, season, Depth)%>%
  rename(Bottom_Temp = Temp_C)%>%
  rename(Bottom_Cond = Conductivity)

min_profile<- min%>%
  left_join(temp_do, by = c("pond", "DATE", "Depth"))%>%
  select(pond, DATE, Temp_C, Conductivity, season)%>%
  rename(Top_Temp = Temp_C)%>%
  rename(Top_Cond = Conductivity)

profile<- merge(max_profile, min_profile, by = c("pond", "DATE", "season"))%>%
  mutate(diffCond = Top_Cond - Bottom_Cond)%>%
  mutate(diffTemp = Top_Temp - Bottom_Temp)

#write_csv(profile, "data/ancillary/min_max.csv")

sampling<- profile%>%
  group_by(pond, season)%>%
  summarize(sampling = n_distinct(DATE))

thermo<- profile%>%
  group_by(season)%>%
  summarize(n = n_distinct(diffCond, diffTemp, DATE),
            temp = sum(abs(diffTemp) > 1),
            cond = sum(diffCond < -550))%>%
  mutate(percTemp = (temp/n)*100)%>%
  mutate(percCond = (cond/n)*100)

summerDiff<-ggplot(dplyr::filter(profile, season == "summer"))+
  geom_point(aes(x = diffTemp, y = diffCond, color = Depth), size = 3, alpha = 0.7)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_colour_viridis_c(name = "Max Sampling \n Depth (m)")+
  scale_shape(name = "Month")+
  ylim(-1200, 200)+
  geom_hline(yintercept = -551, linetype = "dashed")+
  annotate("text", y = -400, x = 10, label = "Thermo", size = 6)+
  annotate("text", y = -1000, x = 10, label = "Chemo/Thermo", size = 6)+
  ggtitle("Summer")+
  theme_bw()

winterDiff<-ggplot(dplyr::filter(profile, season == "winter" & diffCond > -1500))+
  geom_point(aes(x = diffTemp, y = diffCond, color = Depth),  size = 3, alpha = 0.7)+
  geom_vline(xintercept = -1, linetype = "dashed")+
  scale_colour_viridis_c(name = "Max Sampling \n Depth (m)")+
  scale_shape(name = "Month")+
  ylim(-1200, 200)+
  geom_hline(yintercept = -551, linetype = "dashed")+
  annotate("text", y = -400, x = -3, label = "Thermo", size = 6)+
  annotate("text", y = -1000, x = -3, label = "Chemo/Thermo", size = 6)+
  ggtitle("Winter")+
  theme_bw()
library(ggpubr)
ggarrange(summerDiff, winterDiff, ncol = 2, common.legend= TRUE, legend = "bottom")
ggsave('figures/profiles/diffTemp_diffCond.png', height = 6, width = 10, dpi = 500, units = 'in')


#Calculate buoyancy frequency per pond per sampling day
all_bf<- list()
pond_names = unique(temp_do$pond)
bf<- function(temp_do, pond_names){
  for(pond_name in pond_names){
    pond_data<- temp_do%>%
      filter(pond == pond_name)%>%
      mutate(depth = paste('wtr_',Depth, sep = ""))%>%
      select(DATE, depth, Temp_C)%>%
      pivot_wider(names_from = depth, values_from = Temp_C)
    
    
    pond_bf<-ts.buoyancy.freq(pond_data, na.rm = TRUE)
    pond_bf<- pond_bf%>%
      mutate(pond = pond_name)
    all_bf[[pond_name]]<-  pond_bf
  }
  return(all_bf)
}

all_bf<- bf(temp_do, pond_names)
all_bf_df<- do.call(rbind.data.frame, all_bf)|> 
  mutate(season = case_when(month(DATE) <= 3 ~ 'winter',
                            month(DATE) > 3 ~ 'summer'))
ggplot(all_bf_df)+
  geom_point(aes(x = DATE, y = n2, color = pond))+
  facet_wrap(~season, scales = "free")


#Trying regression of bf to diff temperature
all_bf_ponds<- all_bf_df%>%
  mutate(log_n2 = log(n2))%>%
  select(pond, log_n2, n2)

regression<- merge(profile, all_bf_df, by = c("pond"))

ggplot(regression)+
  geom_point(aes(x = diffTemp, y = n2, color = pond))


#Density Profiles
density_profile<- temp_do%>%
  mutate(wtr = Temp_C)%>%
  mutate(dens = water.density(wtr))|> 
  mutate(season = case_when(month(DATE) <= 3 ~ 'winter',
                            month(DATE) > 3 ~ 'summer'))

ggplot(dplyr::filter(density_profile, pond == "ALC"))+
  geom_point(aes(x = dens, y = Depth))+
  geom_path(aes(x = dens, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  facet_wrap(~season, scales = "free")+
  theme_bw()
  

#Density Gradient between top and bottom (Bottom - Top/Depth)
layers_temp<-read_csv("data/ancillary/layer_temp.csv")
dens_water<- layers_temp%>%
  mutate(dens = water.density(wtr))%>%
  select(pond, DATE, season, Depth, layer, dens)%>%
  pivot_wider(names_from = layer, values_from = dens)%>%
  mutate(diffDens = abs(bottom-top)/Depth)

#Holgerson et al. 2022 cutoff was 0.287
ggplot(dplyr::filter(dens_water, season == "summer"))+
  geom_point(aes(x = DATE, y = diffDens))+
  geom_line(aes(x = DATE,y = diffDens, group = pond))+
  ylab(expression(paste("Density gradient (kg ", m^-3, m^-1,")")))+
  geom_hline(yintercept = 0.287, linetype = "dashed")+
  facet_wrap(~pond)+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/dg_summer.png", width = 10, height = 8, units = 'in')

#Winter literature doesn't use one cutoff (Gray et al. 2020- 0.05 in the middle)
ggplot(dplyr::filter(dens_water, season == "winter"))+
  geom_point(aes(x = DATE, y = diffDens))+
  geom_line(aes(x = DATE,y = diffDens, group = pond))+
  ylab(expression(paste("Density gradient (kg ", m^-3, m^-1,")")))+
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  facet_wrap(~pond)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/dg_winter.png", width = 10, height = 8, units = 'in')

pond_wd<- temp_do%>%
  filter(pond == pond_name)%>%
  mutate(depth = paste('wtr_',Depth, sep = ""))%>%
  select(DATE, depth, Temp_C)%>%
  pivot_wider(names_from = depth, values_from = Temp_C)




