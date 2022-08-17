library(tidyverse)
library(patchwork)

temp_do = read_csv('data/tempdo/tempdo.csv') |> 
  mutate(season = case_when(month(DATE) <= 3 ~ 'winter',
                            month(DATE) > 3 ~ 'summer'))


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

summerDiff + winterDiff + plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

ggsave('figures/profiles/diffTemp_diffCond.png', height = 6, width = 10, dpi = 500, units = 'in')

ALC_wider<- buoyancy_ALC%>%
  pivot_wider(names_from = Depth, values_from = Temp_C)