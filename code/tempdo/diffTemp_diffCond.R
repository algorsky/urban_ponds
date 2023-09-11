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
  select(pond, DATE, Temp_C, Conductivity, DO_mgL, DO_Percent, season, Depth)%>%
  rename(Bottom_Temp = Temp_C)%>%
  rename(Bottom_Cond = Conductivity)%>%
  rename(Bottom_DO = DO_Percent)%>%
  rename(Bottom_DOmgL = DO_mgL)

min_profile<- min%>%
  left_join(temp_do, by = c("pond", "DATE", "Depth"))%>%
  select(pond, DATE, Temp_C, Conductivity,DO_mgL, DO_Percent, season)%>%
  rename(Top_Temp = Temp_C)%>%
  rename(Top_Cond = Conductivity)%>%
  rename(Top_DO = DO_Percent)%>%
  rename(Top_DOmgL = DO_mgL)

profile<- merge(max_profile, min_profile, by = c("pond", "DATE", "season"))%>%
  mutate(diffCond = Top_Cond - Bottom_Cond)%>%
  mutate(diffTemp = Top_Temp - Bottom_Temp)

write_csv(profile, "data/ancillary/min_max.csv")

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
#ggsave('figures/profiles/diffTemp_diffCond.png', height = 6, width = 10, dpi = 500, units = 'in')


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
#ggsave("figures/dg_summer.png", width = 10, height = 8, units = 'in')

#Winter literature doesn't use one cutoff (Gray et al. 2020- 0.05 in the middle)
ggplot(dplyr::filter(dens_water, season == "winter"))+
  geom_point(aes(x = DATE, y = diffDens))+
  geom_line(aes(x = DATE,y = diffDens, group = pond))+
  ylab(expression(paste("Density gradient (kg ", m^-3, m^-1,")")))+
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  facet_wrap(~pond)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#ggsave("figures/dg_winter.png", width = 10, height = 8, units = 'in')



df<- read_csv('data/stratification/df_calc.csv')
carbon<- read_csv('data/ancillary/carbon.csv')
carbon<- carbon%>%
  rename(DATE = date)

data<- left_join(profile, df, by = c("pond", "DATE"))
write_csv(data, "data/ancillary/allcomparedata.csv")
data<- data%>%
  mutate(chla_MicrogramsPerLiter = ifelse(chla_MicrogramsPerLiter == 1632.8, 632.8, chla_MicrogramsPerLiter))
data<- left_join(data, carbon, by = c("pond", "DATE"))

write_csv(data, "data/ancillary/allcomparedata.csv")

ggplot(data)+
  geom_point(aes(x = diffTemp, y = (maxN2), color = as.factor(month(DATE))))+
  facet_wrap(~season + pond, scales = "free")

ggplot(filter(data, season == "summer"))+
  geom_point(aes(x = diffTemp, y = (maxN2), color = as.factor(month(DATE))))+
  geom_smooth(aes(x = diffTemp, y = maxN2), method = "lm")+
  facet_wrap(~as.factor(month(DATE)))

ggplot(filter(data, season == "winter"))+
  geom_point(aes(x = diffTemp, y = (maxN2), color = as.factor(month(DATE))))+
  geom_smooth(aes(x = diffTemp, y = maxN2), method = "lm")

ggplot(data)+
  geom_point(aes(x = pond, y = Schmidt))+
  facet_wrap(~season , scales = "free")

ggplot(filter(data, season == "summer"))+
  geom_point(aes(x = DATE, y = densGrad),position = "jitter")+
  geom_line(aes(x = DATE, y = densGrad))+
  geom_hline(yintercept = 0.287, linetype = "dashed")+
  scale_color_viridis_c()+
  facet_wrap(~pond)+
  ylab('Density gradient (kg/m3/m)') +
  theme_bw()

depth_m <- d_sa%>%
  filter(paper == "This study")%>%
  dplyr::select(waterbody, zmax_m)%>%
  rename(pond = waterbody)
data<- data%>%
  left_join(depth_m, by = "pond")

data<- data%>%
  mutate(bottom_dens = calc_dens(Bottom_Temp))%>%
  mutate(top_dens = calc_dens(Top_Temp))%>%
  mutate(RTRM = (bottom_dens - top_dens)/(calc_dens(4)-calc_dens(5)))

ggplot(filter(data, season == "summer"))+
  geom_point(aes(x = DATE, y = RTRM))+
  facet_wrap(~season, scales = "free")+
  geom_hline(yintercept = 50, linetype = "dashed")+
  theme_bw()

ggplot(filter(data, season == "summer"))+
  geom_point(aes(x = DATE, y = densGrad, color = (zmax_m)), position = "jitter")+
  geom_hline(yintercept = 0.287, linetype = "dashed")+
  scale_color_viridis_c()+
  ylab('Density gradient (kg/m3/m)') +
  theme_bw()

ggplot(filter(data, season == "summer"))+
  geom_point(aes(x = diffTemp, y = (maxN2), color = as.factor(month(DATE))))+
  geom_smooth(aes(x = diffTemp, y = maxN2), method = "lm")+
  facet_wrap(~as.factor(month(DATE)))

sum(data$densGrad > 0.287)
densGrad_strat<- data%>%
  group_by(season == "summer")%>%
  summarize(sum(densGrad> 0.287))
densGrad_strat<- data%>%
  group_by(season == "summer")%>%
  summarize(sum(RTRM> 50))

sum(data$season == "summer")

ggplot(filter(data, season == "winter"))+
  geom_point(aes(x = DATE, y = densGrad, color = pond))+
  facet_wrap(~season, scales = "free")+
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  theme_bw()


ggplot(filter(data, season == "summer"))+
  geom_point(aes(x = DATE, y = maxN2, color = pond))+
  theme_bw()

winter_mixing<- data%>%
  filter(season == "winter")%>%
  mutate(strat = ifelse(densGrad < 0.05, "mixing", "stratified"))%>%
  mutate(sampling = ifelse(month(DATE) == 2, "3", 
                           ifelse(DATE < as.Date("2022-01-18"), "1", "2")))


ggplot(winter_mixing)+
  geom_point(aes(x = DATE, y = Bottom_DO))+
  geom_path(aes(x = DATE, y = Bottom_DO, group = pond, color = pond))

df_char <- read.csv('data/ancillary/data.csv') 
df_char<-df_char%>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))%>%
  rename(DATE = date)

winter_mixing<- winter_mixing%>%
  left_join(df_char, by = join_by(DATE, pond))

data<- data%>%
  left_join(df_char, by = join_by(DATE, pond))

winter_summary<- winter_mixing%>%
  group_by(pond)%>%
  summarize(mean_ratio = mean((blackice/totice) *100),
            mean_Bottom_DO = mean(Bottom_DO))
ggplot(data_winter)+
  geom_point(aes(x = (blackice/totice)*100, y = Bottom_DO, color = sampling, size = sampling))+
  scale_color_manual(values = c("gray", "lightblue", "darkblue"), name = "Sampling Event")+
  scale_size_manual(values = c(1, 2, 2), name = "Sampling Event")+
  xlab("Black ice (%)")+
  ylab("Bottom Dissolved Oxygen Saturation (%)")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave('figures/draft/DO_IceRatio.png', height = 5, width = 8, dpi = 500, units = 'in')

ggplot((winter_mixing))+
  geom_point(aes(x = (blackice/totice)*100, y = Bottom_DO, color = max_depth_m))+
  geom_hline(yintercept = 100, linetype = "dashed")+
  scale_color_viridis_c()+
  ylab("Bottom Dissolved Oxygen Saturation (%)")+
  facet_wrap(~strat)+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())


ggplot(dplyr::filter(winter_mixing, pond != "KP"))+
  geom_point(aes(x = CH4_mean, y = Bottom_DO, color = strat))+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("Bottom Dissolved Oxygen Saturation (%)")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

ggplot(dplyr::filter(data, season == "summer"))+
  geom_point(aes(y = CH4_mean, x = RTRM, color = season))+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())


ggsave('figures/draft/DO_Ice.png', height = 5, width = 10, dpi = 500, units = 'in')

data_winter<- data%>%
  filter(season == "winter")%>%
  mutate(sampling = ifelse(month(DATE) == 2, "3", 
                           ifelse(DATE < as.Date("2022-01-18"), "1", "2")))
data_summer<- data%>%
  filter(season == "summer")

DO_RTRM<-ggplot(data_summer)+
  geom_point(aes(x = Bottom_DO, y = RTRM))+
  geom_hline(yintercept = 50, linetype = "dashed")+
  xlab("Bottom water oxygen saturation (%)")+
  scale_color_viridis_c()+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())


ggplot(data_summer)+
  geom_point(aes(x = Top_Temp, y = RTRM))+
  geom_hline(yintercept = 50, linetype = "dashed")+
  geom_smooth(aes(x = Top_Temp, y = RTRM))+
  scale_color_viridis_c()+
  theme_bw()
ggplot(data_summer, aes(x = month(DATE), y = RTRM, group = month(DATE)))+
  geom_boxplot()+
  theme_bw()
Depth_RTRM<-ggplot(average_summer)+
  geom_point(aes(x = depth, y = RTRM))+
  geom_smooth(aes(x = depth, y = RTRM), method = "lm", se = F, color = "black")+
  xlab("Water depth (m)")+
  ylab("RTRM")+
  theme_bw(base_size = 12)+
  geom_text(x = 1.1, y = 185, label = eq(average_summer$depth, average_summer$RTRM), parse = TRUE, size=7, family = "serif")+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())


Depth_RTRM<-ggplot(data = average_summer, aes(x = depth, y = RTRM)) +
  stat_poly_line(se = FALSE, color = "black") +
  stat_poly_eq(use_label("R2"), size = 8) +
  geom_point()+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())



eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(~~(r)^2~"="~r2,
                 list(
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

Depth_RTRM + DO_RTRM + plot_annotation(tag_levels = 'A')
ggsave('figures/draft/RTRM.png', height = 5, width = 10, dpi = 500, units = 'in')

average_summer<- data_summer%>%
  group_by(pond)%>%
  summarize(RTRM = mean(RTRM), 
            depth = mean(zmax_m))

average_winter<- data_winter%>%
  group_by(pond)%>%
  mutate(diffWinter = Bottom_Temp - Top_Temp)%>%
  mutate(inverse = ifelse(diffWinter > 1.9, "stratified", "mixed"))

ggplot(average_winter)+
  geom_point(aes(x = Bottom_DO_perc, y = diffWinter), size = 2)+
  xlab("Bottom water oxygen saturation (%)")+
  ylab("Bottom - Top Water Temp")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

ggsave('figures/draft/comparisons/gradient_DO.png', height = 6, width = 8, dpi = 500, units = 'in')
ggplot((average_winter))+
  geom_point(aes(x = Bottom_Temp, y = (blackice/totice)*100))

ggplot(average_winter)+
  geom_point(aes(x = inverse, y = Bottom_Cond))+
  theme_bw()
ggplot(data_winter)+
  geom_point(aes(x = max_depth_m, y = totice))+
  #geom_smooth( method = "lm", se = FALSE,aes(x = max_depth_m, y = totice, group = sampling), color = "black")+
  facet_wrap(~sampling, scales = "free_y")+
  theme_bw()

ggplot(data_winter)+
  geom_point(aes(x = max_depth_m, y = totice, color = sampling))+
 #geom_smooth( method = "lm", se = FALSE,aes(x = max_depth_m, y = totice, group = sampling), color = "black")+
  theme_bw()
ggplot(filter(data, season =="winter"))+
  geom_point(aes(x = DATE, y = blackice/totice))+
  geom_path(aes(x = DATE, y = blackice/totice, group = pond))+
  facet_wrap(~pond)

ggplot(filter(data))+
  geom_point(aes(x = chla_MicrogramsPerLiter, y = secchi_m, color = season))+
 # facet_wrap(~season, scales = "free")+
  theme_bw()

ggplot(filter(data))+
  geom_point(aes(x = chla_MicrogramsPerLiter, y = RTRM, color = season))+
  # facet_wrap(~season, scales = "free")+
  theme_bw()

cv <- function(x) 100*( sd(x)/mean(x))
data_cv<- data%>%
  select(-pond, -DATE, -Depth, -avsnow, -totice, -blackice, -whiteice, -CO2_sd, -CH4_sd, -N2O_sd, 
         -max_depth_m)%>%
  group_by(season) %>% 
  summarise(across(everything(), list(cv = cv)))

winter_mix<- winter_mixing%>%
  select(pond, DATE, strat)
df_winter_mix<- df_winter%>%
  select(pond, DATE, type)
raw = read_csv('data/tempdo/tempdo.csv') |> 
  mutate(season = case_when(month(DATE) <= 3 ~ 'winter',
                            month(DATE) > 3 ~ 'summer'))%>%
  mutate(sampling = ifelse(DATE > as.Date("2022-01-31"), "3",
                           ifelse(DATE < as.Date("2022-01-25"), "1", "2")))
tempdo<- raw%>%
  left_join(winter_mix, by = c("pond", "DATE"))%>%
  left_join(df_winter_mix, by = c("pond", "DATE"))

ggplot(filter(tempdo, season == "winter"))+
  geom_point(aes(x = Temp_C, y = Depth, color = strat), size = 2)+
  geom_path(aes(x = Temp_C, y = Depth, group = DATE, color = (strat)))+
  scale_y_reverse()+
  scale_color_discrete(name = "Stratification")+
  facet_wrap(~pond, scales = "free_y")+
  theme_bw()
ggsave('figures/draft/twodegrees.png', height = 10, width = 12, dpi = 500, units = 'in')

ggplot(filter(tempdo, season == "winter"))+
  geom_point(aes(x = Temp_C, y = Depth, color = type), size = 2)+
  geom_path(aes(x = Temp_C, y = Depth, group = DATE, color = (type)))+
  scale_y_reverse()+
  facet_wrap(~pond, scales = "free_y")+
  theme_bw()
ggsave('figures/draft/cryo.png', height = 10, width = 12, dpi = 500, units = 'in')
ggplot(filter(tempdo, season == "winter"))+
  geom_point(aes(x = DO_Percent, y = Depth, color = strat))+
  geom_path(aes(x = DO_Percent, y = Depth, group = DATE, color = (strat)))+
  scale_y_reverse()+
  facet_wrap(~pond, scales = "free_y")+
  theme_bw()

