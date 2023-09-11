library(tidyverse)
library(lubridate)
library(patchwork)
setwd("/Users/adriannagorsky/Documents/urban_pond_mixing/urban_ponds/")
raw = read_csv('data/tempdo/tempdo.csv') |> 
  mutate(season = case_when(month(DATE) <= 3 ~ 'winter',
                            month(DATE) > 3 ~ 'summer'))%>%
  mutate(sampling = ifelse(DATE > as.Date("2022-01-31"), "3",
                           ifelse(DATE < as.Date("2022-01-25"), "1", "2")))


ponds = unique(raw$pond)


p1 = ggplot(raw |> filter(pond %in% ponds[1:10])) +
  geom_ribbon(aes(x = Depth, ymin = 0, ymax = DO_Percent, group = DATE, fill = season), 
              alpha = 0.4) +
  #geom_hline(yintercept = 100, linetype = 2, size = 0.2) +
  scale_fill_manual(values = c('red4','lightblue4')) +
  coord_flip(expand = 0) +
  scale_x_reverse() +
  ylab('DO (%)') +
  facet_grid(pond~season, scales = 'free_y') +
  theme_bw(base_size = 9)

p2 = ggplot(raw |> filter(pond %in% ponds[11:20])) +
  geom_ribbon(aes(x = Depth, ymin = 0, ymax = DO_Percent, group = DATE, fill = season), 
              alpha = 0.4) +
  geom_hline(yintercept = 100, linetype = 2, size = 0.2) +
  scale_fill_manual(values = c('red4','lightblue4')) +
  coord_flip(expand = 0) +
  scale_x_reverse() +
  ylab('DO (%)') +
  facet_grid(pond~season, scales = 'free_y') +
  theme_bw(base_size = 9)

p1 + p2 + plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')
#ggsave('figures/profiles/pondsdo.png', height = 9, width = 6.5, dpi = 500, units = 'in')



p3 = ggplot(raw |> filter(pond %in% ponds[1:10])) +
  geom_ribbon(aes(x = Depth, ymin = 0, ymax = Conductivity, group = DATE, fill = season), 
              alpha = 0.4) +
  # geom_hline(yintercept = 100, linetype = 2, size = 0.2) +
  scale_fill_manual(values = c('red4','lightblue4')) +
  coord_flip(expand = 0, ylim = c(0,1500)) +
  scale_x_reverse() +
  ylab('Cond (µS/cm)') +
  facet_grid(pond~season, scales = 'free_y') +
  theme_bw(base_size = 9)

p4 = ggplot(raw |> filter(pond %in% ponds[11:20])) +
  geom_ribbon(aes(x = Depth, ymin = 0, ymax = Conductivity, group = DATE, fill = season), 
              alpha = 0.4) +
  # geom_hline(yintercept = 100, linetype = 2, size = 0.2) +
  scale_fill_manual(values = c('red4','lightblue4')) +
  coord_flip(expand = 0, ylim = c(0,1500)) +
  scale_x_reverse() +
  ylab('Cond (µS/cm)') +
  facet_grid(pond~season, scales = 'free_y') +
  theme_bw(base_size = 9)

p3 + p4 + plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')
ggsave('figures/profiles/pondscond.png', height = 9, width = 6.5, dpi = 500, units = 'in')

p5 = ggplot(raw |> filter(pond %in% ponds[1:10])) +
  geom_ribbon(aes(x = Depth, ymin = 0, ymax = Temp_C, group = DATE, fill = season), 
              alpha = 0.4) +
  geom_hline(yintercept = 4, linetype = 2, size = 0.2) +
  scale_fill_manual(values = c('red4','lightblue4')) +
  coord_flip(expand = 0) +
  scale_x_reverse() +
  ylab('Temp (°C)') +
  facet_grid(pond~season, scales = 'free') +
  theme_bw(base_size = 9)

p6 = ggplot(raw |> filter(pond %in% ponds[11:20])) +
  geom_ribbon(aes(x = Depth, ymin = 0, ymax = Temp_C, group = DATE, fill = season), 
              alpha = 0.4) +
  geom_hline(yintercept = 4, linetype = 2, size = 0.2) +
  scale_fill_manual(values = c('red4','lightblue4')) +
  coord_flip(expand = 0) +
  scale_x_reverse() +
  ylab('Temp (°C)') +
  facet_grid(pond~season, scales = 'free') +
  theme_bw(base_size = 9)

p5 + p6 + plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')
ggsave('figures/profiles/pondstemp.png', height = 9, width = 6.5, dpi = 500, units = 'in')


ggplot(dplyr::filter(raw, DATE < as.Date("2021-06-15")))+
  geom_point(aes(x = Temp_C, y = Depth))+
  geom_path(aes(x = Temp_C, y = Depth, group = DATE, color = (sampling)))+
  scale_y_reverse()+
  facet_wrap(~pond, scales = "free_y")+
  theme_bw()

ggplot(dplyr::filter(raw, DATE < as.Date("2021-10-15") & pond %in% "DCSW3"))+
  geom_point(aes(x = DO_Percent, y = Depth, color = DATE))+
geom_path(aes(x = DO_Percent, y = Depth, group = DATE, color = DATE))+
  scale_color_viridis_c()+
  scale_y_reverse()+
  theme_bw()

ggplot(dplyr::filter(raw, DATE < as.Date("2021-10-15") & pond %in% "DCSW1"))+
  geom_point(aes(x = Temp_C, y = Depth, color = DATE))+
  geom_path(aes(x = Temp_C, y = Depth, group = DATE, color = DATE))+
  scale_color_viridis_c()+
  scale_y_reverse()+
  theme_bw()

ggsave('figures/profiles/winter_temp.png', height = 10, width = 12, dpi = 500, units = 'in')
