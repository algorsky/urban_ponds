library(tidyverse)
library(lubridate)


chloride = read_csv('data/ancillary/chloride.csv')|> 
  mutate(season = case_when(month(date) <= 3 ~ 'winter',
                            month(date) > 3 ~ 'summer'))

ponds = unique(chloride$pond)


ggplot(chloride)+
  geom_point(aes(x = date, y = chloride_mgL))+
  facet_wrap(~season, scales = "free")


p1 = ggplot(chloride |> filter(pond %in% ponds[1:10])) +
  geom_point(aes(x = date, y = chloride_mgL, color = season)) +
  scale_color_manual(values = c('red4','lightblue4')) +
  ylab('Chloride (mg/L)') +
  facet_grid(pond~season, scales = 'free_x') +
  theme_bw(base_size = 9)

p2 = ggplot(chloride|> filter(pond %in% ponds[11:20])) +
  geom_point(aes(x = date, y = chloride_mgL, color = season)) +
  scale_color_manual(values = c('red4','lightblue4')) +
  ylab('Chloride (mg/L)') +
  facet_grid(pond~season, scales = 'free_x') +
  theme_bw(base_size = 9)

p1 + p2 + plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')


ggplot(chloride, aes(x = as.factor(pond), y = chloride_mgL, fill = season, alpha = 0.3))+
  geom_boxplot()+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Chloride (mg ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("red4", "lightblue4"))+
  theme_bw(base_size = 9)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_alpha(guide = 'none')

ggplot(gas_seasons, aes(x = as.factor(pond), y = dissolvedN2O *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Nitrous Oxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

ggplot(chloride |> filter(pond %in% ponds[1:10])) +
  geom_boxplot(aes(x = season, y = chloride_mgL, color = season)) +
  scale_color_manual(values = c('red4','lightblue4')) +
  ylab('Chloride (mg/L)') +
  facet_grid(~pond, scales = 'free') +
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