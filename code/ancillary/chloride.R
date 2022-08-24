library(tidyverse)
library(lubridate)


chloride = read_csv('data/ancillary/chloride.csv')|> 
  mutate(season = case_when(month(date) <= 3 ~ 'winter',
                            month(date) > 3 ~ 'summer'))

ponds = unique(chloride$pond)


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
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_alpha(guide = 'none')

ggsave("figures/chloride.png", width = 12, height = 10.5, units = 'in')

