library(tidyverse)
library(scales)

depth = read_csv('data/ancillary/max_depth.csv')
max_depth<- depth%>%
  na.omit()%>%
  group_by(pond)%>%
  summarize(max_depth = max(water_depth_m))

d_sa = read_csv('data/ancillary/pond_size_mixing.csv')
plot<- d_sa%>%
  filter(paper != "NA")
  
ggplot(plot)+
  geom_point(aes(x = area_ha,y = zmax_m, fill = paper, shape = paper), size = 3)+
  scale_x_log10(labels = label_comma())+
  geom_vline(xintercept = 4.17, linetype = "dashed")+
  geom_segment(aes(x = 0, y = 0.74, xend = 4.17, yend = 0.74), linetype = "dashed")+
  ylab("Maximum depth (m)")+
  xlab("Area (ha)")+
  scale_fill_manual(values = c("darkslategray3", "skyblue4", "khaki2","black"), name = "")+
  scale_shape_manual(values = c(21, 21, 21, 20), name = "")+
  theme_bw(base_size = 9) +
  theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
ggsave('figures/mixing/comparison.png', height = 3, width = 3.5, dpi = 500, units = 'in')
