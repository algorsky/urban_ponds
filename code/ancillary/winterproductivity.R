chla_winter<-ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = chla_MicrogramsPerLiter, y = Bottom_DO), size = 2.5)+
  geom_line(aes(x = chla_MicrogramsPerLiter, y = Bottom_DO, group = pond))+
  xlab(expression(paste('Winter chl a (',mu,'g ','L'^-1, ')')))+
  # ggtitle("Turbid State")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("Winter bottom water\n  oxygen saturation (%)")+
  # scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
black<-ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = blackice, y = Bottom_DO), size = 2.5)+
  geom_line(aes(x = blackice, y = Bottom_DO, group = pond))+
  xlab('Black Ice')+
  # ggtitle("Turbid State")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("Winter bottom water oxygen saturation (%)")+
  scale_color_viridis_c()+
  # scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
white<-ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = whiteice, y = Bottom_DO), size = 2.5)+
  geom_line(aes(x = whiteice, y = Bottom_DO, group = pond))+
  xlab('White ice (cm)')+
  # ggtitle("Turbid State")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("Winter bottom water\n  oxygen saturation (%)")+
  scale_color_viridis_c()+
  # scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

DOC<-ggplot(filter(data_all, season == "winter"))+
  geom_point(aes(x = DOC, y = Bottom_DO), size = 2.5)+
  geom_line(aes(x = DOC, y = Bottom_DO, group = pond))+
  xlab(expression(paste('Winter DOC (','mg ','L'^-1, ')')))+
  # ggtitle("Turbid State")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("Winter bottom water\n  oxygen saturation (%)")+
  scale_color_viridis_c()+
  # scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

SRP<-ggplot(filter(data_all, season == "winter"))+
  geom_point(aes(x = SRP, y = Bottom_DO), size = 2.5)+
  geom_line(aes(x = SRP, y = Bottom_DO, group = pond))+
  xlab(expression(paste('Winter SRP (','mg ','L'^-1, ')')))+
  # ggtitle("Turbid State")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("Winter bottom water\n  oxygen saturation (%)")+
  scale_color_viridis_c()+
  # scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
(white+ chla_winter + SRP + DOC)+plot_annotation(tag_levels = 'A')
ggsave('figures/draft/winter_productivity.png', height = 8, width = 12, dpi = 500, units = 'in')


ggplot(filter(data_join, season == "winter"))+
  geom_point(aes(x = whiteice, y = chla_MicrogramsPerLiter, color = sampling), size = 2.5)+
  geom_line(aes(x = whiteice, y = chla_MicrogramsPerLiter, group = pond))+
  xlab('White Ice')+
  # ggtitle("Turbid State")+
  geom_hline(yintercept = 100, linetype = "dashed")+
  ylab("Winter bottom water oxygen saturation (%)")+
  scale_color_viridis_c()+
  # scale_color_manual(values = c("lightgray", "lightblue", "darkblue"), name = "Winter Sampling Event")+
  theme_bw(base_size = 16)+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
