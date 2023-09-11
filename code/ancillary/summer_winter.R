library(tidyverse)
library(patchwork)
library(lubridate)
library(dplyr)

data<- read_csv( "data/ancillary/allcomparedata.csv")
plants<- read_csv("data/ancillary/extra_macrophyte.csv")
dan<- read_csv("data/ancillary/dan_data.csv")
summary_alldata<- data%>%
  group_by(pond, season)%>%
  summarize(bottom_do = mean(Bottom_DO, na.rm = TRUE),
            top_do = mean(Top_DO, na.rm = TRUE),
            densGrad = mean(densGrad, na.rm = TRUE),
            RTRM = mean(RTRM, na.rm = TRUE),
            zmax_m = mean(max_depth_m),
            chloride_mgL = mean(chloride_mgL, na.rm = TRUE),
            sulfate_mgL = mean(sulfate_mgL, na.rm = TRUE),
            secchi_m = mean(secchi_m, na.rm = TRUE),
            TSS_gL = mean(TSS_gL, na.rm = TRUE),
            POM_gL = mean(POM_gL,na.rm = TRUE),
            OM_perc = mean(OM_perc, na.rm = TRUE),
            DOC = mean(DOC, na.rm = TRUE),
            DIC = mean(DIC, na.rm = TRUE),
            chla = mean(chla_MicrogramsPerLiter, na.rm = TRUE),
            co2 = mean(CO2_mean, na.rm = TRUE),
            ch4 = mean(CH4_mean, na.rm = TRUE))
#write_csv(summary_alldata, "data/ancillary/means/summaryall_mean.csv")
pH = read_csv('data/ancillary/pH.csv')%>%
  mutate(season = case_when(month(DATE) <= 3 ~ 'winter', month(DATE) > 3 ~ 'summer'))%>%
  mutate(sampling = ifelse(DATE > as.Date("2022-01-31"), "3",
                           ifelse(DATE < as.Date("2022-01-25"), "1", "2")))
#write_csv(pH_mean, "data/ancillary/means/ph_mean.csv")
pH_mean<- pH%>%
  na.omit()%>%
  group_by(pond, season)%>%
  summarise(tot_pH = (mean(pH, na.rm = T)))
cond_mean<- raw%>%
  group_by(pond, season)%>%
  summarise(tot_cond = (mean(Conductivity, na.rm = T)))

data_combine<- data_combine%>%
  left_join(cond_mean, by = c("pond", "season"))

library(readr)
dan_roads<- dan%>%
  filter(Round == 4)%>%
  select(Site.Code, Developed200m, Developed500m, Developed1000m, Surface.Heterogeneity, Sub.Heterogeneity, Perc.Submergent, Perc.Total.Floating)%>%
  rename(pond = Site.Code)
data_combine<- data%>%
  left_join(dan_roads, by = c("pond"))

dan_summary<- dan%>%
  filter(Year == 2020)%>%
  group_by(Site.Code)%>%
  summarize(Emergent = mean(Perc.Emergent, na.rm = TRUE),
            Floating = mean(Perc.Total.Floating, na.rm = TRUE),
            Open = mean(Perc.Open.Water, na.rm = TRUE),
            Submergent = mean(Perc.Submergent, na.rm = TRUE),
            pH = mean(pH))
#write_csv(dan_summary, "data/ancillary/means/dan_mean.csv")

ggplot(data_combine, aes(x = chloride_mgL, y = tot_cond))+
  geom_point(aes(color = season), size = 2)+
 # stat_poly_line(se = FALSE, color = "lightgray") +
  #stat_poly_eq(use_label("R2"), size = 8) +
  ylab('Mean water column conductivity (µS/cm)') +
  xlab("Surface chloride (mg/L)")+
  theme_bw(base_size = 12)
ggsave('figures/draft/cond_chloride.png', height = 6, width = 8, dpi = 500, units = 'in')

ggplot(filter(data_combine, !is.na(chloride_mgL)))+
  geom_point(aes(x = Developed200m, y = chloride_mgL, color = pond))+
  facet_wrap(~season, scales = "free_x")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())
ggplot(filter(data_combine, !is.na(chloride_mgL)))+
  geom_point(aes(x = Developed200m, y = chloride_mgL, color = season))+
  geom_smooth(aes(x = Developed200m, y = chloride_mgL, group = season, color = season), method = "glm")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

ggplot(filter(data_combine, !is.na(TSS_gL)& TSS_gL < 0.1))+
  geom_point(aes(x = Developed200m, y = TSS_gL, color = season))+
  geom_smooth(aes(x = Developed200m, y = TSS_gL, group = season, color = season), method = "glm")+
  theme_bw(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

  
ggplot(filter(data, season == "summer" & !is.na(chloride_mgL)))+
  geom_point(aes(x = DATE, y = chloride_mgL))+
  geom_path(aes(x = DATE, y = chloride_mgL, group = pond))+
  theme_bw()


ggplot(dan)+
  geom_point(aes(x = Site.Code, y = Developed1000m))

ggplot(summary_alldata)+
  geom_point(aes(x = DOC, y = ch4, color = season))

winter_summer<- summary_alldata%>%
  select(-zmax_m)%>%
  pivot_wider(names_from = season, values_from = c(3:17), names_glue = "{season}_{.value}")

chla_relation<- ggplot(winter_summer,aes(x = summer_chla, y = winter_chla))+
  geom_point(size = 3)+
  ylab("Winter")+
  xlab("Summer")+
  ggtitle(expression(paste('Chl a (',mu,'g ','L'^-1, ')')))+
  stat_poly_line(se = FALSE) +
  stat_poly_eq(use_label("R2"), size = 8) +
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

chla_boxplot<- ggplot(summary_alldata, aes(x = season, y = chla))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  xlab("")+
  ylab("")+
  theme_bw(base_size = 12)
chla_boxplot+chla_relation + plot_layout(widths = c(1, 2))

DOC_relation<- ggplot(winter_summer,aes(x = summer_DOC, y = winter_DOC))+
  geom_point(size = 3)+
  stat_poly_line(se = FALSE) +
  stat_poly_eq(use_label("R2"), size = 8) +
  xlab("Winter")+
  ylab("Summer")+
  ggtitle(expression(paste('DOC (','mg ','L'^-1, ')')))+
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

DOC_boxplot<-ggplot(summary_alldata, aes(x = season, y = DOC))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  xlab("")+
  ylab("")+
  theme_bw(base_size = 12)

DIC_compare<-ggplot(winter_summer,aes(x = summer_DIC, y = winter_DIC))+
  geom_point(size = 3)+
  stat_poly_line(se = FALSE) +
  stat_poly_eq(use_label("R2"), size = 8) +
  xlab("Winter")+
  ylab("Summer")+
  ggtitle(expression(paste('DIC (','mg ','L'^-1, ')')))+
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

DIC_box<-ggplot(summary_alldata, aes(x = season, y = DIC))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  xlab("")+
  ylab("")+
  theme_bw(base_size = 12)

chloride_relation<-ggplot((winter_summer),aes(x = summer_chloride_mgL, y = winter_chloride_mgL))+
  geom_point(size = 3)+
  stat_poly_line(se = FALSE) +
  xlab("Winter")+
  ylab("Summer")+
  ggtitle(expression(paste('chloride (','mg ','L'^-1, ')')))+
  stat_poly_eq(use_label("R2"), size = 8) +
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

chloride_box<-ggplot(summary_alldata, aes(x = season, y = chloride_mgL))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  xlab("")+
  ylab("")+
  theme_bw(base_size = 12)

chloride_plot<- chloride_box + chloride_relation + plot_layout(widths = c(1,2))
ggsave('figures/draft/comparisons/chloride.png', height = 4, width = 8, dpi = 500, units = 'in')
TSS_plot<- TSS_box + TSS_relation + plot_layout(widths = c(1,2))
ggsave('figures/draft/comparisons/TSS.png', height = 4, width = 8, dpi = 500, units = 'in')
secchi_plot<- secchi_box + secchi_relation + plot_layout(widths = c(1,2))
ggsave('figures/draft/comparisons/secchi.png', height = 4, width = 8, dpi = 500, units = 'in')
chla_plot<- chla_boxplot+ chla_relation + plot_layout(widths = c(1,2))
ggsave('figures/draft/comparisons/chla.png', height = 4, width = 8, dpi = 500, units = 'in')
doc_plot<- DOC_boxplot + DOC_relation + plot_layout(widths = c(1,2))
ggsave('figures/draft/comparisons/doc.png', height = 4, width = 8, dpi = 500, units = 'in')
dic_plot<- DIC_box + DIC_compare + plot_layout(widths = c(1,2))
ggsave('figures/draft/comparisons/dic.png', height = 4, width = 8, dpi = 500, units = 'in')

TSS_relation<-ggplot((winter_summer),aes(x = summer_TSS_gL, y = winter_TSS_gL))+
  geom_point(size = 3)+
  stat_poly_line(se = FALSE) +
  xlab("Winter")+
  ylab("Summer")+
  ggtitle(expression(paste('TSS (','g ','L'^-1, ')')))+
  stat_poly_eq(use_label("R2"), size = 8) +
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

TSS_box<-ggplot(summary_alldata, aes(x = season, y = TSS_gL))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  xlab("")+
  ylab("")+
  theme_bw(base_size = 12)

secchi_relation<-ggplot((winter_summer),aes(x = summer_secchi_m, y = winter_secchi_m))+
  geom_point(size = 3)+
  stat_poly_line(se = FALSE) +
  xlab("Winter")+
  ylab("Summer")+
  ggtitle("Secchi (m)")+
  stat_poly_eq(use_label("R2"), size = 8) +
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

secchi_box<-ggplot(summary_alldata, aes(x = season, y = secchi_m))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  xlab("")+
  ylab("")+
  theme_bw(base_size = 12)

co2_relation<-ggplot(winter_summer,aes(x = summer_co2, y = winter_co2))+
  geom_point(size = 3)+
  stat_poly_line(se = FALSE) +
  xlab("Winter")+
  ylab("Summer")+
  ggtitle(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  stat_poly_eq(use_label("R2"), size = 8) +
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

co2_box<-ggplot(filter(summary_alldata, co2 < 1500), aes(x = season, y = co2))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  xlab("")+
  ylab("")+
  theme_bw(base_size = 12)

ch4_relation<-ggplot((winter_summer),aes(x = summer_ch4, y = winter_ch4))+
  geom_point(size = 3)+
  stat_poly_line(se = FALSE) +
  xlab("Winter")+
  ylab("Summer")+
  ggtitle(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  stat_poly_eq(use_label("R2"), size = 8) +
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())

ch4_box<-ggplot(filter(summary_alldata, ch4 < 400), aes(x = season, y = ch4))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha = 0.2)+
  xlab("")+
  ylab("")+
  theme_bw(base_size = 12)

(co2_box+co2_relation)/ (ch4_box +ch4_relation)  + plot_layout(widths = c(1,2))
ggsave('figures/draft/comparisons/gas.png', height = 6, width = 10, dpi = 500, units = 'in')

ggplot(filter(data, season == "winter"))+
  geom_point(aes(y = (Bottom_Temp - Top_Temp), x = Bottom_DO_perc))+
  xlab("")


ggplot((winter_summer),aes(x = log(summer_ch4), y = log(winter_ch4)))+
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  xlab("Winter")+
  ylab("Summer")+
  ggtitle(expression(paste('Log dissolved methane (',mu,'mol ','L'^-1, ')')))+
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())
ggplot((winter_summer),aes(x = log(summer_co2), y = log(winter_co2)))+
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  xlab("Winter")+
  ylab("Summer")+
  ggtitle(expression(paste('Log dissolved carbon dioxide (',mu,'mol ','L'^-1, ')')))+
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())
ggplot((winter_summer),aes(x = summer_bottom_do, y = winter_bottom_do))+
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  ylab("Winter")+
  xlab("Summer")+
  ggtitle(expression(paste('Log dissolved carbon dioxide (',mu,'mol ','L'^-1, ')')))+
  theme_bw(base_size = 12 )+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())


