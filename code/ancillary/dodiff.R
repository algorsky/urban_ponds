library(tidyverse)
library(patchwork)
library(lubridate)
library(dplyr)

data_all<- data_all|>
  mutate(diffDO = Top_DO - Bottom_DO)
plants<- read_csv("data/ancillary/extra_macrophyte.csv")

summary_summer<- data_all%>%
  filter(season == "summer")%>%
  group_by(pond)%>%
  summarize(Bottom_DO_summer = mean(Bottom_DO, na.rm = TRUE),
            Top_DO_summer = mean(Top_DO, na.rm = TRUE),
            diffDO_summer = mean(diffDO, na.rm = TRUE),
            densGrad_summer = mean(densGrad, na.rm = TRUE),
            RTRM_summer = mean(RTRM, na.rm = TRUE),
            chloride_mgL_summer = mean(chloride_mgL, na.rm = TRUE),
            sulfate_mgL_summer = mean(sulfate_mgL, na.rm = TRUE),
            secchi_m_summer = mean(secchi_m, na.rm = TRUE),
            TSS_gL_summer = mean(TSS_gL, na.rm = TRUE),
            POM_gL_summer = mean(POM_gL,na.rm = TRUE),
            OM_perc_summer = mean(OM_perc, na.rm = TRUE),
            DOC_summer = mean(DOC, na.rm = TRUE),
            DIC_summer = mean(DIC, na.rm = TRUE),
            chla_summer = mean(chla_MicrogramsPerLiter, na.rm = TRUE),
            co2_summer = mean(CO2_mean, na.rm = TRUE),
            ch4_summer = mean(CH4_mean, na.rm = TRUE),
            srp_summer = mean(SRP, na.rm = TRUE))%>%
  left_join(plants, by = "pond")
#write_csv(summary_summer, "data/ancillary/summary_summer.csv")

winter_ice<- data_all%>%
  filter(DATE > as.Date("2022-01-15"))%>%
  group_by(pond)%>%
  summarize(chla_winter = mean(chla_MicrogramsPerLiter, na.rm = TRUE),
            whiteice = mean(whiteice, na.rm = TRUE))

summary_winter<- data_all%>%
  filter(season == "winter")%>%
  group_by(pond)%>%
  summarize(Bottom_DO_winter = mean(Bottom_DO, na.rm = TRUE),
            Top_DO_winter = mean(Top_DO, na.rm = TRUE),
            diffDO_winter = mean(diffDO, na.rm = TRUE),
            densGrad_winter = mean(densGrad, na.rm = TRUE),
            RTRM_winter = mean(RTRM, na.rm = TRUE),
            chloride_mgL_winter = mean(chloride_mgL, na.rm = TRUE),
            sulfate_mgL_winter = mean(sulfate_mgL, na.rm = TRUE),
            secchi_m_winter = mean(secchi_m, na.rm = TRUE),
            TSS_gL_winter = mean(TSS_gL, na.rm = TRUE),
            POM_gL_winter = mean(POM_gL,na.rm = TRUE),
            OM_perc_winter = mean(OM_perc, na.rm = TRUE),
            DOC_winter = mean(DOC, na.rm = TRUE),
            DIC_winter = mean(DIC, na.rm = TRUE),
            co2_winter = mean(CO2_mean, na.rm = TRUE),
            ch4_winter = mean(CH4_mean, na.rm = TRUE),
            srp_winter = mean(SRP, na.rm = TRUE))%>%
  left_join(winter_ice, by = "pond")

#write_csv(summary_winter, "data/ancillary/summary_winter.csv")

summary_allponds<- summary_summer%>%
  left_join(summary_winter, by = "pond")

summary_allponds<- summary_allponds[,c(2:39)]

t.test((summary_allponds$diffDO_summer), (summary_allponds$diffDO_winter))

ggplot(summary_allponds)+
  geom_point(aes(x = diffDO_summer, y = diffDO_winter))+
  xlim(0, 110)+
  ylim(0, 110)

ggplot(data_all)+
  geom_point(aes(x = diffDO))
