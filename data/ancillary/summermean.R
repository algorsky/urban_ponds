library(tidyverse)

summer_data_all<- data_all%>%
  filter(season == "summer")%>%
  select(pond, Bottom_Temp, Bottom_Cond, Bottom_DO, Top_Temp, Top_Cond, Top_DO, densGrad,
         chloride_mgL, sulfate_mgL, secchi_m, TSS_gL, POM_gL, OM_perc, chla_MicrogramsPerLiter, Bottom_DO_perc,
         DIC, DOC, TIC, TOC, RTRM, NH4, NO2_NO3, SRP)

winter_data_all<- data_all%>%
  filter(season == "winter")

summer_data_all

colMeans(summer_data_all, na.rm = TRUE)

summer_mean<- summer_data_all%>%
  group_by(pond)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
  rename_with( .fn = function(.x){paste0("summer_", .x)})%>%
  rename(pond = summer_pond)

data_stat<- winter_data_all%>%
  left_join(summer_mean, by = "pond")

ggplot(data_stat)+
  geom_point(aes(x = summer_chla_MicrogramsPerLiter, y = Bottom_DO))

#write_csv(data_stat, "data/ancillary/data_stat.csv")
