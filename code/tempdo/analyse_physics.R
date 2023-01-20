setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(rLakeAnalyzer)
library(lubridate)
library(reshape2)
library(patchwork)

calc_dens <-function(wtemp){
  dens = 999.842594 + (6.793952 * 1e-2 * wtemp) - (9.095290 * 1e-3 *wtemp**2) +
    (1.001685 * 1e-4 * wtemp**3) - (1.120083 * 1e-6* wtemp**4) +
    (6.536336 * 1e-9 * wtemp**5)
  return(dens)
}

g <- 9.81

df_tempdo <- read.csv('../../data/tempdo/tempdo.csv')
df_char <- read.csv('../../data/ancillary/data.csv') %>% select(pond, max_depth_m, totice, blackice, whiteice, secchi_m)

df_area <- read.csv('../../data/ancillary/pond_size.csv') %>%
  mutate(Area = SA_ha * 1e4) %>%
  rename(pond = Pond_Id)
df_depth <- read.csv('../../data/ancillary/max_depth.csv')

df <- merge(df_tempdo, df_area, by = 'pond')
df <- merge(df, df_char, by = 'pond')
head(df)

df_calc <- df %>%
  group_by(pond, DATE) %>%
  mutate(DATE = as.Date(DATE),
         dens = calc_dens(Temp_C),
         n2 = c(0,buoyancy.freq(wtr = Temp_C, depth = Depth)),
         area = approx(x = c(0, mean(max_depth_m, na.rm = T)), y = c(mean(Area), 0), xout = Depth, rule = 2 )$y,
         season = ifelse(month(DATE) > 3, 'summer', 'winter')) %>%
  summarise(densGrad = max(dens) - min(dens),
            maxN2 = max(n2, na.rm = T),
            St = schmidt.stability(wtr = Temp_C, depths = Depth, bthA = area,
                                   bthD = Depth, sal = 0))

ggplot(df_calc) +
  geom_line(aes(DATE, maxN2, col = as.factor(pond))) +
  geom_point(aes(DATE, maxN2, col = as.factor(pond))) +

ggplot(df_calc) +
  geom_line(aes(DATE, St, col = as.factor(pond))) +
  geom_point(aes(DATE, St, col = as.factor(pond))) +

ggplot(df_calc) +
  geom_line(aes(DATE, densGrad, col = as.factor(pond))) +
  geom_point(aes(DATE, densGrad, col = as.factor(pond))) +  plot_layout(guides = "collect") 

df_season <- df_calc %>%
  mutate(season = ifelse(month(DATE) > 3, 'summer', 'winter')) %>%
  group_by(pond, season) %>%
  summarise(mean_densGrad = mean(densGrad, na.rm = T),
            mean_maxN2 = mean(maxN2, na.rm = T),
            mean_St = mean(St, na.rm =T))

df_season %>% 
  select(pond, mean_St, season) %>%
  group_by(pond) %>%
  pivot_wider(names_from = season, values_from = mean_St) %>%
  ggplot() +
  geom_point(aes(x = winter, y = summer, col = pond)) +
  ggtitle('Schmidt stability (J/m2)') +
  theme_bw()

df_season %>% 
  select(pond, mean_densGrad, season) %>%
  group_by(pond) %>%
  pivot_wider(names_from = season, values_from = mean_densGrad) %>%
  ggplot() +
  geom_point(aes(x = winter, y = summer, col = pond)) +
  ggtitle('Density gradient (kg/m3/m)') +
  theme_bw()

df_season %>% 
  select(pond, mean_maxN2, season) %>%
  group_by(pond) %>%
  pivot_wider(names_from = season, values_from = mean_maxN2) %>%
  ggplot() +
  geom_point(aes(x = winter, y = summer, col = pond)) +
  ggtitle('Bupyancy freq. (s-2)') +
  theme_bw()


df_winter = df %>% 
  mutate(DATE = as.Date(DATE),
         season = ifelse(month(DATE) > 3, 'summer', 'winter')) %>%
  group_by(pond, DATE) %>%
  mutate(index = seq(1, length(DATE)))%>%
  filter(season == 'winter') %>%
  summarise(type = ifelse(mean(Temp_C, na.rm = T) >= 2, 'cryostratified', 'cryomictic'),
            total_DO = mean(DO_mgL, na.rm = T))

ggplot(df_winter) + 
  geom_line(aes(DATE, total_DO, col = type, shape = type, group = pond)) +
  geom_point(aes(DATE, total_DO, col = type, shape = type, group = pond))
