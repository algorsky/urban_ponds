library(tidyverse)
library(devtools)
library(lubridate)
library(purrr)
library(dplyr)

library(broom)
options(scipen = 999) #HD: how to turn scientific notation off


diffusive = read_csv('data/gas/diffusive_summer2021_run1.csv')%>%
  select(sample_id, concentrationCO2Gas, concentrationCH4Gas, concentrationN2OGas,concentrationCO2Air, concentrationCH4Air, concentrationN2OAir ) %>%
  separate(col = sample_id, into = c('pond','date','time','replicate'), sep = "_", remove = FALSE)%>%
  mutate(date = as.Date(date))%>%
  mutate(time = as.numeric(time))

diffusive %>%
  ggplot(aes(x = time, y = concentrationCO2Gas, group = replicate)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm, se = F, alpha = 0.1) +# Turn SE off
  facet_wrap(~pond + replicate, scales = 'free')

diffusiveCO2 <- diffusive %>% 
  select(date, pond, time, replicate, concentrationCO2Gas)

regressionCO2<- diffusiveCO2 %>% 
  nest(-(c(replicate, pond, date))) %>% 
  mutate(
    fit = map(data,  ~ lm(concentrationCO2Gas ~ time, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>%
  filter(term == "time")%>%
  select(date, pond, replicate, estimate) 

regressionCO2_R<- diffusiveCO2 %>% 
  nest(-(c(replicate, pond,date))) %>% 
  mutate(
    fit = map(data, ~ lm(concentrationCO2Gas ~ time, data = .x)),
    glanced = map(fit, glance)
  ) %>% 
  unnest(glanced) %>%
  select(date, pond, replicate, adj.r.squared) 

all_regressionCO2<- merge(regressionCO2, regressionCO2_R)


diffusive %>%
  ggplot(aes(x = as.numeric(time), y = concentrationN2OGas, group = replicate)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm, se = F, alpha = 0.1) +# Turn SE off
  facet_wrap(~pond + replicate, scales = 'free')

diffusiveCH4 <- diffusive %>% 
  select(date, pond, time, replicate, concentrationCH4Gas)

regressionCH4<- diffusiveCH4 %>% 
  nest(-(c(replicate, pond, date))) %>% 
  mutate(
    fit = map(data, ~ lm(concentrationCH4Gas ~ time, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>%
  filter(term == "time")%>%
  select(date, pond, replicate, estimate) 

regressionCH4_R<- diffusiveCH4 %>% 
  nest(-(c(replicate, pond,date))) %>% 
  mutate(
    fit = map(data, ~ lm(concentrationCH4Gas ~ time, data = .x)),
    glanced = map(fit, glance)
  ) %>% 
  unnest(glanced) %>%
  select(date, pond, replicate, adj.r.squared) 

all_regressionCH4<- merge(regressionCH4, regressionCH4_R)
