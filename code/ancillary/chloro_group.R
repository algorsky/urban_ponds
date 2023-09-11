library(tidyverse)
library(patchwork)
library(lubridate)
library(dplyr)


data <- read_csv('data/ancillary/data_stat.csv')|>
  mutate(chloro_group = ifelse(summer_chla_MicrogramsPerLiter > 49, "high", "low"))


ggplot(data, aes(x = chloro_group, y = Floating, fill = chloro_group))+
  geom_boxplot()
