
library(tidyverse)
library(forcats)
library(patchwork)

# Load data
ponds<- read_csv("data/ancillary/update_data.csv")

# Remove kettle 
df = ponds |> select(pond, DATE, season, CO2_mean, CH4_mean) |> 
  mutate(pond = as.factor(pond)) |> 
  arrange(CH4_mean) |> 
  filter(pond != 'KP')

p1 = ggplot(df, aes(x = fct_reorder(pond, CO2_mean, .desc = TRUE), y = CO2_mean, fill = season)) +
  geom_boxplot(linewidth = 0.2) +
  xlab('Pond') +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p2 = ggplot(df, aes(x = fct_reorder(pond, CO2_mean, .desc = TRUE), y = CH4_mean, fill = season)) +
  geom_boxplot(linewidth = 0.2) +
  xlab('Pond') +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# only kettle
df = ponds |> select(pond, DATE, season, CO2_mean, CH4_mean) |> 
  mutate(pond = as.factor(pond)) |> 
  arrange(CH4_mean) |> 
  filter(pond == 'KP') 

p3 = ggplot(df, aes(x = fct_reorder(pond, CO2_mean, .desc = TRUE), y = CO2_mean, fill = season)) +
  geom_boxplot(linewidth = 0.2) +
  xlab('') +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p4 = ggplot(df, aes(x = fct_reorder(pond, CO2_mean, .desc = TRUE), y = CH4_mean, fill = season)) +
  geom_boxplot(linewidth = 0.2) +
  xlab('') +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# join plots
p3 + p1 + p4 + p2 + 
  plot_layout(widths = c(0.5, 5), guides = 'collect')

ggsave('figures/GHG.png', width = 6, height = 4, dpi = 500)

