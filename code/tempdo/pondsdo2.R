raw = read_csv('data/tempdo/tempdo.csv') |> 
  mutate(season = case_when(month(DATE) <= 3 ~ 'winter',
                            month(DATE) > 3 ~ 'summer'))%>%
  mutate(sampling = ifelse(DATE > as.Date("2022-01-31"), "3",
                           ifelse(DATE < as.Date("2022-01-25"), "1", "2"))) |> 
  group_by(DATE, pond) |> 
  mutate(maxDepth = max(Depth)) |> 
  mutate(layer = case_when(Depth == 0 ~ 'Top',
                           Depth == maxDepth ~ 'Bottom',
                           Depth < maxDepth ~ NA)) |> 
  filter(!is.na(layer))


ponds = unique(raw$pond)


ggplot(raw) +
  geom_path(aes(y = layer, x = DO_Percent, group = DATE, color = season)) +
  geom_point(aes(y = layer, x = DO_Percent, group = DATE, fill = season), 
              alpha = 1, shape = 21, stroke = 0.2, size = 2) +
  geom_vline(xintercept = 100, linetype = 2, size = 0.2) +
  scale_fill_manual(values = c('#fcba03','lightblue4')) +
  scale_color_manual(values = c('#fcba03','lightblue4')) +
  xlab('DO (%)') +
  facet_wrap(~pond) +
  scale_y_discrete(expand = c(0.2, 0)) +
  theme_bw(base_size = 9) +
  theme(axis.title.y = element_blank(), 
        legend.position = 'bottom',
        legend.justification="left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10))

ggsave('figures/profiles/pondsdo2.png', height = 3.5, width = 6.5, dpi = 500, units = 'in')
