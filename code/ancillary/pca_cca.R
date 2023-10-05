# Load packages
library(vegan)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)
library(ggtext)


ponds<- read_csv("data/ancillary/update_data.csv")

df.dobottom<- ponds%>%
  filter(season == "winter")%>%
  select(pond, Bottom_DO)

df.dobottom.wide<- df.dobottom%>%
  group_by(pond)%>%
  mutate(row = row_number())%>%
  pivot_wider(names_from = pond,
              values_from = Bottom_DO)%>%
  select(-row)

ponds_winter<- ponds%>%
  filter(season == "winter")%>%
  select(pond, Bottom_DO, Top_DO, DOC, SRP, whiteice, chla_MicrogramsPerLiter)%>%
  mutate(row = row_number())%>%
  pivot_wider(names_from = pond,
              values_from = Bottom_DO)%>%
  select(-row)


#PCA Attempt 1
ponds <- read_csv("data/ancillary/summary_allponds.csv")
summary_allponds <- ponds[,c(1,2,3,13,15,18,19,22,23,33,35,36,37:39)] # select variables of interest

# Plot histogram to see if data should be log-scaled
summary_allponds |> select(-pond) %>% gather() |> 
  ggplot(aes(log(value))) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

myPR <- prcomp(log(summary_allponds[,-1]+0.1), scale = TRUE)
# biplot(myPR, scale = 0) # basic plot

# Get summary of PCs
summ = summary(myPR)

#Extract PC scores
ponds2 <- summary_allponds |> bind_cols(myPR$x[,1:2])
pca.arrows = as_tibble(myPR$rotation[,1:2]) |> 
  bind_cols(label = rownames(myPR$rotation)) |> 
  mutate(season = if_else(str_detect(label, 'summer'), 'summer','winter'))

# PLOT PC 1 and 2 with ggplot
scaling_factor = 10

ggplot(ponds2) +
  # stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(aes(PC1, PC2, col = pond, fill = pond), shape = 21, col = "black", size = 2) +
  #Add pond text
  geom_segment(data = pca.arrows, 
               aes(x=0, #Starting coordinate in CCA1 = 0 
                   xend = PC1*scaling_factor, 
                   y=0, #Start in CCA2 = 0
                   yend = PC2*scaling_factor), 
               linewidth = 0.3,
               color = "black", #set color
               arrow = arrow(length=unit(0.02,"npc"))) + #Set the size of the lines that form the tip of the arrow
  #Add environmental vars text
  geom_label_repel(data = pca.arrows, 
                   aes(x = PC1*scaling_factor, y = PC2*scaling_factor, color = season),
                   label = pca.arrows$label,
                   force = 10,
                   label.padding = 0.1,
                   nudge_y = -1, fill = alpha("white", 0.9),
                   segment.size = 0.2, size = 2.5, show.legend = FALSE) +
  scale_color_manual(values = c('lightblue4','red3')) +
  #Set x and y axis titles
  labs(x=paste0("PC1 (",round(summ$importance[2,1]*100,1)," %)"),
       y=paste0("PC2 (",round(summ$importance[2,2]*100,1)," %)")) +
  #Set bw theme
  theme_bw(base_size = 9) +
  theme(legend.text = element_text(size = 8), 
        # plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin = margin(c(0,0,0,0), unit = "cm"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm")) +
  theme(legend.text = element_markdown())

ggsave('figures/pca.png', width = 6, height = 4, dpi = 500)

#setNames(c('Summer Bottom DO','Summer Surface DO','Summer DOC', "Summer Chl a", "Summer SRP",
      #     'Floating Macrophyte','Winter Bottom DO','Winter Surface DO','Winter DOC', "Winter SRP",
        #   "Winter Chl a", "White Ice"))
