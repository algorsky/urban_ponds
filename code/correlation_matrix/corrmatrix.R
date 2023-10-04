library(tidyverse)
library(lubridate)
library(ggcorrplot)
library(corrr)
library(patchwork)
library(ggtext)

ponds<- read_csv("data/ancillary/summary_allponds.csv")
summary_allponds<- ponds[,c(2,3, 13,15, 18, 19,22,23,33,37:39)]|>
  setNames(c('Summer Bottom DO','Summer Surface DO','Summer DOC', "Summer Chl a", "Summer SRP",
             'Floating Macrophyte','Winter Bottom DO','Winter Surface DO','Winter DOC', "Winter SRP",
             "Winter Chl a", "White Ice"))

###### Compute a correlation matrix ######
corr <- round(cor(summary_allponds,use = "pairwise.complete.obs", method = 'pearson'), 2)
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(summary_allponds)

# Correlation plot at p < 0.05 sig
c.plot = ggcorrplot(corr, tl.col = "red",type = "full", hc.order = T,
                    lab = TRUE, p.mat = p.mat, insig = "blank",
                    outline.col = "white", tl.cex = 8, lab_size = 2,
                    ggtheme = ggplot2::theme_bw(base_size = 9), sig.level = 0.05,
                    colors = c("#E46726", "grey95", "#6D9EC1"))


#ggsave('figures/draft/corr.png', height = 8, width = 12, dpi = 500, units = 'in')

### correlation network map
varCorr = correlate(summary_allponds, method = 'pearson', use = 'pairwise.complete.obs')
varCorr[,-1][p.mat >= 0.05] <- 0
set.seed(16)
c2.plot = network_plot2.cor_df(varCorr, min_cor = 0.1,
                               colours = c("#E46726", "grey95", "#6D9EC1")); c2.plot

# Join figures together
c.plot + theme(legend.position = 'none') + c2.plot + 
  plot_layout(widths = c(0.4,0.5)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size  = 8),
        # legend.title = element_blank(),
        legend.key.width = unit(0.3, 'cm'),
        legend.text = element_text(size = 8),
        plot.margin = margin(l = 0.05, t = 0.05, r = 0.05, unit='cm'))
ggsave('figures/draft/corr_plots.png', height = 3.5, width = 8, dpi = 500, units = 'in')
