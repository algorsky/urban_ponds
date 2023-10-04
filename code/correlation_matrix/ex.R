library(tidyverse)
library(lubridate)
library(ggcorrplot)
library(corrr)
library(patchwork)

# Load modified corr function, for better visualization
source('SSBcode/Functions//network_plot2.R')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('SSBcode/00_LoadData.R')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select data
useVars = env.vars |> 
  mutate(PAR.est = log10(PAR.est + 0.001)) |> 
  select(avsnow, totice, whiteice, blackice, secchi, chlorophyll_ug_L, PAR.est) |> 
  setNames(c('Snow','Total Ice','White Ice',
             'Black Ice','Secchi','Chl-a 0 m','PAR 0.7 m'))

###### Compute a correlation matrix ######
corr <- round(cor(useVars,use = "pairwise.complete.obs", method = 'pearson'), 2)
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(useVars)

# Correlation plot at p < 0.05 sig
c.plot = ggcorrplot(corr, type = "full", hc.order = T,
                    lab = TRUE, p.mat = p.mat, insig = "blank",
                    outline.col = "white", tl.cex = 8, lab_size = 2,
                    ggtheme = ggplot2::theme_bw(base_size = 9), sig.level = 0.10,
                    colors = c("#E46726", "grey95", "#6D9EC1"))

### correlation network map
varCorr = correlate(useVars, method = 'pearson', use = 'pairwise.complete.obs')
varCorr[,-1][p.mat >= 0.10] <- 0

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

ggsave('SSBfigures/Figure3_CorrPlots.png', width = 6.5, height = 2.5, dpi = 500)
