library(tidyverse)

data <- read_csv('data/ancillary/data_stat.csv')

ggplot(data)+
  geom_point(aes(x = summer_Top_DO, y = summer_Bottom_DO))

ggplot(data)+
  geom_point(aes(x = summer_Top_DO, y = Bottom_DO))

ggplot(data)+
  geom_point(aes(x = summer_DOC, y = Bottom_DO))

# Does variance of methane appear homogeneous among ponds? 
model1 <- lm(Bottom_DO~pond, data=data)
summary(model1)
plot(model1, ask = F)
#ggsave(filename = 'figures/logCH4_01.png',width = 6.5,height = 6,units = 'in')
hist(log(data$Bottom_DO))
#Log transform
data$logDO <-log10(data$Bottom_DO)

model2 <- lm(logDO~pond, data=data)
plot(model2, ask=F)
summary(model2)
ggsave(log, filename = 'figures/logCH4_02.png',width = 6.5,height = 6,units = 'in')

#Correlation Matrix
library(Hmisc)
library(ggcorrplot)
data.cor<- data[,c(4, 8, 10, 16, 18, 19, 22, 23, 26, 31, 32, 44, 47, 50, 52, 54, 55, 58, 60,61, 67, 68, 69)]
dat.cor <- rcorr(as.matrix(data.cor))
ggcorrplot(dat.cor$r, type = "lower",
           lab = TRUE) 
ggsave(filename = 'figures/winter_correlation.png',width = 7.5,height = 7,units = 'in')


