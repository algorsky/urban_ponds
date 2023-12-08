library(tidyverse)
library(nlme)
library(AICcmodavg)

data<- read_csv("data/ancillary/model/data.csv")

#Visualize Data Distribution
ggplot(data)+
  geom_histogram(aes(x = Bottom_DO_perc))
# Does variance of DO appear homogeneous among ponds?
model1 <- lm(Bottom_DO_perc~pond, data=data)
plot(model1, ask=F)

model2 <- lm(log(Bottom_DO_perc)~pond, data=data)
plot(model2, ask=F)

#Log transform
ggplot(data)+
  geom_histogram(aes(x = log(Bottom_DO_perc)))
data<- data%>%
  mutate(DO_transform = log(Bottom_DO_perc))

#Morphometry
mod1<- lm(DO_transform~depth_m + PA_ratio + Age, data = data)
anova(mod1)
# Age
#Winter Drivers
mod2<- lm(DO_transform ~ whiteice + TSS_gL + chla_MicrogramsPerLiter + DOC + SRP + NO2_NO3, data = data)
anova(mod2)
# TSS_gL, chla_MicrogramsPerLiter, DOC, SRP
#Summer Drivers
mod3<- lm(DO_transform ~ Floating + densGrad_summer + TSS_gL_summer + DOC_summer +chla_summer, data = data)
anova(mod3)
#Floating, densGrad_summer, TSS_gL_summer, DOC_summer
#Mega model
mod4<- lm(DO_transform ~ Age + TSS_gL + chla_MicrogramsPerLiter + DOC + SRP + Floating + densGrad_summer + TSS_gL_summer + DOC_summer, data = data)
anova(mod4)
#Mega model minus Floating
mod5<- lm(DO_transform ~ Age + TSS_gL + chla_MicrogramsPerLiter + DOC + SRP  + densGrad_summer + TSS_gL_summer + DOC_summer, data = data)
anova(mod5)
modsDO <- list( 
  "Morphometry" = mod1, 
  "Winter drivers" = mod2,
  "Summer drivers" = mod3,
  "Mega model" = mod4,
  "Mega model - Floating" = mod5
)
aictab(modsDO)

anova(mod5)

hist(residuals(mod5))
#Slight right tail 
final_residuals <-residuals(mod5, type='pearson')
final_fitted <-fitted.values(mod5)
plot(final_fitted,final_residuals)
abline(h=0)
plot(mod5)

