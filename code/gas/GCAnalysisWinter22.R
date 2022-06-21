library(tidyverse)
library(devtools)
library(lubridate)


#QA/QC
# Read in data 
# Read in data 
df.2022 = read_csv('data/GC/GC_Winter22.csv')  %>% 
  filter(batch_id == 'POND')%>%
  mutate(sample_id = str_replace_all(sample_id, pattern = "2020_01_", replacement = "2020-01-")) %>%  #because you switch from underscores to dashes
  separate(col = sample_id, into = c('pond','date','depth','replicate'), sep = "_", remove = FALSE) %>% 
  mutate(date = as.Date(date))%>%
  filter(depth == "Air" | depth == "Surface")

#write.csv(df.2022, 'data/gas/winter22/df.2022.csv')
#df.clean.2022 = read_csv('data/gas/winter22/df.clean.2022.csv')
air_samples <- df.clean.2022 %>% filter(grepl("Air",depth)) %>% 
  select(-analysis_date,-batch_id,-sample_id,-depth, -export, -replicate) %>% 
  group_by(pond,date) %>% 
  summarize_all(list(median = median)) %>% 
  select(-CO2_TCD_median,-CH4_TCD_median) %>%  #replaced FID with TCD when >1000
  ungroup()

dat.clean.2022 <- df.clean.2022 %>% 
  filter(!grepl("Air",depth)) %>% 
  select(analysis_date, batch_id,sample_id, pond, date,depth,CO2_FID,CO2_TCD,CH4_FID,CH4_TCD,N2O_ECD) %>% 
  # mutate(N2O_ECD = replace(N2O_ECD,N2O_ECD < 0, 0)) %>% 
  left_join(air_samples) %>% 
  rename(concentrationCO2Gas = CO2_FID) %>% 
  rename(concentrationCH4Gas = CH4_FID) %>%
  rename(concentrationN2OGas = N2O_FID) %>% 
  rename(concentrationCO2Air = CO2_FID_median) %>%
  rename(concentrationCH4Air = CH4_FID_median) %>%
  rename(concentrationN2OAir = N2O_ECD_median) %>% 
  mutate(gasVolume = 60) %>% #mL
  mutate(waterVolume = 940) %>% #mL
  mutate(waterTemp = 0.2) %>% #degC
  mutate(headspaceTemp = 0.2) %>% #degC Assume it is the same was water temp
  mutate(barometricPressure = 103.991) #kpa

#write.csv(dat.clean.2022, 'data/gas/winter22/dat_clean_winter22.csv')
#dat <- read_csv('data/gas/winter22/dat_winter22.csv')
#temp_press<- read_csv('data/ancillary/temp_pressure.csv')
#dat<- dat%>%
  #left_join(temp_press, by = c("pond","date"))
#write.csv(dat, 'data/gas/winter22/dat_winter22.csv')

dat = read_csv('data/gas/winter22/dat_clean_winter22.csv')
dat.out <- def.calc.sdg.conc(as.data.frame(dat)) %>%
  select(pond,date,depth,dissolvedCO2,dissolvedCH4,dissolvedN2O) %>% 
  gather(value="value",key="parameter",-pond,-date,-depth)

tidy.dat.out.winter22<- def.calc.sdg.conc(as.data.frame(dat)) %>%
  select(pond,date,depth,dissolvedCO2,dissolvedCH4, dissolvedN2O)

write.csv(tidy.dat.out.winter22, 'data/gas/winter22/tidy.dat.out.winter22.csv')




#Function Dissolved Gas concentration calculations
def.calc.sdg.conc <- function(
  inputFile,
  volGas = "gasVolume",
  volH2O = "waterVolume",
  baro = "barometricPressure",
  waterTemp = "waterTemp",
  headspaceTemp = "headspaceTemp",
  eqCO2 = "concentrationCO2Gas",
  sourceCO2 = "concentrationCO2Air",
  eqCH4 = "concentrationCH4Gas",
  sourceCH4 = "concentrationCH4Air",
  eqN2O = "concentrationN2OGas",
  sourceN2O = "concentrationN2OAir"
) {
  
  if(typeof(inputFile) == "character"){
    inputFile <- read.csv(inputFile)
  }
  
  ##### Constants #####
  cGas<-8.3144598 #universal gas constant (J K-1 mol-1)
  cKelvin <- 273.15 #Conversion factor from Kelvin to Celsius
  cPresConv <- 0.000001 # Constant to convert mixing ratio from umol/mol (ppmv) to mol/mol. Unit conversions from kPa to Pa, m^3 to L, cancel out.
  cT0 <- 298.15#Henry's law constant T0
  #Henry's law constants and temperature dependence from Sander (2015) DOI: 10.5194/acp-15-4399-2015
  ckHCO2 <- 0.00033 #mol m-3 Pa, range: 0.00031 - 0.00045
  ckHCH4 <- 0.000014 #mol m-3 Pa, range: 0.0000096 - 0.000092
  ckHN2O <- 0.00024 #mol m-3 Pa, range: 0.00018 - 0.00025
  cdHdTCO2 <- 2400 #K, range: 2300 - 2600
  cdHdTCH4 <- 1900 #K, range: 1400-2400
  cdHdTN2O <- 2700 #K, range: 2600 - 3600
  
  ##### Populate mean global values for reference air where it isn't reported #####
  inputFile[,sourceCO2] = ifelse(is.na(inputFile[,sourceCO2]),# if reported as NA
                                 405, # use global mean https://www.esrl.noaa.gov/gmd/ccgg/trends/global.html
                                 inputFile[,sourceCO2])
  
  inputFile[,sourceCH4] = ifelse(is.na(inputFile[,sourceCH4]), # use global average if not measured
                                 1.85, #https://www.esrl.noaa.gov/gmd/ccgg/trends_ch4/
                                 inputFile[,sourceCH4])
  
  inputFile[,sourceN2O] = ifelse(is.na(inputFile[,sourceN2O]), # use global average if not measured
                                 0.330, #https://www.esrl.noaa.gov/gmd/hats/combined/N2O.html
                                 inputFile[,sourceN2O])
  
  ##### Calculate dissolved gas concentration in original water sample #####
  inputFile$dissolvedCO2 <- NA
  inputFile$dissolvedCO2 <- inputFile[,baro] * cPresConv * 
    (inputFile[,volGas]*(inputFile[,eqCO2] - inputFile[,sourceCO2])/(cGas * (inputFile[,headspaceTemp] + cKelvin) * inputFile[,volH2O]) + 
       ckHCO2 * exp(cdHdTCO2*(1/(inputFile[,headspaceTemp] + cKelvin) - 1/cT0))* inputFile[,eqCO2])
  
  inputFile$dissolvedCH4 <- NA
  inputFile$dissolvedCH4 <- inputFile[,baro] * cPresConv * 
    (inputFile[,volGas]*(inputFile[,eqCH4] - inputFile[,sourceCH4])/(cGas * (inputFile[,headspaceTemp] + cKelvin) * inputFile[,volH2O]) + 
       ckHCH4 * exp(cdHdTCH4*(1/(inputFile[,headspaceTemp] + cKelvin) - 1/cT0))* inputFile[,eqCH4])
  
  inputFile$dissolvedN2O <- NA
  inputFile$dissolvedN2O <- inputFile[,baro] * cPresConv * 
    (inputFile[,volGas]*(inputFile[,eqN2O] - inputFile[,sourceN2O])/(cGas * (inputFile[,headspaceTemp] + cKelvin) * inputFile[,volH2O]) + 
       ckHN2O * exp(cdHdTN2O*(1/(inputFile[,headspaceTemp] + cKelvin) - 1/cT0))* inputFile[,eqN2O])
  
  ##### Step-by-step Calculation of dissolved gas concentrations for testing #####
  
  # Dissolved gas concentration in the original water samples (dissolvedGas) is
  # calculated from a mass balance of the measured headspace concentration (eqGas), the 
  # calculated concentration in the equilibrated headspace water (eqHeadspaceWaterCO2), 
  # and the volumes of the headspace water and headspace gas, following:
  
  # dissolvedGas  = ((eqGas * volGas) + (eqHeadspaceWaterGas * volH2O) - (sourceGas * volGas)) / volH2O
  
  # Measured headspace concentration should be expressed as mol L- for the mass
  # balance calculation and as partial pressure for the equilibrium calculation.
  
  # #Temperature corrected Henry's Law Constant
  # HCO2 <- ckHCO2 * exp(cdHdTCO2 * ((1/(headspaceTemp+cKelvin)) - (1/cT0)))
  # HCH4 <- ckHCH4 * exp(cdHdTCH4 * ((1/(headspaceTemp+cKelvin)) - (1/cT0)))
  # HN2O <- ckHN2O * exp(cdHdTN2O * ((1/(headspaceTemp+cKelvin)) - (1/cT0)))
  # 
  # #Mol of gas in equilibrated water (using Henry's law)
  # CO2eqWat <- HCO2 * eqCO2 * cPresConv * baro * (volH2O/1000)
  # CH4eqWat <- HCH4 * eqCH4 * cPresConv * baro * (volH2O/1000)
  # N2OeqWat <- HN2O * eqN2O * cPresConv * baro * (volH2O/1000)
  # 
  # #Mol of gas in equilibrated air (using ideal gas law)
  # CO2eqAir <- (eqCO2 * cPresConv * baro * (volGas/1000))/(cGas * (headspaceTemp + cKelvin))
  # CH4eqAir <- (eqCH4 * cPresConv * baro * (volGas/1000))/(cGas * (headspaceTemp + cKelvin))
  # N2OeqAir <- (eqN2O * cPresConv * baro * (volGas/1000))/(cGas * (headspaceTemp + cKelvin))
  # 
  # #Mol of gas in source gas (using ideal gas law)
  # CO2air <- (inputFile[,sourceCO2] * cPresConv * baro * (volGas/1000))/(cGas * (headspaceTemp + cKelvin))
  # CH4air <- (inputFile[,sourceCH4] * cPresConv * baro * (volGas/1000))/(cGas * (headspaceTemp + cKelvin))
  # N2Oair <- (inputFile[,sourceN2O] * cPresConv * baro * (volGas/1000))/(cGas * (headspaceTemp + cKelvin))
  # 
  # #Total mol of gas is sum of equilibrated water and equilibrated air
  # CO2tot <- CO2eqWat + CO2eqAir
  # CH4tot <- CH4eqWat + CH4eqAir
  # N2Otot <- N2OeqWat + N2OeqAir
  # 
  # #Total mol of gas minus reference air mol gas to get water mol gas before equilibration
  # CO2wat <- CO2tot - CO2air
  # CH4wat <- CH4tot - CH4air
  # N2Owat <- N2Otot - N2Oair
  # 
  # #Concentration is mol of gas divided by volume of water
  # inputFile$dissolvedCO2 <- CO2wat/(volH2O/1000)
  # inputFile$dissolvedCH4 <- CH4wat/(volH2O/1000)
  # inputFile$dissolvedN2O <- N2Owat/(volH2O/1000)
  
  #Round to significant figures
  inputFile$dissolvedCO2 <- signif(inputFile$dissolvedCO2, digits = 3)
  inputFile$dissolvedCH4 <- signif(inputFile$dissolvedCH4, digits = 3)
  inputFile$dissolvedN2O <- signif(inputFile$dissolvedN2O, digits = 3)
  
  return(inputFile)
  
}



