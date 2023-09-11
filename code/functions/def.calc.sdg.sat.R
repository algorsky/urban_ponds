##############################################################################################
def.calc.sdg.sat <- function(
    inputFile,
    baro = "barometricPressure",
    waterTemp = "waterTemp",
    headspaceTemp = "headspaceTemp",
    concCO2 = "dissolvedCO2",
    sourceCO2 = "concentrationCO2Air",
    concCH4 = "dissolvedCH4",
    sourceCH4 = "concentrationCH4Air",
    concN2O = "dissolvedN2O",
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
  cConcPerc <- 100 #Convert to percent
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
  
  ##### Calculate dissolved gas concentration at 100% saturation ##### 
  
  # 100% saturation occurs when the dissolved gas concentration is in equilibrium
  # with the atmosphere.
  inputFile$satConcCO2 <- (ckHCO2 * exp(cdHdTCO2*(1/(inputFile[,waterTemp] + cKelvin) - 1/cT0))) * 
    inputFile[,sourceCO2] * inputFile[,baro] * cPresConv
  inputFile$satConcCH4 <- (ckHCH4 * exp(cdHdTCH4*(1/(inputFile[,waterTemp] + cKelvin) - 1/cT0))) * 
    inputFile[,sourceCH4] * inputFile[,baro] * cPresConv  
  inputFile$satConcN2O <- (ckHN2O * exp(cdHdTN2O*(1/(inputFile[,waterTemp] + cKelvin) - 1/cT0))) * 
    inputFile[,sourceN2O] * inputFile[,baro] * cPresConv
  
  ##### Calculate dissolved gas concentration as % saturation #####
  inputFile$CO2PercSat <- inputFile[,concCO2]/inputFile$satConcCO2 * cConcPerc
  inputFile$CH4PercSat <- inputFile[,concCH4]/inputFile$satConcCH4 * cConcPerc
  inputFile$N2OPercSat <- inputFile[,concN2O]/inputFile$satConcN2O * cConcPerc
  
  return(inputFile)
  
}