#########################################################################
##  
##  Calculate the 8 land use wedges (except for enhanced weathering and soil carbon sequestration)
##  
##  Instructions:
##    change the path on line 23 to be the base of this replication package
##    (where readme.md exists)
##  
##  Outputs:
##    wedge_def
##    wedge_temp
##    wedge_trop
##    wedge_trop_incrop
##    wedge_temp_incrop
##    wedge_trop_silvo
##    wedge_temp_silvo
##    h_wedge_rewet
##    h_wedge_drain
##
##

  # set working directory
  setwd('C:/stabilisation-wedges-2025/')
  

  
###### LOAD PACKAGES AND SOURCE FILES ######
  
  library(dplyr)
  
  source('blam_library.r')
  
  
###### BESPOKE FUNCTIONS ###### 

  # gives median, 5th and 95th percentile
  sum_stat = function (x) {
    data.frame(median = median(x),
               lq = quantile(x, 0.05),
               uq = quantile(x, 0.95))
  }
  
  
  
###### LOAD INPUTS ######
  
  # number of monte carlo simulations
  N <- 10000

  # target emissions savings in 2050 for a wedge [GtCO2e/year]
  w_target <- 2
  
  # set seed for monte carlo simulations
  set.seed(42)
  
  
###### AVOIDED TROPICAL FOREST CONVERSION ######
  
  # use monte_carlo to generate simulate the committed flux following deforestation
  def_flux <- monte_carlo(N, 109.5, 96, 123) * 3.67 / 1000 # GtCO2 per Mha - mean and 95% CI from Griscom et al.
  
  # calculate wedge using monte carlo output [Mha / yr]
  wedge_def <- sum_stat(w_target / def_flux)
  
  cat('Prevent tropical forest loss: A reduction of', wedge_def$median , 'Mha per year acheives a wedge\n')
  

###### REFORESTATION  ######
  
  # temperate [Mha]
  # use monte_carlo to generate simulate the average carbon accumulation rate
  temp_flux <- monte_carlo(N, 1.8, 1.8 * 0.68, 1.8 * 1.32) * 3.67 / 1000 # GtCO2 per Mha - mean and 95% CI from Griscom et al., adapted according to Cook-Patton et al.
  wedge_temp <- sum_stat(w_target / temp_flux)
  
  cat('Reforest the temperate zone: Reforesting', wedge_temp$median , 'Mha by 2050 acheives a wedge\n')

  # tropical [Mha]
  trop_flux <- monte_carlo(N, 4.3, 4.3 * 0.68, 4.3 * 1.32) * 3.67 / 1000 # GtCO2 per Mha - mean and 95% CI from Griscom et al., adapted according to Cook-Patton et al.
  wedge_trop <- sum_stat(w_target / trop_flux)
  
  cat('Reforest the tropics: Reforesting', wedge_trop$median , 'Mha by 2050 acheives a wedge\n')
  
  
###### TREE INTERCROPPING ###### 
  
  # tropical [Mha]
  trop_incrop_flux <- monte_carlo(N, 3.59, 3.59 - 0.42, 3.59 + 0.42) * 3.67 / 1000 # GtCO2 per Mha - mean and 95% CI for alley-cropping from Cardineal et al. 
  wedge_trop_incrop <- sum_stat(w_target / trop_incrop_flux)
  
  cat('Add trees to tropical cropland: Adding', wedge_trop_incrop$median , 'Mha by 2050 acheives a wedge\n')
  
  # temperate [Mha]
  temp_incrop_flux <- monte_carlo(N, 1.61, 1.61 - 0.69, 1.61 + 0.69) * 3.67 / 1000 # GtCO2 per Mha - mean and 95% CI for silvoarable from Cardineal et al.
  wedge_temp_incrop <- sum_stat(w_target / temp_incrop_flux)
  
  cat('Add trees to temperate cropland: Adding', wedge_temp_incrop$median , 'Mha by 2050 acheives a wedge\n')
  
  
  
###### SILVOPASTURE ######
  
  # tropical [Mha]
  trop_silvo_flux <- monte_carlo(N, 4.2, 4.2 - 1.92, 4.2 + 1.92) * 3.67 / 1000 # GtCO2 per Mha - mean and 95% CI for silvopasture from Cardineal et al.
  wedge_trop_silvo <- sum_stat(w_target / trop_silvo_flux)
  
  cat('Add trees to tropical pasture: Adding', wedge_trop_silvo$median , 'Mha by 2050 acheives a wedge\n')
  
  # temperate [Mha]
  temp_silvo_flux <- monte_carlo(N, 3.1, 3.1 - 1.32, 3.1 + 1.32) * 3.67 / 1000 # GtCO2 per Mha - mean and 95% CI for silvopasture from Cardineal et al.
  wedge_temp_silvo <- sum_stat(w_target / temp_silvo_flux)
  
  cat('Add trees to temperate pasture: Adding', wedge_temp_silvo$median , 'Mha by 2050 acheives a wedge\n')
  
  
  
###### PEATLAND MANAGEMENT ###### 
  
  # emissions factors for drained and rewet tropical peatlands
  ef_drained <- monte_carlo(N, 61.2, 61.2 - 38.56, 61.2 + 38.56) / 1000 # GtCO2 per Mha - mean and 95% CI for silvopasture from Leifeld et al.
  ef_rewet <- monte_carlo(N, 5.44, 5.44 - 3.48, 5.44 + 3.48) / 1000 # GtCO2 per Mha - mean and 95% CI for silvopasture from Wilson et al.
  
  # committed emissions factor for new drainage over 30 years
  ef_committed <- ef_drained * 30
  
  # half-wedge of re-wetting [Mha]
  h_wedge_rewet <- sum_stat((w_target / 2) / (ef_drained - ef_rewet)) # in Mha
  
  cat('Rewet drained peatlands: Rewetting', h_wedge_rewet$median , 'Mha of drained peatlands by 2050 acheives a half-wedge\n')
  
  # half-wedge of prevented drainage [Mha/year]
  h_wedge_drain <- sum_stat((w_target / 2) / ef_committed) # in Mha of avoided drainage
  
  cat('Phase-out peatland drainage: Reducing peatland drainage by', h_wedge_drain$median , 'Mha per year by 2050 acheives a half-wedge\n')
