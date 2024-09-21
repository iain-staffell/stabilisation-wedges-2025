#########################################################################
##  
##  Calculate the 7 transport sector wedges
##  
##  Instructions:
##    change the path on line 21 to be the base of this replication package
##    (where readme.md exists)
##  
##  Outputs:
##    wedge_v_eff
##    wedge_avoid
##    wedge_EVs
##    wedge_public
##    wedge_air
##    wedge_bio
##    wedge_freight
##  
##  

  # set working directory
  setwd('C:/stabilisation-wedges-2025/')
  
  
  
###### LOAD PACKAGES AND SOURCE FILES ######
  
  library(tidyverse)
  
  
  
###### LOAD INPUT DATA ######
  
  # load passenger transport data from scenarios
  psg_tran <- readRDS("Fig 3/Inputs/passenger_transport.rds")
  
  # load freight transport data from scenarios
  freight <- readRDS("Fig 3/Inputs/freight_transport.rds")

  # load carbon intensity of electricity [GtCO2e/TWh]
  CI_elec <- read.csv("Fig 3/Inputs/grid_carbon_intensity.csv", check.names = FALSE, row.names = 1) %>%
             pivot_longer(c(2:10), names_to = "year")
  
  # scenario data on hydrogen production
  h2_prod <- readRDS("Fig 3/Inputs/hydrogen_production.rds")
  
  # combustion emissions factors [GtCO2e/Twh]
  ef <- readRDS("Fig 3/Inputs/emission_factors.rds")
  
  # overhead for combustion emissions factors [GtCO2e/Twh]
  ef_oh <- ef * c(0.075, 0.20, 0.25)
  
  # total emissions factors including overhead [GtCO2e/Twh]
  ef_tot <- ef + ef_oh
  
  # CO2e multiplier for aviation emissions from Lee et al.
  avi_mult <- 1.7
  
  # life cycle emissions from ethanol production [g/MJ]
  biofuels_emis <- readRDS("Fig 3/Inputs/ethanol_emissions.rds")
  
  # number of simulations for monte_carlo 
  N <- 10000
  
  # target emissions savings in 2050 for a wedge [GtCO2e/year]
  w_target <- 2
  
  
  
###### BESPOKE FUNCTIONS ######
  
  # calculates carbon intensity of hydrogen production [GtCO2/TWh]
  # assumes all SMR hydrogen in IEA2 is fitted with CCS, but does not matter as only value for 2050 is used
  CI_hydrogen <- function (data) {
    
    CI_h2 <- list()
    
    for (i in 1:nrow(data)){
      
      if (grepl('SMR', data[i, 2]) & data[i, 1] != "iea2") {
        CI_h2[i] <- ((1 / data[i, 4]) * ef_tot$gas)
      } 
      else if (grepl('SMR', data[i, 2]) & data[i, 1] == "iea2") {
        CI_h2[i] <- ((1 / data[i, 4]) * ef_tot$gas) * 0.33
      } 
      else if (grepl('coal', data[i, 2])) {
        CI_h2[i] <- ((1 / data[i, 4]) * ef_tot$solid)
      } 
      else if (grepl('elec', data[i, 2])) {
        CI_h2[i] <- ((1 / data[i, 4]) * data[i, 6])
      } 
    }
    as.numeric(unlist(as.character(CI_h2)))
  }
  
  # calculates emissions per vehicle km [GtCO2e/vkm]
  CI_vkm <- function (data) {
    
    CI <- list()
    
    for (i in 1:nrow(data)){
      
      if (grepl('fuel|liquid', data[i, 2]) & !grepl("plane", data[i, 2])) {
        CI[i] <- data[i, 7] * ef_tot$liquid
      } 
      else if (grepl('fuel|liquid', data[i, 2]) & grepl("plane", data[i, 2])) {
        CI[i] <- data[i, 7] * ef$liquid * avi_mult + data[i, 7] * ef_oh$liquid
      } else if (grepl('gas', data[i, 2])) {
        CI[i] <- data[i, 7] * ef_tot$gas
      } else if (grepl('elec|EV', data[i, 2])) {
        CI[i] <- data[i, 7] * data[i, 10]
      } else if (grepl('PH', data[i, 2])) {
        CI[i] <- data[i, 7] * data[i, 11]
      } else if (grepl('H2', data[i, 2])) {
        CI[i] <- data[i, 7] * data[i, 12]
      }
    }
    as.numeric(unlist(as.character(CI)))
  }
  
  # calculates total emissions from freight [GtCO2e]
  emi_freight <- function (data) {
    
    emi <- list()
    
    for (i in 1:nrow(data)){
      
      if (grepl('fuel|liquid', data[i, 2])) {
        emi[i] <- data[i, 4] * ef_tot$liquid
      } else if (grepl('gas', data[i, 2])) {
        emi[i] <- data[i, 4] * ef_tot$gas
      } else if (grepl('elec|EV', data[i, 2])) {
        emi[i] <- data[i, 4] * data[i, 5]
      } else if (grepl('PH', data[i, 2])) {
        emi[i] <- data[i, 4] * data[i, 6]
      } else if (grepl('H2', data[i, 2])) {
        emi[i] <- data[i, 4] * data[i, 7]
      }
    }
    as.numeric(unlist(as.character(emi)))
  }
  
  # from TWh per km to litres per 100km
  to_l100km <- function (data) {
    l100km = data * 1e9 / 8.9 * 100
  }
  
  # monte carlo function
  monte_carlo <- function(N, mean, min=NULL, max=NULL) {
    
    if (is.null(min) | is.null(max))
      return(rep(mean, N))
    
    constrained_distro('rnorm', N, min=min, max=max, mean=mean, sd=(max-min)/3)
  }

  # convert from g per MJ to Gt per TWh 
  to_Gt_TWh <- function (data) { 
    Gt_TWh = (data * 3.6e9) / 1e15 
  }
  
  
  
###### CALCULATE CARBON INTENSITY OF HYDROGEN PRODUCTION ######
  
  # add CI of electricity as a column
  h2_prod_CI <- h2_prod %>%
                full_join(CI_elec, by = c("scenario", "year")) %>%
                rename("CI_elec" = value)
  
  # calculate average carbon intensity of hydrogen production for each scenario
  CI_h2 <- h2_prod_CI %>%
           mutate(CI.h2 = CI_hydrogen(h2_prod_CI)) %>%
           group_by(scenario, year) %>%
           summarise(CI.h2 = weighted.mean(CI.h2, share))
  
  
  ###### CALCULATE CARBON INTENSITY OF DIFFERENT TYPES OF TRANSPORT ######
  
  # add carbon intensity of electricity, plug-in hybrids (oil/electricity) and H2
  psg_trans <- psg_tran %>% 
               full_join(CI_elec, by = c("scenario", "year")) %>%
               rename("CI.elec" = value) %>%
               mutate(CI.PH = (CI.elec + ef_tot$liquid) / 2) %>%
               full_join(CI_h2, by = c("scenario", "year"))
  
  # calculate carbon intensity of transport per vkm and pkm [GtCO2e/km] and total emissions from each vehicle type [GtCO2e]
  psg_trans_full <- psg_trans %>%
                    mutate(GtCO2e.vkm = CI_vkm(psg_trans),
                           GtCO2e.pkm = GtCO2e.vkm / occupancy,
                           GtCO2e.total = GtCO2e.vkm * vehicle.km) %>%
                    select(-CI.PH, -CI.elec, -CI.h2) %>%
                    separate(type, into = c( "mode", "type"), sep = ".passenger.") %>%
                    separate(type, into = c("region", "type1", "type2")) %>%
                    unite("type", c(type1, type2), sep = ".", na.rm = TRUE)
  psg_trans_full[is.na(psg_trans_full)] <- 0
  
  # aggregate urban and rural regions together and sum/average relevant columns
  # summarise occupancy and CO2.pkm where 'vehicles' or 'passenger.km' are zero
  psg_trans_agg <- psg_trans_full %>%
                   group_by(scenario, year, mode, type) %>%
                   summarise(efficiency = mean(TWh.vehicle.km),
                             occupancy_w = weighted.mean(occupancy, vehicles),
                             occupancy = mean(occupancy),
                             pkm = sum(passenger.km), 
                             vkm = sum(vehicle.km), 
                             n.veh = sum(vehicles),
                             CO2.tot = sum(GtCO2e.total), 
                             CO2.vkm = mean(GtCO2e.vkm), 
                             CO2.pkm_w = weighted.mean(GtCO2e.pkm, passenger.km),
                             CO2.pkm = mean(GtCO2e.pkm),
                             vkm.veh = mean(vehicle.km.vehicle)) %>%
                   mutate(occupancy = ifelse(is.nan(occupancy_w), occupancy, occupancy_w),
                          CO2.pkm = ifelse(is.nan(CO2.pkm_w), CO2.pkm, CO2.pkm_w)) %>%
                   select(scenario, year, mode, type, n.veh, vkm.veh, occupancy, pkm, vkm, efficiency, CO2.vkm, CO2.pkm, CO2.tot) %>%
                   ungroup()
  
  
  
###### CALCULATE EMISSIONS FACTOR FOR BIOETHANOL ######
  
  # filter biofuel emissions for bioethanol and group by 1st and 2nd generation [g/MJ] 
  ethanol <- biofuels_emis %>%
    filter(energy.type == "bioethanol") %>%
    select(location, species, generation, "GHG.emis" = GHG.emissions.of.bioenergy) %>%
    group_by(generation) 
  
  # produce a clipped set of values for 2nd generation biofuels, where values > gasoline filtered out and calculate summary statistics
  ethanol_optimisitic <- ethanol %>%
                         filter(GHG.emis < 91.5, generation == "2G") %>%
                         summarise(mean = mean(GHG.emis),
                                   median = median(GHG.emis),
                                   min = min(GHG.emis),
                                   max = max(GHG.emis),
                                   n = n())
  
  
  
###### CALCULATE EMISSIONS FROM FREIGHT ######
  
  freight_emissions <- freight %>%
                       full_join(CI_elec %>% filter(scenario != "iea2"), by = c("scenario", "year")) %>%
                       rename("CI.elec" = value) %>%
                       mutate(CI.PH = (CI.elec + ef_tot$liquid) / 2) %>%
                       full_join(CI_h2 %>% filter(scenario != "iea2"), by = c("scenario", "year"))
  
  freight_emissions <- freight_emissions %>%
                       mutate(emi = emi_freight(freight_emissions)) %>%
                       filter(year == "2050", !grepl("H2|EV|PH", type)) %>%
                       group_by(scenario, year) %>%
                       summarise(emi = sum(emi))
  
  
  
###### CALCULATE EFFORT REQUIRED TO ACHIEVE A WEDGE ######
  
  ### vehicle efficiency [l/100km in 2050]
  wedge_v_eff <- psg_trans_agg %>%
                 filter(mode == "car", type == "ICE.liquid", year == "2050", scenario != "iea2") %>%
                 mutate(target = (CO2.tot - w_target) / (vkm * ef_tot$liquid)) %>%
                        select(scenario, "baseline" = efficiency, target) %>%
                        mutate(baseline = to_l100km(baseline),
                               target = to_l100km(target))

  cat('Improve efficiency of cars:', range(wedge_v_eff$target), 'litres per 100km acheives a wedge\n')


  ### avoided travel [pkm]
  wedge_avoid <- psg_trans_agg %>%
                 filter(mode == "car", type == "ICE.liquid", year == "2050", scenario != "iea2") %>%
                 mutate(target = w_target / CO2.pkm) %>%
                 select(scenario, target)

  cat('Avoid car travel or walk/cycle:', range(wedge_avoid$target) / 1e12, 'trillion pkm acheives a wedge\n')


  ### electric vehicles [pkm]
  wedge_EVs <- psg_trans_agg %>% 
               filter(mode == "car", type %in% c("EV", "ICE.liquid"), year == "2050") %>%
               select(scenario, type, CO2.pkm) %>%
               pivot_wider(names_from = type, values_from = CO2.pkm) %>%
               mutate(target = w_target / (ICE.liquid - EV[1])) %>%
               select(scenario, target) %>%
               filter(scenario != "iea2")

  cat('Deploy more electric vehicles:', range(wedge_EVs$target) / 1e12, 'trillion pkm acheives a wedge\n')


  ### public transport [pkm]
  wedge_public <- psg_trans_agg %>% 
                  filter(mode %in% c("bus", "train"), year == '2050') %>%
                  group_by(scenario) %>%
                  summarise(pt = weighted.mean(CO2.pkm, pkm)) %>%
                  cbind(psg_trans_agg %>% filter(mode == "car", type == "ICE.liquid", year == "2050") %>% select("car" = CO2.pkm)) %>%
                  mutate(target  = (w_target / (car - pt[1]))) %>%
                  select(scenario, target) %>%
                  filter(scenario != "iea2")

  cat('Use more public transport:', range(wedge_public$target) / 1e12, 'trillion pkm acheives a wedge\n')


  ### reduced air travel [pkm]
  wedge_air <- psg_trans_agg %>% 
               filter(mode == "plane", type %in% c("short.fuel", "long.fuel"), year == "2050", scenario != "iea2") %>%
               group_by(scenario) %>%
               summarise(CO2.pkm = weighted.mean(CO2.pkm, pkm)) %>%
               mutate(target = w_target / CO2.pkm) %>%
               select(scenario, target)

  cat('Avoid taking flights:', range(wedge_air$target) / 1e12, 'trillion pkm acheives a wedge\n')


  ### biofuels [pkm]
  # calculate the amount of ethanol that must be produced [TWh] to achieve a wedge
  bio_energy <- w_target / ((ef_tot$liquid) - to_Gt_TWh(ethanol_optimisitic$median))
  
  # calculate how many pkm this equates to 
  wedge_bio <- psg_trans_agg %>%
               filter(mode == "car", type == "ICE.liquid", year == "2050", scenario != "iea2") %>%
               mutate(pkm.TWh = efficiency / occupancy,
                      target = bio_energy / pkm.TWh) %>%
               select(scenario, target)

  cat('Deploy more biofuels:', range(wedge_bio$target) / 1e12, 'trillion pkm acheives a wedge\n')


  ### freight [% of 2050 freight emissions]
  wedge_freight <- freight_emissions %>%
                   summarise(target = w_target / emi) %>%
                   select(scenario, target)

  cat('Decarbonise surface freight:', range(wedge_freight$target) * 100, '% reduction in 2050 freight emissions achieves a wedge\n')
