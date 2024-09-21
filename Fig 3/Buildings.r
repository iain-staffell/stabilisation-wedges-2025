#########################################################################
##  
##  Calculate the 3 buildings wedges
##  
##  Instructions:
##    change the path on line 17 to be the base of this replication package
##    (where readme.md exists)
##  
##  Outputs:
##    wedge_fabric
##    wedge_hp  
##    wedge_stoves
##  
##  

  # set working directory
  setwd('C:/stabilisation-wedges-2025/')
  
  

###### LOAD PACKAGES AND SOURCE FILES ######
  
  library(tidyverse)

  # load scenario data from the Global Calculator
  build <- readRDS("Fig 3/Inputs/buildings_data.rds")
  
  # load cooling data from IEA Future of cooling (this is replicated for each scenario)
  foc <- readRDS("Fig 3/Inputs/future_of_cooling.rds")
  
  # load carbon intensity of electricity [GtCO2e/TWh] - make scenario names upper case
  CI_elec <- read.csv("Fig 3/Inputs/grid_carbon_intensity.csv" , check.names = FALSE, row.names = 1) %>%
             mutate(scenario = toupper(scenario))
  
  # CO2 equivalent emission factors [GtCO2e/TWh]
  ef <- readRDS("Fig 3/Inputs/emission_factors.RDS")
  
  # overheads from fuel production from IPCC [AR5 applied to combustion emissions factors [%]
  ef_oh <- ef * c(.075, .20, .25)
  
  # combustion emissions plus overheads 
  ef_tot <- ef + ef_oh 
  
  # district heating emissions factor in GtCO2e per TWh from IEA
  ef_dh <- 225 / 1e6 # full range is 150-300  
  
  # biomass emissions factor [GtCO2/TWh] derived from IPCC (median emissions per kWh electricity * efficiency) 
  ef_bio <- 230 * 0.31 / 1e6
  
  # higher heating value of wood from Greishop et al. [MJ/kg of air dry wood]
  HHV_wood <- 15
  
  # emissions factors for stove-fuel combinations [g CO2e/kg of wood] from Greishop et al.
  ef_stoves <- readRDS("Fig 3/Inputs/stove_emissions.rds")
  
  # efficiency of stoves [%]
  eff_stoves <- data.frame(wood_trad = 0.20, wood_fan = 0.4, wood_fan_eff = 0.5)
  
  # fraction of non-renewable biomass from Bailis et al. (the average of their range)
  fnrb <- mean(c(0.27, 0.34))
  
  # target emissions savings in 2050 for a wedge [GtCO2e/year]
  w_target <- 2
  
  

###### BESPOKE FUNCTIONS ######
  
  # calculate emissions from space heating
  emissions_heat <- function (data) {
    
    emissions <- list()
    
    for (i in 1:nrow(data)){
      
      if (grepl('liquid', data[i, "type"])) {
        
        emissions[i] <- data[i, "energy_demand_TWh"] * ef_tot$liquid
        
      } else if (grepl('gas', data[i, "type"])) {
        
        emissions[i] <- data[i, "energy_demand_TWh"] * ef_tot$gas
        
      } else if (grepl('solid', data[i, "type"])) {
        
        emissions[i] <- data[i, "energy_demand_TWh"] * ef_tot$solid
        
      } else if (grepl('heatpump|electricity', data[i, "type"])) {
        
        emissions[i] <- data[i, "energy_demand_TWh"] * data[i, "CI_elec"]
        
      } else if (grepl('solar', data[i, "type"])) {
        
        emissions[i] <- 0
        
      } else if (grepl('biomass', data[i, "type"])) {
        
        emissions[i] <- data[i, "energy_demand_TWh"] * ef_bio 
        
      } else if (grepl('chp', data[i, "type"])) {
        
        emissions[i] <- data[i, "energy_demand_TWh"] * ef_tot$gas 
        
      } else if (grepl('district', data[i, "type"])) {
        
        emissions[i] <- data[i, "heat_demand_TWh"] * ef_dh  
      }
    }
    as.numeric(unlist(as.character(emissions)))
  }
  
  

###### SCALE COOLING DEMAND ACCORDING TO FOC AND CALCULAT EHATING AND COOLING EMISSIONS ######
  
  # extract cooling data for the Global Calculator scenarios
  cool_gc <- build %>% 
             filter(key == "cool", measure == "demand_TWh", scenario != "IEA2", year == "2050", grepl("airconditioners", type))
  
  # extract residential cooling demand and sum demand across urban and rural areas
  res_gc <- cool_gc %>%
            filter(!grepl("non", type)) %>%
            group_by(scenario) %>%
            summarise("gc_res" = sum(value))
  
  # extract non-residential cooling demand 
  nres_gc <- cool_gc %>%
               filter(grepl("non", type)) %>%
               select(scenario, "gc_nres" = value)
  
  # extract data for 2050 from FOC scenario
  cool_foc <- foc %>% filter(measure == "demand_TWh", scenario != "IEA2", year == "2050")
  
  # pick out just residential cooling demand
  res_foc <- cool_foc %>% 
             filter(!grepl("non", type)) %>%
             select(scenario, "foc_res" = value)
  
  # pick out just non-residential cooling demand
  nres_foc <- cool_foc %>% 
              filter(grepl("non", type)) %>%
              select(scenario, "foc_nres" = value)
  
  # merge cooling data together 
  cool_all <- Reduce(merge, list(res_gc, nres_gc, res_foc, nres_foc)) %>% filter(scenario != "RCP6")
  
  # calculate the average useful energy demand across scenarios (excluding RCP6.0)
  cool_av <- apply(cool_all[2:5], 2, mean)
  
  # calculate scaling factors for residential and non-residential buildings
  res_sf <- cool_av[3] / cool_av[1]
  nres_sf <- cool_av[4] / cool_av[2]
  
  # get emissions factors for electricity in desired format
  CI_elec_ <- CI_elec %>% select(scenario, "2050") %>% filter(scenario != "IEA2")
  
  # scale useful energy demand, convert to final energy demand, and calculate emissions 
  scale_data <- cool_all %>% 
                mutate(scale_res = gc_res * res_sf,
                       scale_nres = gc_nres * nres_sf) %>%
                pivot_longer(c(2:7), values_to = "ued") %>%
                mutate(fed = ifelse(grepl("gc_res", name), ued / 2,
                             ifelse(grepl("gc_nres", name), ued / 2.5,
                             ifelse(grepl("nres", name), ued / 5.34, ued / 4.88)))) %>%
                merge(CI_elec_) %>%
                rename("CI_elec" = "2050") %>%
                mutate(emissions = fed * CI_elec) 
  
  # summarise emissions from cooling
  emi_cool <- scale_data %>%
                 filter(grepl("scale", name)) %>%
                 group_by(scenario) %>%
                 summarise(emissions = sum(emissions))
  
  # filter out space heating
  heat <- build %>% 
          filter(key == "space", measure %in% c("heat_demand_TWh", "energy_demand_TWh"), year == "2050", scenario %in% c("IEA6", "IEA4", "RCP8")) %>%
          pivot_wider(names_from = measure, values_from = value) %>%
          mutate(energy_demand_TWh = energy_demand_TWh * -1) %>%
          merge(CI_elec_) %>% rename("CI_elec" = "2050")
  
  # calculate emissions from space heating         
  emi_heat <- heat %>% 
              mutate(emissions = emissions_heat(heat)) %>%
              group_by(scenario) %>%
              summarise(emissions = sum(emissions))

  
  # extract building heat loss values from build dataframe [GW / (M ha * C)]
  heat_loss <- build %>% 
               filter(measure == "loss", year == "2050", scenario %in% c("IEA6", "IEA4", "RCP8")) %>%
               group_by(scenario) %>%
               summarise(u_val = mean(value))
  
  # combine heating and cooling and convert heat loss into W / (m2 * K)
  heat_cool <- rbind(emi_heat, emi_cool) %>%
               group_by(scenario) %>%
               summarise(emissions = sum(emissions)) %>%
               merge(heat_loss) %>%
               mutate(u_val = u_val / 10)
  
  
  
###### EXTRACT DATA FOR HEAT PUMPS AND DISPLACED HEATING SYSTEMS ######
  
  # filter build data for space and water heating in buildings with access to electricity
  # summarise efficiency and energy demand and calculate emissions
  heating <- build %>%
             filter(key %in% c("space", "water"), !grepl(".no.elec", type), year == "2050") %>%
             pivot_wider(names_from = measure, values_from = value) %>%
             mutate(energy_demand_TWh = energy_demand_TWh * -1) %>%
             separate(type, into = c("heater", "tech", "other", "other2", "other3"), extra = "merge") %>%
             unite(col = "other", c("other", "other2", "other3"), sep = "_") %>%
             mutate(other = gsub("boiler_|_space|.space|_water|.water|_NA", "", other)) %>%
             select(scenario, key, tech, "region" = other, year, efficiency, "ued" = heat_demand_TWh, "fed" = energy_demand_TWh) %>%
             group_by(scenario, tech, year) %>%
             summarise(efficiency = mean(efficiency),
                       energy_demand_TWh = sum(fed),
                       heat_demand_TWh = sum(ued),
                       efficiency2 = heat_demand_TWh / energy_demand_TWh) %>%
             mutate(efficiency = ifelse(is.na(efficiency)|is.nan(efficiency), efficiency2, efficiency)) %>%
             select(-efficiency2) %>%
             rename("type" = tech) %>%
             ungroup() %>%
             merge(CI_elec %>% select(scenario, "2050")) %>% rename("CI_elec" = "2050") 
  
  heating <- heating %>%
             mutate(emissions = emissions_heat(heating))

  # calculate the carbon intensity of heat provision for all technologies
  CI_heat <-  heating %>%
              group_by(scenario, type, year) %>%
              summarise(fed = sum(energy_demand_TWh),
                        ued = sum(heat_demand_TWh),
                        emissions = sum(emissions)) %>%
              mutate(CI_heat = emissions / ued) %>%
              ungroup()
  
  # calculate weighted average carbon intensity of heat provision for all 'displaced' technologies
  CI_displaced <- CI_heat %>%
                  group_by(scenario, year) %>%
                  filter(!grepl(c("district|solar|heatpump"), type)) %>%
                  summarise(CI_d = weighted.mean(CI_heat, ued)) %>%
                  ungroup()
 
  # extract carbon intensity of heat pumps from CI_heat
  CI_hp <- CI_heat %>%
           filter(type == "heatpump") %>%
           select(scenario, year, "fed_hp" = fed, ued_hp = "ued", "CI_hp" = CI_heat) %>%
           mutate("scop" = ued_hp / fed_hp)

  # merge carbon intensity of displaced heating and heat pumps
  CI_diff <- merge(CI_displaced, CI_hp) 
  
  
  
###### CALCULATE EMISSIONS FROM TRADITIONAL AND CLEAN COOKSTOVES ######    

  # calculate total emissions accounting for the fraction of renewable biomass [t CO2e/kg of wood] 
  ef_stoves_ <- ef_stoves %>% 
                pivot_longer(2:4, names_to = "stove") %>%
                mutate(value = ifelse(pollutant == "CO2", value * fnrb, value)) %>%
                group_by(stove) %>%
                summarise(ef = sum(value) / 1e6) %>%
                pivot_wider(names_from = stove, values_from = ef)
  
  # extract and summarise cooking data from scenarios
  # calculate useful energy demand for each stove in 2011
  # assume that all stoves are traditional and cooking energy (ued) does not decrease over time
  # calculate annual emissions per traditional and improved stove [tCO2e/yr]
  # substitute in the actual number of households in 2020 reliant on cookstoves from ESMAP
  cook <- build %>% 
          filter(key == "cook", scenario != "IEA2", year == "2011") %>%
          pivot_wider(names_from = measure, values_from = value) %>%
          rename("fed" = energy_demand_TWh, "ued" = demand_TWh) %>%
          mutate(efficiency = eff_stoves$wood_trad,
                 fed = ued / efficiency) %>%
          group_by(scenario) %>% 
          summarise(households = sum(households),
                    fed = sum(fed),
                    ued = sum(ued),
                    efficiency = mean(efficiency)) %>%
          mutate(ued_stove = ued[1] * 3.6e9 / households[1], # [MJ/year]
                 households = 907 * 1e6,
                 ued = ued_stove * households,
                 fed = ued / efficiency,
                 wood_Gt = fed / HHV_wood / 1e12,
                 wood_stove = wood_Gt / households * 1e12,
                 emissions_Gt = wood_Gt * ef_stoves_$wood_trad / 1000,
                 emissions_trad = ued_stove / eff_stoves$wood_trad / HHV_wood * ef_stoves_$wood_trad,
                 emissions_clean = ued_stove / eff_stoves$wood_fan_eff / HHV_wood * ef_stoves_$wood_fan_eff, 
                 emissions_diff = emissions_trad - emissions_clean) %>% # [tCO2e/year]
          ungroup()
  
  
  
###### CALCULATE THE EFFORT REQUIRED TO ACHIEVE A WEDGE FROM EACH STRATEGY ######
  
  # calculate the improvement in fabric efficiency by 2050 required to achieve a wedge 
  wedge_fabric <- heat_cool %>% mutate(improvement = w_target / emissions,
                                       target = u_val * (1 - improvement))
  
  cat('Reduce building heat transfer:', range(wedge_fabric$target), 'W/m2/*C heat transfer coefficient acheives a wedge\n')


  # calculate the amount of 'conventional' heating that must be displaced with heat pumps in 2050 to achieve a wedge
  wedge_hp <- CI_diff %>%
    mutate(target_ued = w_target / (CI_d - CI_hp[1]),
           target_fed = target_ued / scop[1]) %>%
    select(scenario, target_ued, target_fed) %>%
    filter(scenario != "IEA2")

  cat('Install more heat pumps:', range(wedge_hp$target_ued), 'TWh useful energy demand met by heat pumps acheives a wedge\n')


  # calculate how many stoves need to be displaced immediately to achieve a wedge
  # note that more traditional stoves need to be displaced than are currently in use
  # additional stoves that come into use between 2020-2050 must also be clean
  wedge_stoves <- cook %>% 
            select(scenario, emissions_diff) %>%
            mutate(target = w_target * 1e9 / 2 / emissions_diff)

  cat('Deploy more clean stoves:', range(wedge_stoves$target), 'traditional stoves must be diplaced immediately to achieve a wedge\n')
