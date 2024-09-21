#########################################################################
##  
##  Calculate the 9 power wedges
##  
##  Instructions:
##    change the path on line 20 to be the base of this replication package
##    (where readme.md exists)
##  
##  Outputs:
##    wedge_res
##    wedge_c2g
##    wedge_CCS_coal
##    wedge_CCS_gas
##    wedge_BECCS
##    wedge_DAC
##  
##  

  # set working directory
  setwd('C:/stabilisation-wedges-2025/')
  
  
  
###### LOAD PACKAGES AND SOURCE FILES ######
   
  library(tidyverse)
  
  
  
###### PREPARE INPUT DATA ######

  # scenario data on electricity output and fuel input to power stations [TWh]
  elec <- readRDS("Fig 3/Inputs/electricity_output.rds")
  fuel <- readRDS("Fig 3/Inputs/fuel_input.rds")
  
  # combustion emissions factors for coal (solid), oil (liquid) and gas [GtCO2e/TWh]
  ef <- readRDS("Fig 3/Inputs/emission_factors.rds")

  # emissions overheads for fuel production from IPCC [%]
  ef_oh <- ef * c(0.075, 0.20, 0.25)
  
  # combustion emissions plus overheads [GtCO2e/TWh]
  ef_tot <- ef + ef_oh 
  
  # bioenergy emissions credits allocated to power sector [GtCO2e/year]
  bio_credit <- readRDS("Fig 3/Inputs/bio_credit.rds")
  
  # assumed capture rate of CCS plants [%]
  c_rate <- 0.90
  
  # assumed efficiency penalties for post-combustion capture [%]
  # midpoint of the ranges from House et al. and Budinis et al.
  coal_pen <- mean( c(0.08 + 0.155) )
  gas_pen  <- mean( c(0.06 + 0.115) )
  
  # BECCS supply chain emissions with LUC [kgCO2e/tdm]
  ef_BECCS <- readRDS("Fig 3/Inputs/BECCS_emissions_factors.rds")
  
  # efficiency of BECCS plant [% HHV]
  # 100% cofiring and 90% capture from Bui et al.
  BECCS_eff <- 0.38
  
  # average carbon content of biomass [% dm]
  # average value from Fajardy et al.
  bio_C <- 0.475 * 3.67 
  
  # average HHV of biomass [kWh/tdm]
  # average value from Fajardy et al.
  bio_HHV <- (18.5 * 1000) / 3.6
  
  # DAC heat requirement [kWh/tonne]
  DAC_h <- 4.8 * 278
  
  # DAC electricity requirement [kWh/tonne]
  DAC_e <- 1 * 278
  
  # DAC heat pump COP
  COP <- 3
  
  # target emissions savings in 2050 for a wedge [GtCO2e/year]
  w_target <- 2
  
  
  
###### BESPOKE FUNCTIONS ######
  
  # apply emissions factors to fuel inputs to give emissions per plant
  get_emissions <- function (data) {
    
    m <- data.frame(matrix(nrow = 15, ncol = 9))
    
    for(i in 1:nrow(data)) {
      if (i == 15) {
        m[i, ] <- data[i, ] * 0
      } else if (i == 14 | i == 13) {
        m[i, ] <- data[i, ] * ef$gas * (1 - c_rate) + data[i, ] * ef_oh$gas
      } else if (i == 11 | i == 12) {
        m[i, ] <- data[i, ] * ef$liquid * (1 - c_rate) + data[i, ] * ef_oh$liquid
      } else if (i == 10 | i == 9 | i == 8) {
        m[i, ] <- data[i, ] * ef$solid * (1 - c_rate) + data[i, ] * ef_oh$solid
      } else if (i == 6 | i == 7) {
        m[i, ] <- data[i, ] * ef_tot$gas
      } else if (i == 4 | i == 5) {
        m[i, ] <- data[i, ] * ef_tot$liquid
      } else {
        m[i, ] <- data[i, ] * ef_tot$solid
      }
    }
    m <- cbind(fuel$iea2.fuel$plant, m)
    colnames(m) <- c("plant", 2011, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050)
    m
  }
  
  # extract data on unabated fossil fuels and put it in a dataframe
  prepare_df <- function (data, name) {
                do.call(rbind.data.frame, data) %>% 
                rename("scenario" = baseline) %>%
                select(-variable) %>%
                filter(grepl(c("unabated"), plant)) %>%
                pivot_longer(cols = starts_with("20"), names_to = "year", values_to = name)
  }
  
  # calculate the carbon intensity of BECCS plants, assuming 100% co-firing
  get_CI_BECCS <- function(data) {
    
    CI <- (1 / BECCS_eff / bio_HHV * bio_C * -c_rate + ((1 / BECCS_eff / bio_HHV) * (data / 1000)))
    CI
  }
  
  
  
###### CALCULATE EMISSIONS INTENSITY OF THE GRID IN EACH SCENARIO ######
  
  # apply get_emissions() to give emissions per plant [GtCO2e]
  emissions <- lapply(fuel, `[`, 4:12) %>%
              lapply(get_emissions)
  
  # sum emissions per fuel and subtract bioenergy emissions savings to give total power sector emissions [GtCO2e]
  sum_emissions <- do.call(rbind, lapply(lapply(emissions, `[`, 2:10),colSums)) - bio_credit
  
  # sum all electricity generation [TWh]
  sum_elec <- do.call(rbind, lapply(lapply(elec, `[`, 4:12), colSums))
  
  # calculate carbon intensity of the grid [GtCO2e/TWh]
  grid_CI <- as.data.frame(sum_emissions / sum_elec) %>% # * 1e6 # for g per kWh
            rownames_to_column(var = "scenario")
  
  # save data for grid CI with overheads
  write.csv(grid_CI, 'Fig 3/Inputs/grid_carbon_intensity.csv')
  
  
  ###### CALCULATE EMISSIONS INTENSITY AND EFFICIENCY OF EACH TYPE OF FOSSIL PLANT ######
  
  # prepare emissions data
  f_emissions <- do.call(rbind.data.frame, emissions) %>% 
                rownames_to_column(var = "scenario") %>% 
                separate(scenario, c("scenario", NA), ".f") %>%
                filter(grepl("unabated", plant)) %>%
                pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "emi")
  
  # turn elec and fuel into dataframes with the same dimensions as f_emissions
  f_elec <- prepare_df(elec, "out")
  f_fuel <- prepare_df(fuel, "inp")
  
  # join f_elec, f_fuel and f_emissions, tidy, and calculate carbon intensity and and efficiency
  f_df <- left_join(f_elec, f_fuel, by = c("year", "scenario", "plant")) %>% left_join(f_emissions, by = c("year", "scenario", "plant")) %>%
          separate(plant, c("unabated", "fuel", "type"), c("\\.")) %>% select(-unabated) %>%
          mutate(CI = emi / out, eff = out / inp)
  
  # calculate the carbon intensity of average coal, oil and gas plants, weighted by share of generation (GtCO2e / TWh)
  ff_av <- f_df %>%
          group_by(scenario, year, fuel) %>%
          summarise(CI = weighted.mean(CI, out), 
                    eff = sum(out) / sum(inp),
                    out = sum(out),
                    inp = sum(inp),
                    emi = sum(emi)) %>%
                    ungroup()
  
  # calculate average fossil (coal, gas and oil) carbon intensity 
  f_av <- f_df %>%
              group_by(scenario, year) %>%
              summarise(CI = weighted.mean(CI, out)) %>%
              ungroup()
  
  
  
###### CALCULATE THE CARBON INTENSITY OF CCS PLANTS ######
  
  # calculate the efficiency and carbon intensity of combined cycle gas plants and ultracritical coal plants with CCS [GtCO2e/TWh]
  CCS_eff <- f_df %>%
                  filter(fuel %in% c("gas", "solid"), type %in% c("CC", "ultra")) %>%
                  mutate(eff_CCS = ifelse(fuel == "gas", eff - gas_pen, eff - coal_pen),
                        CI_CCS = ifelse(fuel == "gas", 1 / eff_CCS * ef$gas * (1 - c_rate) + (1 / eff_CCS) * ef_oh$gas,
                                        1 / eff_CCS * ef$solid * (1 - c_rate) + (1 / eff_CCS) * ef_oh$solid)) %>%
                  ungroup() %>%
                  select(scenario, year, fuel, eff, CI, eff_CCS, CI_CCS)
  
  # calculate the efficiency and carbon intensity of average coal and gas  plants once fitted with CCS [GtCO2e / TWh]
  CCS_av <- ff_av %>%
                  filter(fuel != "liquid") %>%
                  mutate(eff_CCS = ifelse(fuel == "gas", eff - gas_pen, eff - coal_pen),
                          CI_CCS = ifelse(fuel == "gas", 1 / eff_CCS * ef$gas * (1 - c_rate) + (1 / eff_CCS) * ef_oh$gas,
                                          1 / eff_CCS * ef$solid * (1 - c_rate) + (1 / eff_CCS) * ef_oh$solid)) %>%
                  ungroup() %>%
                  select(scenario, year, fuel, eff, CI, eff_CCS, CI_CCS)
  
  
  
###### CALCULATE CARBON INTENSITY OF BECCS PLANTS ######
  # calculate BECCS CI, average across regions and average switchgrass and miscanthus to grasses [GtCO2e/TWh]
  BECCS_CI <- ef_BECCS %>%
              mutate(CI = get_CI_BECCS(mean_w_LUC),
                    feedstock = ifelse(feedstock %in% c("misc", "swch"), "gras", feedstock)) %>%
              group_by(feedstock) %>%
              summarise(CI = mean(CI)) 
  
  
  
###### CALCULATE EFFORT FOR EACH POWER SECTOR WEDGE IN 2050 in TWh ######
  
  ### RES and nuclear - assuming average fossil power generation is displaced
  wedge_res <- filter(f_av, year == "2050" & scenario != "iea2") %>% 
               mutate(target = w_target / CI) %>%
               select(-year, -CI)

  cat('Install more wind/solar/nuclear:', range(wedge_res$target), 'TWh annual generation acheives a wedge\n')


  ### coal to gas switching - assuming that CCGT displaces average coal 
  wedge_c2g <- cbind(
               filter(ff_av, year == "2050" & fuel == "solid" & scenario != "iea2") %>% select(scenario, "CI_coal" = CI),
               filter(f_df, year == "2050" & type == "CC" & scenario != "iea2") %>% select("CI_gas" = CI)) %>%
               mutate(target = w_target / (CI_coal - CI_gas)) %>% select(scenario, target)

  cat('Switch from coal to gas plants:', range(wedge_c2g$target), 'TWh annual generation acheives a wedge\n')


  ### CCS retrofitting
  # assuming CCS is retrofit to CCGTs
  wedge_CCS_coal <- filter(CCS_av, year == "2050", fuel == "solid", scenario != "iea2") %>%
                    mutate(CI_diff = CI - CI_CCS,
                           target = w_target / CI_diff) %>%
                    select(scenario, target)

  cat('Retrofit coal plants with CCS:', range(wedge_CCS_coal$target), 'TWh annual generation acheives a wedge\n')


  # assuming CCS is retrofit at average coal plants
  wedge_CCS_gas <- filter(CCS_eff, year == "2050", fuel == "gas", scenario != "iea2") %>%
                   mutate(CI_diff = CI - CI_CCS,
                          target = w_target / CI_diff) %>%
                   select(scenario, target) 

  cat('Retrofit gas plants with CCS:', range(wedge_CCS_gas$target), 'TWh annual generation acheives a wedge\n')


  ### BECCS - assume that average fossil power generation is displaced
  wedge_BECCS <- filter(f_av, year == "2050" & scenario != "iea2") %>%
                 mutate(target_grass = w_target / (CI - BECCS_CI$CI[1]),
                        target_straw = w_target / (CI - BECCS_CI$CI[2])) %>%
                 select(-CI, -year)

  cat('Power BECCS with crops:', range(wedge_BECCS$target_grass), 'TWh annual generation acheives a wedge\n')
  cat('Power BECCS with waste:', range(wedge_BECCS$target_straw), 'TWh annual generation acheives a wedge\n')


  ### DAC - assuming electricity input from IEA2DS scenarios
  # carbon efficiency [%]
  DAC_CE <- 1 - (DAC_h / COP + DAC_e) * grid_CI[1,9]
  
  # electricity required for a wedge
  wedge_DAC <- w_target / DAC_CE * (DAC_h / COP + DAC_e)

  cat('Deploy direct air capture:', range(wedge_DAC), 'TWh annual consumption acheives a wedge\n')
