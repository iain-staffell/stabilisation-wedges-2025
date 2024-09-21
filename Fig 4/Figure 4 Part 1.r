#########################################################################
##  
##  Extract the values from IAMC database for variables representing our 
##  wedge strategies under 'current policy' and 'decarbonisation' scenarios
##
##  Instructions:
##    change the path on line 16 to be the base of this replication package
##    (where readme.md exists)
##  
##  Outputs:
##    a set of CSV files with results to process in `Figure 4 Part 2.r`
##  
##  

	# set working directory
	setwd('C:/stabilisation-wedges-2025/')

	library(reshape2)

	# load the ar6 database and helper functions
	source('blam_library.r')
	source('ar6_library.r')


	# setup our definition of our two scnearios
	# 3 wedges either side of 'current policies' and 'decarbonisation' scenarios
	# measured in Gt emissions in 2050
	e_cool = 9.784 +    0 + 2*c(-3,3)
	e_warm = 9.784 + 2*20 + 2*c(-3,3)





#####
## ##  FIGURE 4 (interim data for individual strategies)
#####

	# define the IAMC variable names that we wish to access
	wedges = list(

		wind = 'Secondary Energy|Electricity|Wind',
		solar = 'Secondary Energy|Electricity|Solar',
		nuclear = 'Secondary Energy|Electricity|Nuclear',
		gas_ccs = 'Secondary Energy|Electricity|Gas|w/ CCS',
		coal_ccs = 'Secondary Energy|Electricity|Coal|w/ CCS',
		h2_green = 'Secondary Energy|Hydrogen|Electricity',
		h2_blue = 'Secondary Energy|Hydrogen|Fossil|w/ CCS',
		evs = 'Final Energy|Transportation|Electricity',
		biofuel = 'Final Energy|Transportation|Liquids|Bioenergy',
		ind_c_ccs = 'Carbon Sequestration|CCS|Fossil|Energy|Demand|Industry',
		ind_p_ccs = 'Carbon Sequestration|CCS|Fossil|Industrial Processes',
		hfc = 'Emissions|HFC',
		ch4_energysupply = 'Emissions|CH4|Energy|Supply',
		afforest = 'Carbon Sequestration|Land Use|Afforestation',
		beccs = 'Secondary Energy|Electricity|Biomass|w/ CCS',
		daccs = 'Carbon Sequestration|Direct Air Capture',
		ew = 'Carbon Sequestration|Enhanced Weathering',
		meat = 'Food Demand|Livestock',
		freight = 'Emissions|CO2|Energy|Demand|Transportation|Freight'

	)

	for (i in 1:length(wedges))
	{
		# the shortname for this wedge
		n = names(wedges)[i]

		# the iamc variable name
		w = wedges[[i]]

		# extract all variables under the high and low temperature scenarios
		x = calculate_scenario_difference(data, w)

		# save the summary, used in the next step of the analaysis - a list giving:
		#   `historical`: values for 2020 in all IAMC scenarios
		#   `high`: values for 2050 in all IAMC scenarios with 2050 emissions in the range of e_warm (current policy)
		#   `lo`: values for 2050 in all IAMC scenarios with 2050 emissions in the range of e_cool (decarbonisation)
		list_to_csv(x$summary, 'Fig 4/iamc_data/' %&% n %&% '.csv')

		# note this also produces x$full, which gives the entire slice of the IAMC database for this variable
		# but this is only provided for info, it's not used in the remaining analysis...
		
		# save the scatter & boxplot summary for Figure S6-S10
		good_png('Fig 4/iamc_data/' %&% n %&% '.png', zoom=1.88)
	}



#####
## ##  FIGURE 4 (interim data for sector totals - calculated from all scenarios)
#####

	# define the IAMC variable names that we wish to access
	sectors = list(
		ccs_total = 'Carbon Sequestration|CCS',
		ccs_biomass = 'Carbon Sequestration|CCS|Biomass',
		ccs_fossil = 'Carbon Sequestration|CCS|Fossil',
		co2_total = 'Emissions|CO2',
		co2_energy = 'Emissions|CO2|Energy',
		co2_energy_indprocess = 'Emissions|CO2|Energy and Industrial Processes',
		co2_energysupply = 'Emissions|CO2|Energy|Supply',
		co2_afolu = 'Emissions|CO2|AFOLU',
		co2_ind = 'Emissions|CO2|Energy|Demand|Industry',
		co2_rescom = 'Emissions|CO2|Energy|Demand|Residential and Commercial',
		co2_transp = 'Emissions|CO2|Energy|Demand|Transportation',
		co2_electricity = 'Emissions|CO2|Energy|Supply|Electricity',
		co2_indprocess = 'Emissions|CO2|Industrial Processes',
		n2o_total = 'Emissions|N2O',
		n2o_afolu = 'Emissions|N2O|AFOLU',
		n2o_energy = 'Emissions|N2O|Energy',
		n2o_indprocess = 'Emissions|N2O|Industrial Processes',
		ch4_total = 'Emissions|CH4',
		ch4_energy = 'Emissions|CH4|Energy',
		ch4_afolu = 'Emissions|CH4|AFOLU',
		ch4_ind = 'Emissions|CH4|Energy|Demand|Industry',
		ch4_rescom = 'Emissions|CH4|Energy|Demand|Residential and Commercial',
		ch4_transp = 'Emissions|CH4|Energy|Demand|Transportation',
		ch4_indprocess = 'Emissions|CH4|Industrial Processes'
	)

	for (i in 1:length(sectors))
	{
		# the shortname for this sector
		n = names(sectors)[i]

		# the iamc variable name
		w = sectors[[i]]

		# extract all variables under the high and low temperature scenarios
		x = calculate_scenario_difference(data, w)

		# save the summary, used in the next step of the analaysis - a list giving:
		#   `historical`: values for 2020 in all IAMC scenarios
		#   `high`: values for 2050 in all IAMC scenarios with 2050 emissions in the range of e_warm (current policy)
		#   `lo`: values for 2050 in all IAMC scenarios with 2050 emissions in the range of e_cool (decarbonisation)
		list_to_csv(x$summary, 'Fig 4/iamc_data/' %&% n %&% '.csv')

		# note this also produces x$full, which gives the entire slice of the IAMC database for this variable
		# but this is only provided for info, it's not used in the remaining analysis...
		
		# save the scatter & boxplot summary for Figure S6-S10
		good_png('Fig 4/iamc_data/' %&% n %&% '.png', zoom=1.88)
	}



#####
## ##  FIGURE 4 (interim data for sector totals - calculated from only scenarios reporting everything)
#####

	# define the IAMC variable names that we wish to access
	sectors = list(
		co2_ind = 'Emissions|CO2|Energy|Demand|Industry',
		co2_rescom = 'Emissions|CO2|Energy|Demand|Residential and Commercial',
		co2_energy = 'Emissions|CO2|Energy|Supply',
		co2_indprocess = 'Emissions|CO2|Industrial Processes',
		ch4_total = 'Emissions|CH4',
		ch4_afolu = 'Emissions|CH4|AFOLU',
		ch4_energy = 'Emissions|CH4|Energy'
	)



	# here we restrict ourselves to scenarios (i.e. combinations of 
	# model and run) which report on ALL of the required sectors to 
	# avoid inconsistency thorugh omission

	# temp function to find all reporting scenarios
	getscen = function(variable)
	{
		id = data$id[ data$Variable == variable]
		sort(unique(id))
	}

	# identify the reporting scenarios for all sectors
	sector_scenarios = lapply(sectors, getscen)

	# now identify the common ones across all sectors
	common_scenarios = Reduce(intersect, sector_scenarios)

	# shrink down the IAMC database to just these
	keep = data$id %in% common_scenarios
	data = data[keep, ]



	for (i in 1:length(sectors))
	{
		# the shortname for this sector
		n = names(sectors)[i]

		# the iamc variable name
		w = sectors[[i]]

		# these ones we don't overwrite
		if (n %in% c('ch4_afolu', 'ch4_energy', 'ch4_total', 'co2_energy', 'co2_ind', 'co2_indprocess', 'co2_rescom'))
			next

		# extract all variables under the high and low temperature scenarios
		x = calculate_scenario_difference(data, w)

		# save the summary, used in the next step of the analaysis - a list giving:
		#   `historical`: values for 2020 in all IAMC scenarios
		#   `high`: values for 2050 in all IAMC scenarios with 2050 emissions in the range of e_warm (current policy)
		#   `lo`: values for 2050 in all IAMC scenarios with 2050 emissions in the range of e_cool (decarbonisation)
		list_to_csv(x$summary, 'Fig 4/iamc_data/' %&% n %&% '.csv')

		# note this also produces x$full, which gives the entire slice of the IAMC database for this variable
		# but this is only provided for info, it's not used in the remaining analysis...
		
		# save the scatter & boxplot summary for Figure S6-S10
		good_png('Fig 4/iamc_data/' %&% n %&% '.png', zoom=1.88)
	}
