#########################################################################
##  
##  Calculate the emissions and temperature trajectories under our
##  'current policy' and 'decarbonisation' scenarios
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
## ##  FIGURE S5 (left panel - emissions time series under two scenarios)
#####

	v = 'AR6 climate diagnostics|Infilled|Emissions|Kyoto Gases (AR6-GWP100)'

	# get the data
	v = get_variable(data, v)
	v = kill_inter_years(v)

	# filter these according to emissions in 2050
	warm = v[ v$emish >= e_warm[1] & v$emish <= e_warm[2], ]
	cool = v[ v$emish >= e_cool[1] & v$emish <= e_cool[2], ]

	# reshape
	cols = as.character(seq(2015, 2100, 5))
	emissions_decarb = melt(cool[ , cols], variable.name='year', value.name='value')
	emissions_curpol = melt(warm[ , cols], variable.name='year', value.name='value')

	# summarise as boxlots
	boxplot.pars$boxfill = '#182CC43F'
	bxp_emissions_decarb = boxplot(emissions_decarb$value ~ emissions_decarb$year, pars=boxplot.pars)
	boxplot.pars$boxfill = '#DC001A3F'
	bxp_emissions_curpol = boxplot(emissions_curpol$value ~ emissions_curpol$year, pars=boxplot.pars, add=TRUE, axes=FALSE)



#####
## ##  FIGURE S5 (right panel - temperature time series under two scenarios)
#####

	v = 'AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile'

	# get the data
	v = get_variable(data, v)
	v = kill_inter_years(v)

	# filter these according to emissions in 2050
	warm = v[ v$emish >= e_warm[1] & v$emish <= e_warm[2], ]
	cool = v[ v$emish >= e_cool[1] & v$emish <= e_cool[2], ]

	# reshape
	cols = as.character(seq(2015, 2100, 5))
	temperature_decarb = melt(cool[ , cols], variable.name='year', value.name='value')
	temperature_curpol = melt(warm[ , cols], variable.name='year', value.name='value')

	# summarise as boxlots
	boxplot.pars$boxfill = '#182CC43F'
	bxp_temperature_decarb = boxplot(temperature_decarb$value ~ temperature_decarb$year, pars=boxplot.pars, ylim=c(0,3))
	boxplot.pars$boxfill = '#DC001A3F'
	bxp_temperature_curpol = boxplot(temperature_curpol$value ~ temperature_curpol$year, pars=boxplot.pars, add=TRUE, axes=FALSE)
