#########################################################################
##  
##  This code produces a minimally pre-processed version of the IAMC 
##  AR6 database and provides functions for extracting results from it
##
##  Instructions:
##    first, download the AR6 Scenarios Database from IIASA from
##    https://data.ece.iiasa.ac.at/ar6/#/downloads
##    then run the `PREREQUISITE` code block starting line 25 to
##    generate a processed version of this
##
##    change the path on line 56 to be the location where you saved
##    the resulting processed database
## 


	source('blam_library.r')

#####
## ##  PREREQUISITE -- DOWNLOAD AND PRE-PROCESS THE AR6 SCENARIO DATABASE
#####

# run once
if (0)
{
	# download AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1 from https://data.ece.iiasa.ac.at/ar6/#/downloads
	# access via https://iiasa.ac.at/models-tools-data/ar6-scenario-explorer-and-database
	data = read.csv('AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.csv')

	colnames(data) = gsub('X', '', colnames(data))

	# tie in a unique id for each row
	data$id = paste(data$Model, data$Scenario, data$Region, sep='|')

	# attribute the 2100 warming to each row
	warming = (data$Variable == 'AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile')
	warming = data.frame(id=data$id[warming], value=data$`2100`[warming])
	m = match(data$id, warming$id)
	data$warming = warming$value[m]

	# attribute the 2050 emissions to each row
	emissions = (data$Variable == 'AR6 climate diagnostics|Infilled|Emissions|Kyoto Gases (AR6-GWP100)')
	emissions = data.frame(id=data$id[emissions], value=data$`2050`[emissions])
	m = match(data$id, emissions$id)
	data$emish = emissions$value[m] / 1000

	# save as rds for speed
	saveRDS(data, 'AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.rds')
}



#####
## ##  READ IN THE DATA
#####

	data = readRDS('AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.rds')

	# which columns contain values vs. metadata
	data.cols = as.character(1995:2100)




#####
## ##  FIX UNIT ERRORS
#####

	# ch4 results presented in kt not mt
	# assumed to be any result with >100,000 Mt CH4 emissions (obviously implausible)
	# all other results are in the range of 100-800 Mt
	w = which(data$Variable == 'Emissions|CH4')
	# hist_log_y(as.vector(t(data[w,data.cols])), col='grey90')
	ch4max = apply(data[w, data.cols], 1, max, na.rm=TRUE)
	fix = w[ which(ch4max > 1e5) ]
	data[fix, data.cols] = data[fix, data.cols] / 1000
	# hist_log_y(as.vector(t(data[w,data.cols])), col='grey90')


	# ch4 results presented in kt not mt
	# assumed to be any result with >10,000 Mt CH4 emissions from ALOFU alone (v strong outlier)
	# all other results are in the range of 0-600 Mt
	w = which(data$Variable == 'Emissions|CH4|AFOLU')
	# hist_log_y(as.vector(t(data[w,data.cols])), col='grey90')
	ch4max = apply(data[w, data.cols], 1, max, na.rm=TRUE)
	fix = w[ which(ch4max > 1e4) ]
	data[fix, data.cols] = data[fix, data.cols] / 1000
	# hist_log_y(as.vector(t(data[w,data.cols])), col='grey90')


	# population presented in thousands not millions
	# assumed to be any result with >1 trillion people (obviously implausible)
	# all other results are in the range of 6-14 billion
	w = which(data$Variable == 'Population')
	# hist_log_y(as.vector(t(data[w,data.cols])), col='grey90')
	popmax = apply(data[w, data.cols], 1, max, na.rm=TRUE)
	fix = w[ which(popmax > 1e6) ]
	data[fix, data.cols] = data[fix, data.cols] / 1000
	# hist_log_y(as.vector(t(data[w,data.cols])), col='grey90')


	# people eating hulk hogan quantities of meat
	# assumed to be any result with >5000 calories per day per capita from livestock
	# all other results are in the range of 200-1000 calories
	w = which(data$Variable == 'Food Demand|Livestock')
	# hist_log_y(as.vector(t(data[w,data.cols])), col='grey90')
	beefmax = apply(data[w, data.cols], 1, max, na.rm=TRUE)
	fix = w[ which(beefmax > 5000) ]
	data[fix, data.cols] = NA
	# hist_log_y(as.vector(t(data[w,data.cols])), col='grey90')






#####
## ##  HELPER FUNCTIONS
#####

	# search for variable names containing 'x'
	find_variable = function(x)
	{
		v = as.data.frame(table(data$Variable), stringsAsFactors=FALSE)
		colnames(v) = c('data', 'freq')

		ok = tolower(v$data) %contains% tolower(x)
		v[ok, ]
	}

	# extract a variable (i.e. rows)
	get_variable = function(data, var, reg='*')
	{
		filter = (data$Variable == var)
		if (reg != '*') filter = (filter & data$Region == reg)

		if (sum(filter) == 0)
			stop(paste('get_variable: No data for', var, reg, '\n'))

		data = data[filter, ]
		data
	}

	# remove years (i.e. columns) that are not divisible by 'n'
	kill_inter_years = function(d, n=5)
	{
		kill = 1995:2100
		kill = kill[ kill %% n != 0 ]
		d[ , colnames(d) %notin% as.character(kill) ]
	}


	# calculate the difference between AR6 model runs (given in 'data')
	# for a given 'variable'
	# comparing mitigation scenarios where emissions in 2050 are within the range specified by 'cool' (a vector length 2)
	# and baseline scenarios where emissions in 2050 are within the range specified by 'warm' (a vector length 2)
	#
	# this will return a list containing
	#     $summary (a list giving the variable values in 2020, and in 2050 under the two scenarios)
	#     $full (a data.frame of all rows in the IAMC database that correspond to the two scenarios)
	#
	# it will draw a scatter and boxplot summarising the differences
	# this will use log axes if 'log' is set to 'x', 'y' or 'xy'
	# 
	calculate_scenario_difference = function(data, variable, cool=e_cool, warm=e_warm, log='')
	{
		# get the data
		v = get_variable(data, variable)
		v = kill_inter_years(v)
		flush('Found', nrow(v), 'scenarios\n')

		full_v = v


		# establish the historical baseline
		y = v$`2020`
		results = list(
			historical = y[ !is.na(y) ]
		)

	
		# get our three emissions categories
		t1 = (v$emish >= warm[1] & v$emish <= warm[2])
		t2 = (v$emish >= cool[1] & v$emish <= cool[2])
	
		results$high = v$`2050`[ t1 ]
		results$lo = v$`2050`[ t2 ]


		# make a scatter/bar plot
		tx = 'Greenhouse gas emissions in 2050 (GtCO\U2082' %&% 'e)'
		#ty = paste0(str_replace('|', ' | ', v$Variable[1]), ' (', v$Unit[1], ')')
		ty = gsub('Sequestration', 'Seq.', v$Variable[1], fixed=TRUE)
		ty = paste0(ty, ' (', v$Unit[1], ')')

		col = rep('grey75', length(t1))
		col[t1] = '#FEC506'
		col[t2] = '#02908A'


		good_graphics(padding=TRUE, mar.bottom=3, mar.left=3.5)
		par(font.lab=1)
		plot_p(v$emish, v$`2050`, ymin=min(0, v$`2050`, na.rm=TRUE), col=alpha(col, 0.75), xlab=tx, ylab='', log=log)
		ylab_top(ty, font=1, at=0.25, adj=0, cex=0.9)

		x = c(-10, 150)
		y = as.numeric(quantile(results$historical, c(0.25, 0.5, 0.75), na.rm=TRUE))
		#plot_area(x, rep(y[1],2), rep(y[3],2), col=alpha('black', 0.25), border='black')
		lines(x, rep(y[2], 2), col='black', lwd=2)

		x = warm
		y = as.numeric(quantile(results$high, c(0.25, 0.5, 0.75), na.rm=TRUE))
		plot_area(x, rep(y[1],2), rep(y[3],2), col=alpha('#FEC506', 0.33), border='black')
		lines(x, rep(y[2], 2), col='black')

		x = cool
		y = as.numeric(quantile(results$lo, c(0.25, 0.5, 0.75), na.rm=TRUE))
		plot_area(x, rep(y[1],2), rep(y[3],2), col=alpha('#02908A', 0.33), border='black')
		lines(x, rep(y[2], 2), col='black')

		axis(3, labels=FALSE)
		axis(4, labels=FALSE)

		return(list(summary=results, full=full_v))
	}

