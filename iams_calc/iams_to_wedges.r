
#####
## ##  INITIAL SETUP
#####

	# set working directory (e.g. the base path where readme.md exists)
	setwd('C:/stabilisation-wedges-2025/')

	# load the ar6 database and helper functions
	ar6_path = './iams_calc/AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1_vetted.rds'

	source('./others/blam_library.r')
	source('./others/ar6_library.r')


	# load the pairings for AR6 scenarios
	scenario_pairs = read.csv('./iams_calc/AR6_Paired_Scenarios.csv')

	# create shorthands for the emissions in each pair and their difference
	bas = scenario_pairs$Baseline.2050.GHG.Emissions
	mit = scenario_pairs$Mitigation.2050.GHG.Emissions
	dif = scenario_pairs$Difference.in.2050.GHG.Emissions

	# decide which scenarios to keep to better match our world view
	# i.e. the emissions pathways in baseline and decarbonisation pairs
	keep = (mit <= 20 & bas <= 75)
	flush('> Selecting', sum(keep), 'scenario pairs with 2050 emissions of\n')
	flush('  ', round(mean(mit[keep]),2), 'vs', round(mean(bas[keep]),2), 'Gt (', round(mean(dif[keep]),2), 'difference)\n\n')
	# another option is `keep = (mit <= 15 & bas <= 65)`
	# that gets even closer to our CP/DC pathways, but only 189 population

	# filter
	scenario_pairs = scenario_pairs[keep, ]

	# add the region onto scenario names (for compatability with the ar6 database)
	scen_mitigation = paste0(scenario_pairs$Mitigation, '|World')
	scen_baseline = paste0(scenario_pairs$Baseline, '|World')





#####
## ##  PRODUCE FIGURE S5 OF EMISSIONS IN EACH PAIR
#####

	# change 0 to 1 to run this...
	if (0)
	{
		# taken from the flexoki colour scheme <3
		colours = c('#FCC192', '#F89A8A', '#E47DA8', '#8B7EC8', '#3171B2')

		# establish
		good_graphics(mar=c(4,4,1.5,1.5))
		plot_p(1, col=NA, xlim=c(30,112), ylim=c(-5,85), yline=2.2, xlab='Baseline scenario emissions (GtCO₂e in 2050)', ylab='Mitigation scenario emissions (GtCO₂e in 2050)')

		# iso lines of emissions difference
		abline(  0, 1, col=colours[1], lwd=2)
		abline(-20, 1, col=colours[2], lwd=2)
		abline(-40, 1, col=colours[3], lwd=2)
		abline(-60, 1, col=colours[4], lwd=2)
		abline(-80, 1, col=colours[5], lwd=2)

		# scenario pairs
		points(bas[!keep], mit[!keep], cex=0.7, col='grey70')
		points(bas[keep], mit[keep], pch=21, col='black', bg='white')

		# legend
		add_legend('topleft', inset=c(0.03, 0.01), legend=c('0 Gt', '20 Gt', '40 Gt', '60 Gt', '80 Gt'), col=colours, lwd=2, horiz=FALSE)

		# the actual CP and DC pathways for reference
		points(49.784, 9.784, pch=4, col='black', cex=2, lwd=6)
		points(49.784, 9.784, pch=4, col='white', cex=2, lwd=5)
		points(49.784, 9.784, pch=4, col=office$r, cex=2, lwd=2.5)

		box()
		good_png('Figure S5.png', height=7*1200/2.54, width=8.8*1200/2.54, res=1200/1.75)
	}



#####
## ##  PRODUCE FIGURE S6 OF EMISSIONS AND TEMPERAURE RISE IN THE GROUPS
#####

	# change 0 to 1 to run this...
	# beware it's sloooooooooow...
	if (0)
	{
		dev.new(width=14)
		good_graphics()
		layout(t(1:2))
		par(mar=c(2.5, 3.25, 1.75, 1.5))

		# extract global emissions and temperature change
		var = 'AR6 climate diagnostics|Infilled|Emissions|Kyoto Gases (AR6-GWP100)'
		emish = get_variable(data, var)
		emish[ , data.cols] = emish[ , data.cols] / 1000
		var = 'AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile'
		tempz = get_variable(data, var)

		# plot emissions
		x = emish$id %notin% c(scen_mitigation, scen_baseline)
		ar6_lineplot(emish[x, ], yline=2.3, xlim=c(2000, 2100), ylim=c(-20, 120), xaxt='n', col='#66666611', ylab='Global GHG emissions (GtCO₂e)')
		axis(1, at=seq(2000, 2100, 10), labels=FALSE)
		axis(1, at=c(2000, 2020, 2050, 2080, 2100))


		# highlight iam runs in our baseline & mitigation pairs
		for (loop in 1:4)
		{
			x = emish$id %in% scen_mitigation
			ar6_lineplot(emish[x, ], col='#22B0AA16', add=TRUE)
			x = emish$id %in% scen_baseline
			ar6_lineplot(emish[x, ], col='#FEC50622', add=TRUE)
		}

		# summarise these trajectories with the median and IQR
		my.years = 2000:2100
		my.cols = seq(2000, 2100, 5)

		# summarise the mitigation emissions trajectories
		x = emish$id %in% scen_mitigation
		# # P25
		# summary = apply(emish[x, as.character(my.cols)], 2, quantile, probs=0.25, na.rm=TRUE)
		# points(my.cols, summary, pch=16, cex=0.8, col='white')
		# points(my.cols, summary, pch=16, cex=0.5, col='#22B0AA')
		# # P75
		# summary = apply(emish[x, as.character(my.cols)], 2, quantile, probs=0.75, na.rm=TRUE)
		# points(my.cols, summary, pch=16, cex=0.8, col='white')
		# points(my.cols, summary, pch=16, cex=0.5, col='#22B0AA')
		# # P50
		summary = apply(emish[x, as.character(my.cols)], 2, quantile, probs=0.50, na.rm=TRUE)
		summary[1:3] = c(41.343422, 45.839122, 50.274144) # bind on historical for context
		lines(my.cols, summary, col='black', lwd=4.50)
		lines(my.cols, summary, col='#22B0AA', lwd=2.25)

		# summarise the baseline emissions trajectories
		x = emish$id %in% scen_baseline
		# # P25
		# summary = apply(emish[x, as.character(my.cols)], 2, quantile, probs=0.25, na.rm=TRUE)
		# points(my.cols, summary, pch=16, cex=0.8, col='black')
		# points(my.cols, summary, pch=16, cex=0.5, col='#FEC506')
		# # P75
		# summary = apply(emish[x, as.character(my.cols)], 2, quantile, probs=0.75, na.rm=TRUE)
		# points(my.cols, summary, pch=16, cex=0.8, col='black')
		# points(my.cols, summary, pch=16, cex=0.5, col='#FEC506')
		# # P50
		summary = apply(emish[x, as.character(my.cols)], 2, quantile, probs=0.50, na.rm=TRUE)
		summary[1:3] = c(41.343422, 45.839122, 50.274144) # bind on historical for context
		lines(my.cols, summary, col='black', lwd=4.50)
		lines(my.cols, summary, col='#FEC506', lwd=2.25)

		# polish
		ylab_top('(a)', font=2, cex=1.25, at=0.25)
		axis(4, labels=FALSE)
		box()




		# plot temperature rise
		x = tempz$id %notin% c(scen_mitigation, scen_baseline)
		ar6_lineplot(tempz[x, ], yline=2.1, xlim=c(2000, 2100), ylim=c(0.5, 4), col='#66666611', ylab='Global median surface temperature rise (°C)')

		# highlight iam runs in our baseline & mitigation pairs
		for (loop in 1:4)
		{
			x = tempz$id %in% scen_mitigation
			ar6_lineplot(tempz[x, ], col='#22B0AA16', add=TRUE)
			x = tempz$id %in% scen_baseline
			ar6_lineplot(tempz[x, ], col='#FEC50622', add=TRUE)
		}

		# summarise these trajectories with the median and IQR
		my.years = 2000:2100
		my.cols = seq(2000, 2100, 5)

		# summarise the mitigation emissions trajectories
		x = tempz$id %in% scen_mitigation
		# # P25
		# summary = apply(tempz[x, as.character(my.cols)], 2, quantile, probs=0.25, na.rm=TRUE)
		# points(my.cols, summary, pch=16, cex=0.8, col='white')
		# points(my.cols, summary, pch=16, cex=0.5, col='#22B0AA')
		# # P75
		# summary = apply(tempz[x, as.character(my.cols)], 2, quantile, probs=0.75, na.rm=TRUE)
		# points(my.cols, summary, pch=16, cex=0.8, col='white')
		# points(my.cols, summary, pch=16, cex=0.5, col='#22B0AA')
		# P50
		summary = apply(tempz[x, as.character(my.cols)], 2, quantile, probs=0.50, na.rm=TRUE)
		lines(my.cols, summary, col='black', lwd=4.75)
		lines(my.cols, summary, col='#22B0AA', lwd=2.25)

		# summarise the baseline emissions trajectories
		x = tempz$id %in% scen_baseline
		# # P25
		# summary = apply(tempz[x, as.character(my.cols)], 2, quantile, probs=0.25, na.rm=TRUE)
		# points(my.cols, summary, pch=16, cex=0.8, col='black')
		# points(my.cols, summary, pch=16, cex=0.5, col='#FEC506')
		# # P75
		# summary = apply(tempz[x, as.character(my.cols)], 2, quantile, probs=0.75, na.rm=TRUE)
		# points(my.cols, summary, pch=16, cex=0.8, col='black')
		# points(my.cols, summary, pch=16, cex=0.5, col='#FEC506')
		# P50
		summary = apply(tempz[x, as.character(my.cols)], 2, quantile, probs=0.50, na.rm=TRUE)
		lines(my.cols, summary, col='black', lwd=4.75)
		lines(my.cols, summary, col='#FEC506', lwd=2.25)

		# polish
		ylab_top('(b)', font=2, cex=1.25, at=0.25)
		axis(4, labels=FALSE)
		box()

		add_legend('topleft', inset=c(0.08,0.04), horiz=FALSE, y.intersp=1.75,
				col=c('#AAAAAA', '#FEC506', '#22B0AA'), lwd=2, 
				legend=c('All scenarios', 'Baseline scenarios', 'Mitigation scenarios')
		)

		good_png('figure s6.png', height=7/2.54*600, width=16/2.54*600, res=600/2.2)
	}





#####
## ##  	QUICK DEMONSTRATION OF HOW THINGS WORK WITH ONE WEDGE (cus this code is a twisted labrynth)
#####

	# change 0 to 1 to run this...
	if (0)
	{
		### some basic code to play with iam results

		# extract all data to do with one variable and plot a time-series of all model runs
		var = 'Secondary Energy|Electricity|Wind'
		wind = get_variable(data, var)
		ar6_lineplot(wind)

		# highlight iam runs that are close to 10 and 50 GtCO2 emissions in 2050
		x = wind$emissions > 7.5 & wind$emissions < 12.5
		ar6_lineplot(wind[x, ], col='#22B0AA66', add=TRUE)
		x = wind$emissions > 47.5 & wind$emissions < 52.5
		ar6_lineplot(wind[x, ], col='#FEC50666', add=TRUE)

		# or instead, highlight iam runs in our baseline & mitigation pairs
		ar6_lineplot(wind)
		x = wind$id %in% scen_mitigation
		ar6_lineplot(wind[x, ], col='#22B0AA55', add=TRUE)
		x = wind$id %in% scen_baseline
		ar6_lineplot(wind[x, ], col='#FEC50655', add=TRUE)



		### a quick walkthrough of what the main code does

		# extract the value of this variable in 2020 and 2050 plus some metadata, for our pairs of baseline and mitigation scenarios
		var = 'Secondary Energy|Electricity|Wind'
		v2 = extract_pairs(data, var, how='scenario', baseline=scen_baseline, mitigation=scen_mitigation)

		# show how these correspond to the 2050 values in the line plot
		n = nrow(v2$historical)
		points(rep(2020, n), v2$historical$value, pch=21, col='black', bg='grey50')
		n = nrow(v2$mitigation)
		points(rep(2050, n), v2$mitigation$value, pch=21, col='black', bg='#22B0AA')
		points(rep(2050, n), v2$baseline$value, pch=21, col='black', bg='#FEC506')

		# print a little summary (in native IAM units)
		flush(sprintf("  Mean of %.2f vs %.2f EJ in 2050 DC vs CP scenarios (cf %.2f EJ in 2020)\n",
					mean(v2$mitigation$value), mean(v2$baseline$value), mean(v2$historical$value)))

		# state how to convert IAM units into wedges (EJ/TWh) × (TWh/wedge)
		scale = (3600 / 1000000) * 2836.25

		# print a little summary (in wedge units)
		flush(sprintf("  Mean of %.2f vs %.2f wedges in 2050 DC vs CP scenarios\n",
					mean(v2$mitigation$value)/scale, mean(v2$baseline$value)/scale))

		# and that's the bare bones of what happens in our actual plot/summarise function
		d2 = plot_difference(data, var, v2, 'scenario', scale=1/scale)
		print_pair_summary(d2, 'wedges')
		print_model_summary(v2, d2, 'wedges')

		# hope that's clear! :-)
	}



#####
## ##  CALCULATE SOME DERIVED IAM VARIABLES
#####

	# we want to calculate total GHG emissions from different sectors, 
	# we need to calculate some aggregate variables, summing up the 
	# different gases within each sector, so that mean and uncertainty 
	# statistics are calculated from the same set of model runs for all gases
	# (i.e. the same population is contributing to all individual components)

	###
	## 1) aggregate for industry
	###

	# get the five variables (energy demand, fuel supply, process emissions, CH4 from fuel production, HFCs)
	i1 = get_variable(data, 'Emissions|CO2|Energy|Demand|Industry')
	i2 = get_variable(data, 'Emissions|CO2|Industrial Processes')
	i3 = get_variable(data, 'Emissions|CH4|Energy')
	i4 = get_variable(data, 'Emissions|F-Gases')

	# harmonise and align them
	x = intersect(i1$id, i2$id) |> intersect(i3$id) |> intersect(i4$id)
	i1 = i1[ match(x, i1$id), ]
	i2 = i2[ match(x, i2$id), ]
	i3 = i3[ match(x, i3$id), ]
	i4 = i4[ match(x, i4$id), ]

	# calculate (energy CO2 + process CO2 + energy CH4 (in CO2e) + HFCs (in CO2e))
	# note 'Emissions|CH4|Energy' is in units of MtCH4, hence we multiply by its GWP)
	#  and 'Emissions|F-Gases' is in MtCO2eq - so it doesn't need a scale factor)
	ix = i1
	ix$Variable = 'Emissions|Industry|All'
	ix$Unit = 'Mt CO2e/yr'
	for (dc in data.cols)
		ix[ , dc] = i1[ , dc] + i2[ , dc] + i3[ , dc]*30 + i4[ , dc]

	# add onto our main database
	data = rbind(data, ix)



	###
	## 2) aggregate for transport
	###

	# get the two variables (CO2 from transport, N2O from transport)
	t1 = get_variable(data, 'Emissions|CO2|Energy|Demand|Transportation')
	t2 = get_variable(data, 'Emissions|N2O|Energy')

	# harmonise and align them
	x = intersect(t1$id, t2$id)
	t1 = t1[ match(x, t1$id), ]
	t2 = t2[ match(x, t2$id), ]

	# calculate (CO2 + N2O (in CO2e))
	# note 'Emissions|N2O|Energy' is in units of MtCH4, hence we multiply by its GWP)
	tx = t1
	tx$Variable = 'Emissions|Transport|All'
	tx$Unit = 'Mt CO2e/yr'
	for (dc in data.cols)
		tx[ , dc] = t1[ , dc] + t2[ , dc]*273/1000


	# add onto our main database
	data = rbind(data, tx)



	###
	## 3) aggregate for land use
	###

	# get the three variables (CO2, CH4, N2O)
	l1 = get_variable(data, 'Emissions|CO2|AFOLU')
	l2 = get_variable(data, 'Emissions|CH4|AFOLU')
	l3 = get_variable(data, 'Emissions|N2O|AFOLU')

	# harmonise and align them
	x = intersect(l1$id, l2$id) |> intersect(l3$id)
	l1 = l1[ match(x, l1$id), ]
	l2 = l2[ match(x, l2$id), ]
	l3 = l3[ match(x, l3$id), ]

	lx = l1
	lx$Variable = 'Emissions|Land|All'
	lx$Unit = 'Mt CO2e/yr'

	for (dc in data.cols)
		lx[ , dc] = l1[ , dc] + l2[ , dc]*27 + l3[ , dc]*273/1000

	data = rbind(data, lx)




	# we also do this to look at the amount of zero carbon electricity across 
	# different sources (wind + solar + nuclear), but don't end up reporting
	# anything on this because there's nothing terribly interesting, so 
	# consider thisas just a demo on how it can be used...

	###
	## 4) zero-carbon electricity (wind + solar)
	###

	# get the two variables (electricity from: wind, solar)
	pwr_w = get_variable(data, 'Secondary Energy|Electricity|Wind')
	pwr_s = get_variable(data, 'Secondary Energy|Electricity|Solar')

	# harmonise and align them
	x = intersect(pwr_w$id, pwr_s$id)
	pwr_w = pwr_w[ match(x, pwr_w$id), ]
	pwr_s = pwr_s[ match(x, pwr_s$id), ]

	# calculate (wind + solar)
	pwr_ws = pwr_w
	pwr_ws$Variable = 'Secondary Energy|Electricity|WindAndSolar'
	for (dc in data.cols)
		pwr_ws[ , dc] = pwr_w[ , dc] + pwr_s[ , dc]

	# add onto our main database
	data = rbind(data, pwr_ws)



	###
	## 5) zero-carbon electricity (wind + solar + nuclear)
	###

	# get the two variables (electricity from: windAndSolar, nuclear)
	pwr_n = get_variable(data, 'Secondary Energy|Electricity|Nuclear')

	# harmonise and align them
	x = intersect(pwr_ws$id, pwr_n$id)
	pwr_ws = pwr_ws[ match(x, pwr_ws$id), ]
	pwr_n = pwr_n[ match(x, pwr_n$id), ]

	# calculate (wind + solar + nuclear)
	pwr_wsn = pwr_ws
	pwr_wsn$Variable = 'Secondary Energy|Electricity|WindAndSolarAndNuclear'
	for (dc in data.cols)
		pwr_wsn[ , dc] = pwr_ws[ , dc] + pwr_n[ , dc]
	
	# add onto our main database
	data = rbind(data, pwr_wsn)



	# and finally, we are interested in total clean hydrogen production
	# from both green and blue.  first, harmonise scenarios with green 
	# and blue hydrogen in, and from them calculate total hydrogen

	###
	## 6) clean hydrogen (from electricity + from fossil ccs)
	###

	# get the two variables (hydrogen from electricity, fossil w/ ccs)
	h2_grn = get_variable(data, 'Secondary Energy|Hydrogen|Electricity')
	h2_blu = get_variable(data, 'Secondary Energy|Hydrogen|Fossil|w/ CCS')

	# unlike earlier, ignore any results which don't have both
	# as we are interested in both the constituent components and their total
	kill = h2_grn$id[ h2_grn$id %notin% h2_blu$id ]
	kill = data$id %in% kill & data$Variable == 'Secondary Energy|Hydrogen|Electricity'
	data = data[!kill, ]

	# get the two variables (hydrogen from electricity, fossil w/ ccs)
	h2_grn = get_variable(data, 'Secondary Energy|Hydrogen|Electricity')
	h2_blu = get_variable(data, 'Secondary Energy|Hydrogen|Fossil|w/ CCS')

	# harmonise and align them
	x = intersect(h2_grn$id, h2_blu$id)
	h2_grn = h2_grn[ match(x, h2_grn$id), ]
	h2_blu = h2_blu[ match(x, h2_blu$id), ]

	# calculate (green + blue)
	h2_tot = h2_grn
	h2_tot$Variable = 'Secondary Energy|Hydrogen|Clean'
	h2_tot$Unit = 'Mt CO2e/yr'
	
	# an EJ of electricity isn't the same as an EJ of fossil gas
	# so convert both into MtCO2e abated, so they are equivalent
	scale_green = c(				# EJ per wedge
		(3600 / 1000000) * 4739,	# IEA6
		(3600 / 1000000) * 4845,	# IEA4
		(3600 / 1000000) * 4701,	# RCP8
		(3600 / 1000000) * 4760		# RCP6
	)
	scale_blue = c(					# EJ per wedge
		(3600 / 1000000) * 5989,	# IEA6
		(3600 / 1000000) * 6161,	# IEA4
		(3600 / 1000000) * 5928,	# RCP8
		(3600 / 1000000) * 6023		# RCP6
	)
	scale_green = 2000 / mean(scale_green)	# Mt / EJ
	scale_blue = 2000 / mean(scale_blue)	# Mt / EJ

	for (dc in data.cols)
		h2_tot[ , dc] = h2_grn[ , dc]*scale_green + h2_blu[ , dc]*scale_blue
	
	# add onto our main database
	data = rbind(data, h2_tot)







#####
## ##  FIG 4A) EMISSIONS SAVINGS PER SECTOR
#####

	results = NULL
	dev.new(width=14)

	# set TRUE to make plotting much faster, set to FALSE to use transparency in panel (a) and be prettier
	quick = FALSE

	# our list of IAM variables to look at
	sectors = list(
		'electricity' = 'Emissions|CO2|Energy|Supply',
		'industry'    = 'Emissions|Industry|All',		# derived earlier 
		'transport'   = 'Emissions|Transport|All',		# derived earlier 
		'land'        = 'Emissions|Land|All',			# derived earlier 
		'buildings'   = 'Emissions|CO2|Energy|Demand|Residential and Commercial'
	)

	# the scale factor is always -2000 per wedge, as all these are all in MtCO2, and you reduce rather than increase
	scale = -2000

	var = sectors$electricity
	o = plot_print_save_pairs(data, var, 1/scale, yline=3.1, quick=quick)
	results = push_row(results, o)
	good_png('sector_1_electricity.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)

	var = sectors$industry
	o = plot_print_save_pairs(data, var, 1/scale, yline=3.1, quick=quick)
	results = push_row(results, o)
	good_png('sector_2_industry.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)

	var = sectors$transport
	o = plot_print_save_pairs(data, var, 1/scale, yline=3.1, quick=quick)
	results = push_row(results, o)
	good_png('sector_3_transport.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)

	var = sectors$land
	o = plot_print_save_pairs(data, var, 1/scale, yline=3.1, quick=quick)
	results = push_row(results, o)
	good_png('sector_4_land.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)

	var = sectors$buildings
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.9, quick=quick)
	results = push_row(results, o)
	good_png('sector_5_buildings.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)




	# as an aside, also calculate the individual components
	# to check what they look like + report n
	if (0)
	{
		var = 'Emissions|CO2|Energy|Demand|Industry'
		o = plot_print_save_pairs(data, var, 1/scale, yline=3.1)

		var = 'Emissions|CO2|Industrial Processes'
		o = plot_print_save_pairs(data, var, 1/scale, yline=3.1)

		var = 'Emissions|CH4|Energy'
		o = plot_print_save_pairs(data, var, 30/scale, yline=3.1)

		var = 'Emissions|F-Gases'
		o = plot_print_save_pairs(data, var, 1/scale, yline=3.1)


		var = 'Emissions|CO2|Energy|Demand|Transportation'
		o = plot_print_save_pairs(data, var, 1/scale, yline=3.1)

		var = 'Emissions|N2O|Energy'
		o = plot_print_save_pairs(data, var, 273/1000/scale, yline=3.1)


		var = 'Emissions|CO2|AFOLU'
		o = plot_print_save_pairs(data, var, 1/scale, yline=3.1)

		var = 'Emissions|CH4|AFOLU'
		o = plot_print_save_pairs(data, var, 27/scale, yline=3.1)

		var = 'Emissions|N2O|AFOLU'
		o = plot_print_save_pairs(data, var, 273/1000/scale, yline=3.1)
	}







#####
## ##  FIG 4B) EMISSIONS SAVINGS FROM ELECTRICITY STRATEGIES
#####

	# define the IAMC variable names that correspond to specific strategies
	wedges = list(

		pwr_wind     = 'Secondary Energy|Electricity|Wind',
		pwr_solar    = 'Secondary Energy|Electricity|Solar',
		pwr_nuclear  = 'Secondary Energy|Electricity|Nuclear',
		pwr_beccs    = 'Secondary Energy|Electricity|Biomass|w/ CCS',
		pwr_gas_ccs  = 'Secondary Energy|Electricity|Gas|w/ CCS',
		pwr_coal_ccs = 'Secondary Energy|Electricity|Coal|w/ CCS'

	)


	# for many of these, we have four baselines...
	# we could average their scale factors, but this might corrupt
	# the summary statistics (given that we are calculating percentiles)
	# so instead, calculate them four times and compute the summary from those
	# four independent versions.  this is annoyingly slow because of all the plotting
	# but then you plot a fifth time using the mean of the scale to make the actual
	# picture for the paper supplement :-X

	var = wedges$pwr_wind
	# (EJ/TWh) × (TWh/wedge)
	scale = c(
		(3600 / 1000000) * 2832,	# IEA6
		(3600 / 1000000) * 2918,	# IEA4
		(3600 / 1000000) * 2539,	# RCP8
		(3600 / 1000000) * 3056		# RCP6
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.3, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=2.3, quick=quick)
	good_png('wedge_1_wind.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$pwr_solar
	# (EJ/TWh) × (TWh/wedge) 
	scale = c(
		(3600 / 1000000) * 2832,	# IEA6
		(3600 / 1000000) * 2918,	# IEA4
		(3600 / 1000000) * 2539,	# RCP8
		(3600 / 1000000) * 3056		# RCP6
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.3, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=2.3, quick=quick)
	good_png('wedge_2_solar.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$pwr_nuclear
	# (EJ/TWh) × (TWh/wedge) 
	scale = c(
		(3600 / 1000000) * 2832,	# IEA6
		(3600 / 1000000) * 2918,	# IEA4
		(3600 / 1000000) * 2539,	# RCP8
		(3600 / 1000000) * 3056		# RCP6
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=1.9, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=1.9, quick=quick)
	good_png('wedge_3_nuclear.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	#⚠# note, BECCS is only for crops
	var = wedges$pwr_beccs
	# (EJ/TWh) × (TWh/wedge) 
	scale = c(
		(3600 / 1000000) * 1957,	# IEA6
		(3600 / 1000000) * 1997,	# IEA4
		(3600 / 1000000) * 1812,	# RCP8
		(3600 / 1000000) * 2061		# RCP6
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=1.9, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=1.9, quick=quick)
	good_png('wedge_4_beccs.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$pwr_gas_ccs
	# (EJ/TWh) × (TWh/wedge) 
	scale = c(
		(3600 / 1000000) * 7054,	# IEA6
		(3600 / 1000000) * 7054,	# IEA4
		(3600 / 1000000) * 7158,	# RCP8
		(3600 / 1000000) * 7054		# RCP6
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.1, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=2.1, quick=quick)
	good_png('wedge_5_gas_ccs.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$pwr_coal_ccs
	# (EJ/TWh) × (TWh/wedge) 
	scale = c(
		(3600 / 1000000) * 2859,	# IEA6
		(3600 / 1000000) * 2859,	# IEA4
		(3600 / 1000000) * 2893,	# RCP8
		(3600 / 1000000) * 2859		# RCP6
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.1, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=2.1, quick=quick)
	good_png('wedge_6_coal_ccs.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	# as an aside, look at groups of zero-carbon power sources
	# but it's dull, so don't bother
	if (0)
	{
		# the sector-level wedges
		results = NULL

		# (EJ/TWh) × (TWh/wedge) 
		scale = c(
			(3600 / 1000000) * 2832,	# IEA6
			(3600 / 1000000) * 2918,	# IEA4
			(3600 / 1000000) * 2539,	# RCP8
			(3600 / 1000000) * 3056		# RCP6
		)

		var = wedges$pwr_wind
		o = plot_print_save_pairs(data, var, mean(1/scale), yline=2.3)
		results = push_row(results, o)

		var = wedges$pwr_solar
		o = plot_print_save_pairs(data, var, mean(1/scale), yline=2.3)
		results = push_row(results, o)

		var = wedges$pwr_nuclear
		o = plot_print_save_pairs(data, var, mean(1/scale), yline=2.3)
		results = push_row(results, o)

		var = 'Secondary Energy|Electricity|WindAndSolar'
		o = plot_print_save_pairs(data, var, mean(1/scale), yline=2.3)
		results = push_row(results, o)

		var = 'Secondary Energy|Electricity|WindAndSolarAndNuclear'
		o = plot_print_save_pairs(data, var, mean(1/scale), yline=2.3)
		results = push_row(results, o)
	}




#####
## ##  FIG 4C) EMISSIONS SAVINGS FROM INDUSTRY STRATEGIES
#####

	# define the IAMC variable names that correspond to specific strategies
	wedges = list(

		# look at hydrogen separately to find how much electricity is needed
		ind_h2_green   = 'Secondary Energy|Hydrogen|Electricity',
		ind_h2_blue    = 'Secondary Energy|Hydrogen|Fossil|w/ CCS',
		ind_h2_total   = 'Secondary Energy|Hydrogen|Clean',
		ind_ch4_energy = 'Emissions|CH4|Energy|Supply',
		# ignore ccs process emissions as there are only six scenarios (0 in our pairings)
		ind_p_ccs      = 'Carbon Sequestration|CCS|Fossil|Industrial Processes',
		ind_e_ccs      = 'Carbon Sequestration|CCS|Fossil|Energy|Demand|Industry',
		ind_hfc        = 'Emissions|HFC',
		ind_daccs      = 'Carbon Sequestration|Direct Air Capture'

	)


	var = wedges$ind_h2_green
	# (EJ/TWh) × (TWh/wedge) 
	scale = c(
		(3600 / 1000000) * 4739,	# IEA6
		(3600 / 1000000) * 4845,	# IEA4
		(3600 / 1000000) * 4701,	# RCP8
		(3600 / 1000000) * 4760		# RCP6
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=1.9, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=1.9, quick=quick)
	good_png('wedge_7a_green_h2.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$ind_h2_blue
	# (EJ/TWh) × (TWh/wedge) 
	scale = c(
		(3600 / 1000000) * 5989,	# IEA6
		(3600 / 1000000) * 6161,	# IEA4
		(3600 / 1000000) * 5928,	# RCP8
		(3600 / 1000000) * 6023		# RCP6
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=1.9, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=1.9, quick=quick)
	good_png('wedge_7b_blue_h2.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)


	var = wedges$ind_h2_total
	# (MtCO₂/wedge) 
	scale = 2000
	o = plot_print_save_pairs(data, var, 1/scale, yline=3.1, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=3.1, quick=quick)
	good_png('wedge_7_total_h2.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)


	var = wedges$ind_ch4_energy
	#  (MtCO₂/wedge) ÷ (GWP of CH₄)
	scale = -2000 / 30
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.5, quick=quick)
	results = push_row(results, o)
	good_png('wedge_8_industrial_ch4.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$ind_e_ccs
	# (MtCO₂/wedge)
	scale = 2000
	o = plot_print_save_pairs(data, var, 1/scale, yline=3.1, quick=quick)
	results = push_row(results, o)
	good_png('wedge_9_industrial_ccs.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$ind_hfc
	#  (MtCO₂/wedge) × (kt/Mt) ÷ (GWP of HFC134a)
	scale = -2000 * 1000 / 1300
	o = plot_print_save_pairs(data, var, 1/scale, yline=3.1, quick=quick)
	results = push_row(results, o)
	good_png('wedge_10_industrial_hfc.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$ind_daccs
	# (MtCO₂/wedge)
	scale = 2000
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.9, quick=quick)
	results = push_row(results, o)
	good_png('wedge_11_daccs.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)





#####
## ##  TRANSPORT
#####

	# define the IAMC variable names that correspond to specific strategies
	wedges = list(

		trn_evs = 'Final Energy|Transportation|Electricity',
		trn_biofuel = 'Final Energy|Transportation|Liquids|Bioenergy',
		trn_freight = 'Emissions|CO2|Energy|Demand|Transportation|Freight'

	)



	var = wedges$trn_evs
	# (EJ/TWh) × (TWh/trillion v-km) × (v-km/p-km) × (p-km/wedge)
	#             travel efficiency  ×  occupancy
	scale = c(
		IEA6 = (3600 / 1000000) * 170 * 1/1.55102 * 19.22808,
		IEA4 = (3600 / 1000000) * 170 * 1/1.55116 * 20.12813,
		RCP8 = (3600 / 1000000) * 170 * 1/1.49985 * 17.71464,
		RCP6 = (3600 / 1000000) * 170 * 1/1.54928 * 18.97085
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=1.9, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=1.9, quick=quick)
	good_png('wedge_12_electric_vehicles.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$trn_freight
	# (MtCO₂/wedge)
	scale = -2000
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.9, quick=quick)
	results = push_row(results, o)
	good_png('wedge_13_freight_transport.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$trn_biofuel
	# (EJ/TWh) × (TWh/trillion v-km) × (v-km/p-km) × (p-km/wedge)
	#             travel efficiency  ×  occupancy
	scale = c(
		IEA6 = (3600 / 1000000) * 499 * 1/1.55102 * 20.21790,
		IEA4 = (3600 / 1000000) * 477 * 1/1.55116 * 21.15226,
		RCP8 = (3600 / 1000000) * 523 * 1/1.49985 * 18.65372,
		RCP6 = (3600 / 1000000) * 505 * 1/1.54928 * 19.95532
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=1.9, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=1.9, quick=quick)
	good_png('wedge_14_biofuel_vehicles.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	# a quick sense check: look at evs another way...
	# 8.54 EJ delivered by electricity displaces 3x so ~26 EJ of oil
	# Oil emits ~70 g/MJ = 70 kg/GJ = 70 t/TJ = 70 kt/PJ = 70 Mt/EJ = 0.07 Gt/EJ
	# 26 * 0.07 = 1.8 Gt = 0.9 wedges





#####
## ##  LAND USE
#####

	# define the IAMC variable names that correspond to specific strategies
	wedges = list(

		lnd_afforest = 'Carbon Sequestration|Land Use|Afforestation',
		lnd_ew = 'Carbon Sequestration|Enhanced Weathering',
		lnd_meat = 'Food Demand|Livestock'

	)
	

	var = wedges$lnd_afforest
	# (MtCO₂/wedge)
	scale = 2000
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.9, quick=quick)
	results = push_row(results, o)
	good_png('wedge_15_afforestation.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$lnd_meat
	# (days/year) × (population) × (gCO2e/kcal) × (MtCO2e/gCO2e) × (MtCO2e/wedge)
	#  kcal/cap/day → kcal/year
	scale = c(
		IEA6 = 1 / (365 * 9550945000 * -6.2 / 1e12) * 2000,
		IEA4 = 1 / (365 * 9550945000 * -6.2 / 1e12) * 2000,
		RCP8 = 1 / (365 * 9309097200 * -6.2 / 1e12) * 2000,
		RCP6 = 1 / (365 * 10077944600 * -6.2 / 1e12) * 2000		
	)
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.5, quick=TRUE)
	results = push_row(results, o)
	o = plot_print_save_pairs(data, var, mean(1/scale), yline=2.5, quick=quick)
	good_png('wedge_16_diet_change.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)



	var = wedges$lnd_ew
	# (MtCO₂/wedge)
	scale = 2000
	o = plot_print_save_pairs(data, var, 1/scale, yline=2.9, quick=quick)
	results = push_row(results, o)
	good_png('wedge_17_enhanced_weathering.png', height=6/2.54*1200, width=16/2.54*1200, res=1200/2.2)




#####
## ##  AND... WE ARE DONE
#####

	write.csv(results, 'wedges_iam_results.csv', row.names=FALSE)
	