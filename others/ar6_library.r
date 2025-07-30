#########################################################################
##  
##  This code reads in the processed AR6 database we use and
##  provides functions for extracting results from it
##
##  Instructions:
##    first, run 'first_run_setup.r' to generate the processed data
## 


	library(vioplot)


#####
## ##  READ IN THE DATA
#####

	if (!exists('ar6_path'))
	{
		cat('ar6_library.r: please run "first_run_steup.r" first to\n')
		cat('generate the processed ar6 database file, then set\n')
		cat('"ar6_path" to give the location of the output file\n\n')
		stop('variable `ar6_path` not found\n')
	}

	data = readRDS(ar6_path)

	# note which columns contain values (rather than metadata)
	data.cols = as.character(1995:2100)






#####
## ##  REMOVE SUSPECTED IAMC UNIT ERRORS
#####

	# people eating hulk hogan quantities of meat
	# assumed to be any result with >5000 calories per day per capita from livestock
	# all other results are in the range of 200-1000 calories
	w = which(data$Variable == 'Food Demand|Livestock')
	# hist_log_y(as.vector(t(data[w,data.cols])), col='grey90')
	beefer = data[w, '2050']
	kill = w[ which(beefer > 5000) ]
	data = data[-kill, ]
	# hist_log_y(as.vector(t(data[w,data.cols])), col='grey90')
	# ar6_lineplot(data[w, ], log='y', ylim=c(200,20000))

	# the energy sector absorbing CH4 rather than producing it
	# assumed to be any result with negative CH4 emissions from energy supply
	w = which(data$Variable == 'Emissions|CH4|Energy|Supply')
	sucker = data[w, '2050']
	kill = w[ which(sucker < 0) ]
	data = data[-kill, ]



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


	# version with splines to smooth the transitions
	ar6_lineplot = function(ar6, xlim=NULL, ylim=NULL, xlab='', ylab=NULL, yline=par('mgp')[1], col='#66666622', lwd=1, add=FALSE, ...)
	{
		# get our x values
		years = as.numeric(data.cols)
	
		# create a string giving the variable & units
		if (is.null(ylab))
			ylab = paste0(gsub('Sequestration', 'Seq.', ar6$Variable[1], fixed=TRUE), ' (', ar6$Unit[1], ')')

		# decide chart limits if needed
		if (is.null(xlim))
		{
			xlim = range(years)
		}
		if (is.null(ylim))
		{
			ylim = range(ar6[ , data.cols], na.rm=TRUE)
			ylim[1] = min(0, ylim[1]*1.02)
			ylim[2] = max(0, ylim[2]*1.02)
		}

		# establish plot
		if (add == FALSE)
		{
			plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab='', ...)
			mtext(ylab, side=2, line=yline, cex=par('cex'), las=0)
		}

		# create & plot a line for each scenario
		for (i in 1:nrow(ar6))
		{
			# assemble this scenario's data (year vs. value)
			xy = data.frame(
				x = years,
				y = as.numeric(ar6[i, data.cols])
			)

			# remove NA
			xy = xy[ !is.na(xy$y), ]
			lines(xy, col=col, lwd=lwd)
		}
	}


	# calculate the difference between AR6 model runs (given in 'data') for a given 'variable'
	# decide which model runs fit within your 'baseline' and 'mitigation' in one of three ways:
	#   how = 'scenario' → specify a pairwise list of scenario id(s) to compare, defined by
	#                      'Model|Scenario|Region' - e.g. 'AIM/CGE 2.0|ADVANCE_INDC|World'
	#   how = 'warming' → specify the range of warming outcomes in 2100 that belong in each
	#                     group - e.g. baseline=c(3.0,3.5), mitigation=c(1.5,2.0)
	#   how = 'emissions' → specify the range of GHG emissions in 2050 that belong in each
	#                     group - e.g. baseline=c(36,44), mitigation=c(-4,4)
	#
	# this will return a list containing three data.frames:
	#     $historical
	#        $row       ← the row within 'data' this result relates to
	#        $value     ← the value of 'variable' in 2020 from this model run
	#     $baseline
	#        $row       ← the row within 'data' this result relates to
	#        $value     ← the value of 'variable' in 2050 from this model run
	#        $warming   ← the warming in 2100 corresponding to this model run
	#        $emissions ← the ghg emissions in 2050 corresponding to this model run
	#     $mitigation
	#        $row       ← the row within 'data' this result relates to
	#        $value     ← the value of 'variable' in 2050 from this model run
	#        $warming   ← the warming in 2100 corresponding to this model run
	#        $emissions ← the ghg emissions in 2050 corresponding to this model run
	# 
	extract_pairs = function(data, variable, how, baseline, mitigation, log='')
	{
		# check inputs
		if (how %notin% c('scenario', 'warming', 'emissions'))
			stop('"how" you filter model runs must be either "scenario", "warming", or "emissions"\n')


		# extract this variable
		H = which(data$Variable == variable)
		d = data[H, ]
		d = kill_inter_years(d)

		# establish the historical results
		historical = data.frame(row=H, value=d[ , '2020'])
		historical = historical[ !is.na(historical$value), ]



		# decide which rows to include in the baseline and mitigation groups
		# option 1: string matching scenario names
		if (how == 'scenario')
		{
			# check inputs
			if (length(baseline) != length(mitigation))
				stop('"baseline" and "mitigation" are compared pair-wise, so they must be the same length\n')
			
			# filter out pairs which are missing this variable
			keep = rep(TRUE, length(baseline))
			for (i in 1:length(baseline))
				if (baseline[i] %notin% d$id | mitigation[i] %notin% d$id)
					keep[i] = FALSE

			baseline = baseline[keep]
			mitigation = mitigation[keep]

			# get our two classifications
			B = match(baseline, d$id)
			M = match(mitigation, d$id)
		}


		# decide which rows to include in the baseline and mitigation groups
		# option 2 or 3: pick based on a metadata column (numeric ranges)
		#
		# We used this on the initial submission of the paper, selecting scenarios
		# by 'boxing' them based on emissions in 2050 being close to CP or DC levels.
		# This is ill-advised as you're comparing a MESSAGE baseline to an IMAGE
		# decarbonisation pathway, etc.  The mean results are pretty close to the 
		# new 'scenario pairs' method, but the standard deviations were much larger
		# ... but this is still kept here for legacy
		# 
		# e.g. call with:
		#     ghg_mitigation = 9.784 +    0 + 2*c(-3,3)
		#     ghg_baseline  =  9.784 + 2*20 + 2*c(-3,3)
		#     v1 = extract_pairs(data, var, how='emissions', baseline=ghg_baseline, mitigation=ghg_mitigation)
		#     d1 = plot_difference(data, var, v1, 'emissions', scale=scale, yline=yline)
		#
		if (how != 'scenario')
		{
			if (how == 'warming')
				selector.col = 'warming'
			
			if (how == 'emissions')
				selector.col = 'emissions'
			
			# get our two classifications
			B = which(d[ , selector.col] >= baseline[1] & d[ , selector.col] <= baseline[2])
			M = which(d[ , selector.col] >= mitigation[1] & d[ , selector.col] <= mitigation[2])
		}


		# report on what we found
		B = B[!is.na(B)]
		M = M[!is.na(M)]

		flush('Found', nrow(d), 'scenarios, of which', length(B), 'baseline and', length(M), 'mitigation\n')

		if (length(B) == 0 | length(M) == 0)
			stop('\nNo data in one of your groups, please adjust your selection criterea!\n')


		# establish the baseline & mitigation results
		baseline  =  data.frame(row = H[B], value = d[B, '2050'], warming=d[B, 'warming'], emissions=d[B, 'emissions'])
		mitigation = data.frame(row = H[M], value = d[M, '2050'], warming=d[M, 'warming'], emissions=d[M, 'emissions'])

		return(list(historical=historical, baseline=baseline, mitigation=mitigation))
	}



####
####  3 panel version
####



	# create a set of plots summarising a variable in the two categories of scenarios
	# return the difference in the variable across pairs, in the scaled units (i.e. in wedges)
	plot_difference = function(data, var, difference, how, scale=1, yline=par('mgp')[1], quick=NULL)
	{
		good_graphics(padding=TRUE)
		par(font.lab=1)
		par(tck=0.015)
		layout(t(1:3), widths=c(8,8,3))

		# get the data
		d = get_variable(data, var)

		# get the units
		units = d$Unit[1]




		###
		## panel 1 - our selected scenarios versus the overall IAMC
		###

		par(mar = c(3, 4.25, 1.5, 1.75))
		
		# chart labels
		chart.xlab = 'Greenhouse gas emissions in 2050 (GtCO₂e)'
		chart.ylab = paste0(gsub('Sequestration', 'Seq.', d$Variable[1], fixed=TRUE), ' (', units, ')')
		
		# plot everything in the iamc
		plot(d[ , 'emissions'], d[ , '2050'], pch=16, col='#DDDDDD66', xlab=chart.xlab, ylab='')
		mtext(chart.ylab, side=2, line=yline, cex=par('cex'), las=0)
		ylab_top('(a)', font=2, at=0.25)
		ylab_top('      Individual IAM results', font=1, at=0.25)

		# plot our two scenarios
		points(difference$baseline$emissions, difference$baseline$value, pch=16, col='#FEC50688')
		points(difference$mitigation$emissions, difference$mitigation$value, pch=16, col='#22B0AA88')

		# plot the historical (2020) value
		y = median(difference$historical$value)
		abline(h=y)
		text_with_shadow(par('usr')[2], y, adj=1, font=2, cex=0.8, shadow.x=3, shadow.y=3, '2020   ')

		# a boxplot to summarise P25, P50, P75
		x = range(difference$baseline$emissions)
		y = quantile(difference$baseline$value, c(0.25,0.50,0.75)) |> as.numeric()
		rect(x[1], y[1], x[2], y[2], border='black', col=ALF('#FEC506', 0.25))
		rect(x[1], y[2], x[2], y[3], border='black', col=ALF('#FEC506', 0.25))

		# a boxplot to summarise P25, P50, P75
		x = range(difference$mitigation$emissions)
		y = quantile(difference$mitigation$value, c(0.25,0.50,0.75)) |> as.numeric()
		rect(x[1], y[1], x[2], y[2], border='black', col=ALF('#22B0AA', 0.25))
		rect(x[1], y[2], x[2], y[3], border='black', col=ALF('#22B0AA', 0.25))


		# add a legend
		pzn = 'topleft'
		inz = c(0.04, 0.02)
		if (mean(d$`2050`[ d$emissions<20 ], na.rm=TRUE) > mean(d$`2050`[ d$emissions>60 ], na.rm=TRUE))
		{
			pzn = 'topright'
			inz = c(0.08, 0.02)
		}
		clz = c('#DDDDDD', '#FFFFFF', '#FFFFFF', '#22B0AA', '#FEC506', '#FFFFFF')
		lbz = c('All scenarios',  'n = ' %&% sum( !is.na(d[ , '2050']) ), '', 'Mitigation scenarios', 'Baseline scenarios', 'n = ' %&% nrow(difference$mitigation))
		add_legend(pzn, legend=lbz, col=clz, pch=16, horiz=FALSE, inset=inz, y.intersp=2)






		###
		## panel 2 - the difference in our variable vs. the difference in emissions
		###

		par(mar = c(3, 2.75, 1.5, 2.75))

		# this is pairwise
		if (how == 'scenario')
		{
			x = difference$baseline$emissions - difference$mitigation$emissions
			y = difference$mitigation$value - difference$baseline$value
			diff = data.frame(emissions=x, value=y, mitigation=difference$mitigation$emissions, baseline=difference$baseline$emissions)
		}

		# this isn't pairwise... so look at all combinations
		# ⚠ deprecated af ⚠
		if (how != 'scenario')
		{
			x = expand.grid(difference$baseline$emissions, difference$mitigation$emissions)
			y = expand.grid(difference$baseline$value, difference$mitigation$value)
			diff = data.frame(emissions = x$Var1-x$Var2, value=y$Var2-y$Var1, mitigation=x$Var2, baseline=x$Var1)
		}

		chart.xlim = range(0, diff$emissions)
		chart.xlim = c(18, 72) # hard code when running our default 381 pairs
		chart.ylim = range(0, diff$value)
		if (scale < 0) chart.ylim = rev(chart.ylim)
		chart.xlab = 'Pair-wise difference in total 2050 emissions (GtCO₂e)'
		chart.ylab = paste0('Pair-wise difference in mitigation effort (', units, ')')

		plot(diff$emissions, diff$value, pch=16, col='#DDDDDD66', xlab=chart.xlab, ylab='', xlim=chart.xlim, ylim=chart.ylim, xpd=NA)
		mtext(chart.ylab, side=2, line=yline, cex=par('cex'), las=0)
		ylab_top('(b)', font=2, at=0.25)
		ylab_top('      Differences between IAM pairs', font=1, at=0.25)

		x = quantile(diff$emissions, c(0.25,0.50,0.75))
		y = quantile(diff$value, c(0.25,0.50,0.75))
		x[2] = mean(diff$emissions)
		y[2] = mean(diff$value)
		##x = mean(diff$emissions) + c(-1, 0, 1) * sd(diff$emissions)
		##y = mean(diff$value) + c(-1, 0, 1) * sd(diff$value)

		error_bars(x[2], y[1], y[3])
		error_bars_x(x[1], x[3], y[2])
		points(x[2], y[2], pch=21, bg='#CAB7D7')



		# add a second scale of wedges (and convert our results)
		if (scale != 1)
		{
			pax = par('usr')[3:4]
			pax = pax + c(-0.1, 0.1) * diff(pax)
			pax = pretty(pax * scale)
			axis(4, at=pax/scale, labels=pax)
			axis_minor_ticks(4, spacing=1/scale)
			chart.ylab = 'Number of wedges deployed'
			mtext(chart.ylab, side=4, line=par('mgp')[1]*1.2, cex=par('cex'), las=0, xpd=NA)

			diff$value = diff$value * scale
			diff$mitigation = diff$mitigation * scale
			diff$baseline = diff$baseline * scale
		}



		###
		## panel 3 - the violin plot
		###

		diff$normalised = diff$value / diff$emissions * 40

		ylim = par('usr')[3:4] * scale

		par(mar = c(3, 2.5, 1.5, 0.5))

		vioplot(diff$value, diff$normalised, names=c('Absolute', 'Normalised'), ylim=ylim, yaxs='i', tck=0.025, wex=1.25,
		        col=ALF(office$p, 0.333), border=NA, rectCol=office$p, colMed=NA, colMed2=NA, pchMed=NA)

		axis_minor_ticks(2, spacing=1)
		ylab_top('(c)', font=2, at=0.25)
		ylab_top('      Summary of (b)', font=1, at=0.25)

		points(1, mean(diff$value), cex=1.5, pch=21, col='black', bg='white')
		points(2, mean(diff$norm), cex=1.5, pch=21, col='black', bg='white')

		
		return(diff)

	}









####
####  4 panel version
####



	# create a set of plots summarising a variable in the two categories of scenarios
	# return the difference in the variable across pairs, in the scaled units (i.e. in wedges)
	plot_difference = function(data, var, difference, how, scale=1, yline=par('mgp')[1], quick=FALSE)
	{
		good_graphics(padding=TRUE)
		par(font.lab=1)
		par(tck=0.015)
		layout(t(1:4), widths=c(6,7.6,7.4,3.5))
		par(cex=1)

		# get the data
		d = get_variable(data, var)

		# get the units
		units = d$Unit[1]



		###
		## panel 1 - time series of all variable outputs 
		###

		if (quick) {
			L = 1
			colours = list(
				null = '#DDDDDD',
				miti = '#22B0AA',
				base = '#FEC506'
			)
		} else {
			L = 4
			colours = list(
				null = '#DDDDDD22',
				miti = '#22B0AA11',
				base = '#FEC50622'
			)
		}

		x = d$id %notin% c(scen_baseline, scen_mitigation)
		ylim = range(d[ , as.character(seq(2020, 2050, 5))], na.rm=TRUE)

		par(mar = c(3, 4.25, 1.5, 0.2))

		ar6_lineplot(d[x, ], col=colours$null, xlim=c(2000,2070), xaxt='n', ylim=ylim, yline=yline, xlab='Year')
		axis(1, labels=FALSE)
		axis(1, at=c(2000, 2020, 2050, 2070), tck=0, tcl=0)

		for (loop in 1:L)
		{
			x = d$id %in% scen_mitigation
			ar6_lineplot(d[x, ], col=colours$miti, add=TRUE)

			x = d$id %in% scen_baseline
			ar6_lineplot(d[x, ], col=colours$base, add=TRUE)
		}


		# summarise the two groups
		my.cols = seq(2000, 2100, 10)

		x = d$id %in% scen_mitigation
		# P50
		summary = apply(d[x, as.character(my.cols)], 2, quantile, probs=0.50, na.rm=TRUE)
		lines(my.cols, summary, col='black', lwd=4.50)
		lines(my.cols, summary, col='#22B0AA', lwd=2.25)

		x = d$id %in% scen_baseline
		# P50
		summary = apply(d[x, as.character(my.cols)], 2, quantile, probs=0.50, na.rm=TRUE)
		lines(my.cols, summary, col='black', lwd=4.50)
		lines(my.cols, summary, col='#FEC506', lwd=2.25)


		# data points for all 2050 values
		# bg = rep('black', nrow(d))
		# x = d$id %in% scen_mitigation
		# bg[x] = '#22B0AA'
		# x = d$id %in% scen_baseline
		# bg[x] = '#FEC506'
		#
		# x = d$id %in% c(scen_baseline, scen_mitigation)
		# points(rep(2050, sum(x)), d[x, '2050'], pch=21, bg=bg[x], col='black', lwd=0.5, cex=0.8)

		axis(4, labels=FALSE)
		box()
		ylab_top('(a)', font=2, cex=1.25, at=0.25)



		###
		## panel 2 - our selected scenarios versus the overall IAMC
		###

		par(mar = c(3, 0.2, 1.5, 2.25))
		
		# chart labels
		chart.xlab = '2050 GHG emissions (GtCO₂e)'
		chart.ylab = ''
		
		# plot everything in the iamc
		plot(d[ , 'emissions'], d[ , '2050'], pch=16, cex=0.8, col='#DDDDDD66', xlab=chart.xlab, ylim=ylim, ylab='', yaxt='n')
		axis(2, labels=FALSE)
		axis(4, labels=FALSE)
		ylab_top('(b)', font=2, cex=1.25, at=0.25)

		# plot our two scenarios
		points(difference$baseline$emissions, difference$baseline$value, pch=16, cex=0.8, col='#FEC50688')
		points(difference$mitigation$emissions, difference$mitigation$value, pch=16, cex=0.8, col='#22B0AA88')

		# plot the historical (2020) value
		y = median(difference$historical$value)
		abline(h=y)
		text_with_shadow(par('usr')[2], y, adj=1, font=2, cex=0.8, shadow.x=3, shadow.y=3, '2020   ')

		# a boxplot to summarise P25, P50, P75
		x = range(difference$baseline$emissions)
		y = quantile(difference$baseline$value, c(0.25,0.50,0.75)) |> as.numeric()
		rect(x[1], y[1], x[2], y[2], border='black', col=ALF('#FEC506', 0.25))
		rect(x[1], y[2], x[2], y[3], border='black', col=ALF('#FEC506', 0.25))

		# a boxplot to summarise P25, P50, P75
		x = range(difference$mitigation$emissions)
		y = quantile(difference$mitigation$value, c(0.25,0.50,0.75)) |> as.numeric()
		rect(x[1], y[1], x[2], y[2], border='black', col=ALF('#22B0AA', 0.25))
		rect(x[1], y[2], x[2], y[3], border='black', col=ALF('#22B0AA', 0.25))


		# add a legend
		pzn = 'topleft'
		inz = c(0.04, 0.02)
		if (mean(d$`2050`[ d$emissions<20 ], na.rm=TRUE) > mean(d$`2050`[ d$emissions>60 ], na.rm=TRUE))
		{
			pzn = 'topright'
			inz = c(0.08, 0.02)
		}
		clz = c('#DDDDDD', '#FFFFFF', '#FFFFFF', '#22B0AA', '#FEC506', '#FFFFFF')
		lbz = c('All scenarios',  'n = ' %&% sum( !is.na(d[ , '2050']) ), '', 'Mitigation scenarios', 'Baseline scenarios', 'n = ' %&% nrow(difference$mitigation))
		add_legend(pzn, legend=lbz, col=clz, pch=16, horiz=FALSE, inset=inz, y.intersp=1.5)






		###
		## panel 3 - the difference in our variable vs. the difference in emissions
		###

		par(mar = c(3, 3, 1.5, 0.2))

		# this is pairwise
		if (how == 'scenario')
		{
			x = difference$baseline$emissions - difference$mitigation$emissions
			y = difference$mitigation$value - difference$baseline$value
			diff = data.frame(emissions=x, value=y, mitigation=difference$mitigation$emissions, baseline=difference$baseline$emissions)
		}

		# this isn't pairwise... so look at all combinations
		# ⚠ deprecated af ⚠
		if (how != 'scenario')
		{
			x = expand.grid(difference$baseline$emissions, difference$mitigation$emissions)
			y = expand.grid(difference$baseline$value, difference$mitigation$value)
			diff = data.frame(emissions = x$Var1-x$Var2, value=y$Var2-y$Var1, mitigation=x$Var2, baseline=x$Var1)
		}

		chart.xlim = range(0, diff$emissions)
		chart.xlim = c(18, 72) # hard code when running our default 381 pairs
		chart.ylim = range(0, diff$value)
		if (scale < 0) chart.ylim = rev(chart.ylim)
		chart.xlab = 'Pair-wise difference in 2050 GHG emissions (GtCO₂e)'
		chart.ylab = paste0('Pair-wise difference in mitigation effort (', units, ')')

		plot(diff$emissions, diff$value, pch=16, cex=0.8, col='#DDDDDD66', xlab=chart.xlab, ylab='', xlim=chart.xlim, ylim=chart.ylim, xpd=NA)
		mtext(chart.ylab, side=2, line=yline, cex=par('cex'), las=0)
		ylab_top('(c)', font=2, cex=1.25, at=0.25)

		x = quantile(diff$emissions, c(0.25,0.50,0.75))
		y = quantile(diff$value, c(0.25,0.50,0.75))
		x[2] = mean(diff$emissions)
		y[2] = mean(diff$value)
		##x = mean(diff$emissions) + c(-1, 0, 1) * sd(diff$emissions)
		##y = mean(diff$value) + c(-1, 0, 1) * sd(diff$value)

		error_bars(x[2], y[1], y[3])
		error_bars_x(x[1], x[3], y[2])
		points(x[2], y[2], pch=21, cex=1.2, bg='#CAB7D7')



		# add a second scale of wedges (and convert our results)
		if (scale != 1)
		{
			pax = par('usr')[3:4]
			pax = pax + c(-0.1, 0.1) * diff(pax)
			pax = pretty(pax * scale)
			axis(4, at=pax/scale, labels=NA)
			axis_minor_ticks(4, spacing=1/scale)
	
			diff$value = diff$value * scale
			diff$mitigation = diff$mitigation * scale
			diff$baseline = diff$baseline * scale
		}



		###
		## panel 4 - the violin plot
		###

		diff$normalised = diff$value / diff$emissions * 40

		ylim = par('usr')[3:4] * scale

		par(mar = c(3, 0.2, 1.5, 2.8))

		vioplot(diff$value, diff$normalised, names=c('Abs.', 'Norm.'), ylim=ylim, yaxs='i', yaxt='n', tck=0.025, wex=1.25,
		        col=ALF(office$p, 0.333), border=NA, rectCol=office$p, colMed=NA, colMed2=NA, pchMed=NA)


		axis(4, tck=0.035)
		axis(2, labels=FALSE, tck=0.035)
		par(tck = 0.035)
		axis_minor_ticks(2, spacing=1)
		axis_minor_ticks(4, spacing=1)
		par(tck = 0.015)
	
		chart.ylab = 'Number of wedges deployed'
		mtext(chart.ylab, side=4, line=par('mgp')[1]*0.95, cex=par('cex'), las=0, xpd=NA)

		ylab_top('(d)', font=2, cex=1.25, at=0.25)

		points(1, mean(diff$value), cex=1.2, pch=21, col='black', bg='white')
		points(2, mean(diff$norm), cex=1.2, pch=21, col='black', bg='white')

		return(diff)

	}





	# extract and simplify the model names from a paired population
	get_model_names = function(v2, d2)
	{
		m = data$Model[v2$mitigation$row]
		m = get_text_before(m, ' ')
		m = get_text_before(m, '_')

		m[ m %contains% 'AIM'] = 'AIM'
		m[ m %contains% 'MESSAGE'] = 'MESSAGE'
		m[ m %contains% 'REMIND'] = 'REMIND'

		m
	}


	# print summary stats about the whole paired population to console
	print_pair_summary = function(d2, units='')
	{
		# mean, P25–P75 for absolute values
		d2m = mean(d2$value) |> round(3)
		d2u = quantile(d2$value, 0.75) |> round(3)
		d2l = quantile(d2$value, 0.25) |> round(3)

		# mean, P25–P75 for normalised values (if every pair scaled linearly to 40 Gt difference)
		n2m = mean(d2$normalised) |> round(3)
		n2u = quantile(d2$normalised, 0.75) |> round(3)
		n2l = quantile(d2$normalised, 0.25) |> round(3)

		flush(d2m, '[', d2l, ',', d2u, '] absolute -', n2m, '[', n2l, ',', n2u, '] normalised\n')

		# P90–P100 of absolute values to reflect maximum potential
		p90 = quantile(d2$value, 0.90) |> round(1)
		pmx = quantile(d2$value, 1.00) |> round(1)
		if (1) flush('  ', p90, '-', pmx, 'upper bound\n')
	}


	# print summary stats about each model's pairs to console
	print_model_summary = function(v2, d2, units='')
	{
		models = get_model_names(v2, d2)

		for (m in sort(unique(models)))
		{
			# just the mean for absoulte and normalised values
			v = mean(d2$value[ models==m ]) |> round(3)
			n = mean(d2$normalised[ models==m ]) |> round(3)
			flush(sprintf('> %8s (%2d) = %2.3f (absolute) - %2.3f (normalised) %s\n', m, sum(models==m), v, n, units))
		}
	}


	# calculate summary stats on the difference in this variable across the pairs
	# returns a 1-row data.frame of statistics
	# 	var = variable name
	#   v2 = the full results output from extract_pairs()
	#   d2 = the summary output from plot_difference()
	#   multi = how many conversion factors have been used to generate these results
	#           (needed so that 'n' values are correctly saved)
	#
	calculate_pair_summary = function(var, v2, d2, multi=1)
	{
		### for the overall population of scenario pairs
		df = data.frame(
			variable = var,

			# n
			paired_N = nrow(d2) / multi,

			# mean, stdev, P25–P50–P75 of absolute values
			paired_mean = mean(d2$value),
			paired_stdev = sd(d2$value),
			paired_P25 = quantile(d2$value, 0.25), 
			paired_P50 = quantile(d2$value, 0.50), 
			paired_P75 = quantile(d2$value, 0.75),

			# mean, stdev, P25–P50–P75 of normalised values (if every pair scaled linearly to 40 Gt difference)
			paired_normalised_mean = mean(d2$normalised),
			paired_normalised_stdev = sd(d2$normalised),
			paired_normalised_P25 = quantile(d2$normalised, 0.25), 
			paired_normalised_P50 = quantile(d2$normalised, 0.50), 
			paired_normalised_P75 = quantile(d2$normalised, 0.75)
		)

		# get the list of models
		models = get_model_names(v2, d2)
		names = c('AIM', 'GCAM', 'GEM-E3', 'IMAGE', 'MESSAGE', 'POLES', 'REMIND', 'TIAM-ECN', 'WITCH')

		# get how many results we have from each model
		n = table(models)
		n = data.frame(name=names, count=as.integer(n[names]))
		n$count[ is.na(n$count) ] = 0
		n = as.list(setNames(n$count, n$name))

		### for the individual models 
		for (m in names)
		{
			M = make.names(m)

			# n
			df[ , M %&% '_N'] = n[[m]]

			# mean, stdev, P25–P50–P75 of absolute values
			df[ , M %&% '_mean'] = ifelse(n[[m]] < 1, NA, mean(d2$value[models == m]))
			df[ , M %&% '_sd']   = ifelse(n[[m]] < 3, NA, sd(d2$value[models == m]))
			df[ , M %&% '_P25']  = ifelse(n[[m]] < 3, NA, quantile(d2$value[models == m], 0.25))
			df[ , M %&% '_P50']  = ifelse(n[[m]] < 1, NA, quantile(d2$value[models == m], 0.50))
			df[ , M %&% '_P75']  = ifelse(n[[m]] < 3, NA, quantile(d2$value[models == m], 0.75))

			# mean, stdev, P25–P50–P75 of normalised values (if every pair scaled linearly to 40 Gt difference)
			df[ , M %&% '_normalised_mean'] = ifelse(n[[m]] < 3, NA, quantile(d2$normalised[models == m], 0.25))
			df[ , M %&% '_normalised_sd']   = ifelse(n[[m]] < 3, NA, sd(d2$normalised[models == m]))
			df[ , M %&% '_normalised_P25']  = ifelse(n[[m]] < 3, NA, quantile(d2$normalised[models == m], 0.25))
			df[ , M %&% '_normalised_P50']  = ifelse(n[[m]] < 1, NA, quantile(d2$normalised[models == m], 0.50))
			df[ , M %&% '_normalised_P75']  = ifelse(n[[m]] < 3, NA, quantile(d2$normalised[models == m], 0.75))
		}
		
		# boom
		df
	}


	plot_print_save_pairs = function(data, var, scale=1, units='', yline=NULL, quick=FALSE)
	{
		# extract the relavent variable for our baseline and mitigation scenarios (plus historical)
		v2 = extract_pairs(data, var, how='scenario', baseline=scen_baseline, mitigation=scen_mitigation)

		# plot the results and calculate the difference between pairs in units of wedges
		if (length(scale) == 1)
		{
			# do this for a single conversion factor
			d2 = plot_difference(data, var, v2, 'scenario', scale=scale, yline=yline, quick=quick)
		} else {
			# do this for multiple conversion factors (e.g. for our four baseline scenarios)
			d2 = NULL
			for (i in 1:length(scale))
				d2 = push_row(d2, plot_difference(data, var, v2, 'scenario', scale=scale[i], yline=yline, quick=quick))
		}

		# summarise to the console
		print_pair_summary(d2, units)
		print_model_summary(v2, d2, units)

		# summarise into a data.frame for saving results
		return(calculate_pair_summary(var, v2, d2, multi=length(scale)))
	}

