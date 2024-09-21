#########################################################################
##  
##  Calculate the relationship between temperature and emissions 
##  across the AR6 database scenarios
##  
##  

	# set working directory
	setwd('C:/stabilisation-wedges-2025/')

	# load the ar6 database and helper functions
	source('blam_library.r')
	source('ar6_library.r')


	# get emissions
	co2 = data[ data$Variable == 'Emissions|CO2', ]
	ghg = data[ data$Variable == 'AR6 climate diagnostics|Native-with-Infilled|Emissions|Kyoto Gases (AR6-GWP100)', ]

	# get the two temperature measures
	tfair = data[ data$Variable == 'AR6 climate diagnostics|Surface Temperature (GSAT)|FaIRv1.6.2|50.0th Percentile', ]
	tmagic = data[ data$Variable == 'AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile', ]

	# use magicc as the preferred option (based on conversation with J Skea)
	t50 = data[ data$Variable == 'AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile', ]
	t33 = data[ data$Variable == 'AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|33.0th Percentile', ]
	t67 = data[ data$Variable == 'AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|67.0th Percentile', ]

	# align all these together
	idx_co2 = paste(co2$Model, co2$Scenario, co2$Region)
	idx_ghg = paste(ghg$Model, ghg$Scenario, ghg$Region)
	idx_t33 = paste(t33$Model, t33$Scenario, t33$Region)
	idx_t50 = paste(t50$Model, t50$Scenario, t50$Region)
	idx_t67 = paste(t67$Model, t67$Scenario, t67$Region)

	idx = sort(unique(c(idx_co2, idx_ghg, idx_t33, idx_t50, idx_t67)))
	a = (idx %in% idx_co2) & (idx %in% idx_ghg) & (idx %in% idx_t50)
	idx = idx[a]

	co2 = co2[ match(idx, idx_co2), ]
	ghg = ghg[ match(idx, idx_ghg), ]
	t33 = t33[ match(idx, idx_t33), ]
	t50 = t50[ match(idx, idx_t50), ]
	t67 = t67[ match(idx, idx_t67), ]



	# plot temperature vs ghg emissions
	df = data.frame(idx=idx, ghg2050=ghgcum, tlo2050=t33$`2050`, tmu2050=t50$`2050`, thi2050=t67$`2050`, tlo2100=t33$`2100`, tmu2100=t50$`2100`, thi2100=t67$`2100`)

	layout(t(1:2))
	par(mar=c(3.0, 3.5, 2.0, 1.5))

	plot_p(df$ghg, df$thi2050, col=alpha(office$o, 0.1), ylim=c(0,5.5), xlim=c(0,2750), xlab='Cumulative GHG emissions 2020 to 2050 (GtCO2e)', ylab='Temperature rise in 2050 (*C)', yline=2)
	points(df$ghg, df$tmu2050, col=alpha('grey70', 0.1), pch=16, cex=0.75)
	points(df$ghg, df$tlo2050, col=alpha(office$t, 0.1), pch=16, cex=0.75)

	fit_hi_2050 = smooth_spline(df$ghg, df$thi2050, xres=10)
	lines(fit_hi_2050$x, fit_hi_2050$fit, lwd=5, col='white')
	lines(fit_hi_2050$x, fit_hi_2050$fit, lwd=2, col=office$r)
	fit_mu_2050 = smooth_spline(df$ghg, df$tmu2050, xres=10)
	lines(fit_mu_2050$x, fit_mu_2050$fit, lwd=5, col='white')
	lines(fit_mu_2050$x, fit_mu_2050$fit, lwd=2, col='grey30')
	fit_lo_2050 = smooth_spline(df$ghg, df$tlo2050, xres=10)
	lines(fit_lo_2050$x, fit_lo_2050$fit, lwd=5, col='white')
	lines(fit_lo_2050$x, fit_lo_2050$fit, lwd=2, col=office$b)

	plot_p(df$ghg, df$thi2100, col=alpha(office$o, 0.1), ylim=c(0,5.5), xlim=c(0,2750), xlab='Cumulative GHG emissions 2020 to 2050 (GtCO2e)', ylab='Temperature rise in 2100 (*C)', yline=2)
	points(df$ghg, df$tmu2100, col=alpha('grey70', 0.1), pch=16, cex=0.75)
	points(df$ghg, df$tlo2100, col=alpha(office$t, 0.1), pch=16, cex=0.75)

	fit_hi_2100 = smooth_spline(df$ghg, df$thi2100, xres=10)
	lines(fit_hi_2100$x, fit_hi_2100$fit, lwd=5, col='white')
	lines(fit_hi_2100$x, fit_hi_2100$fit, lwd=2, col=office$r)
	fit_mu_2100 = smooth_spline(df$ghg, df$tmu2100, xres=10)
	lines(fit_mu_2100$x, fit_mu_2100$fit, lwd=5, col='white')
	lines(fit_mu_2100$x, fit_mu_2100$fit, lwd=2, col='grey30')
	fit_lo_2100 = smooth_spline(df$ghg, df$tlo2100, xres=10)
	lines(fit_lo_2100$x, fit_lo_2100$fit, lwd=5, col='white')
	lines(fit_lo_2100$x, fit_lo_2100$fit, lwd=2, col=office$b)

	add_legend('topleft', pch=c(16,16), col=alpha(c(office$o, 'grey70', office$t), 0.5), legend=c('67th percentile', '50th percentile', '33rd percentile'), horiz=FALSE, inset=c(0.02,0.01))



	# save the results
	results = data.frame(
		ghg = fit_mu_2100$x,
		p33 = fit_lo_2100$fit,
		p50 = fit_mu_2100$fit,
		p67 = fit_hi_2100$fit
	)
