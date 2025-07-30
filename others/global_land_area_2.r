
#####
## ##  LOAD AND PROCESS THE CCI DATA
#####

	library(terra)
	r = rast('cci_2022_1m.tif')

	# classify crops as 1, pasture as 2, forests as 3
	esa = r
	values(esa) = 0
	values(esa)[ values(r) %in% c(10, 11, 12, 20) ] = 1
	values(esa)[ values(r) %in% c(120, 130, 150) ] = 2
	values(esa)[ values(r) >= 50 & values(r) <= 100] = 3

	plot(esa, xlim=c(-10,20), ylim=c(45,65))


	# calculate the total land areas
	cs = cellSize(esa, unit = 'ha')
	area = list()

	a = mask(cs, esa != 1, maskvalues = TRUE)
	area$cropland = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = mask(cs, esa != 2, maskvalues = TRUE)
	area$pasture = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = mask(cs, esa != 3, maskvalues = TRUE)
	area$forest = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6


	# the data for Table S22
	str(area)

	# Our World in Data has some general benchmarks we can compare to
	# for [forest cover](https://ourworldindata.org/forest-area)
	# and [agriculture uses](https://ourworldindata.org/land-use)
	#
	#             OWID    This   
	#   crops     1.63    1.89	(+16%)
	#   pasture   3.20    3.31  (+4%)
	#   forest    4.05    4.41  (+9%)




#####
## ##  OVERLAY WITH FUTURE BIOMES (c. 2055 middling climate)
#####

	# load the aligned koppen-geiger map
	r = rast('./2041_2070/ssp245/koppen_geiger_0p01666666.tif')

	# map tropical to 1, temperate to 2, boreal to 3, deserts to 4
	biome = r
	values(biome) = 0
	values(biome)[ values(r) >=  1 & values(r) <=  3] = 1
	values(biome)[ values(r) >=  8 & values(r) <= 16] = 2
	values(biome)[ values(r) >= 17 & values(r) <= 28] = 3
	values(biome)[ values(r) >=  4 & values(r) <=  7] = 4
	values(biome)[ values(r) >= 29 & values(r) <= 30] = 4

	# bang tidy
	a = (esa == 1) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_tropical_cropland = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (esa == 1) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_temperate_cropland = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (esa == 2) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_tropical_pasture = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (esa == 2) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_temperate_pasture = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (esa == 3) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_tropical_forest = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (esa == 3) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_temperate_forest = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6




#####
## ##  OVERLAY WITH HISTORICAL BIOMES (c. 2005)
#####

	# load the aligned koppen-geiger map
	r = rast('./1991_2020/koppen_geiger_0p01666666.tif')

	# map tropical to 1, temperate to 2, boreal to 3, deserts to 4
	biome = r
	values(biome) = 0
	values(biome)[ values(r) >=  1 & values(r) <=  3] = 1
	values(biome)[ values(r) >=  8 & values(r) <= 16] = 2
	values(biome)[ values(r) >= 17 & values(r) <= 28] = 3
	values(biome)[ values(r) >=  4 & values(r) <=  7] = 4
	values(biome)[ values(r) >= 29 & values(r) <= 30] = 4

	# bosh
	a = (esa == 1) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_tropical_cropland = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (esa == 1) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_temperate_cropland = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (esa == 2) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_tropical_pasture = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (esa == 2) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_temperate_pasture = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (esa == 3) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_tropical_forest = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (esa == 3) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_temperate_forest = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6




#####
## ##  THE CORE RESULTS
#####

	# the data for Table S23
	str(area)

	# List of 15
	# $ cropland            : num 1895
	# $ pasture             : num 3315
	# $ forest              : num 4406
	# $ f_tropical_cropland : num 520
	# $ f_temperate_cropland: num 461
	# $ f_tropical_pasture  : num 358
	# $ f_temperate_pasture : num 272
	# $ f_tropical_forest   : num 1709
	# $ f_temperate_forest  : num 650
	# $ h_tropical_cropland : num 460
	# $ h_temperate_cropland: num 463
	# $ h_tropical_pasture  : num 343
	# $ h_temperate_pasture : num 273
	# $ h_tropical_forest   : num 1617
	# $ h_temperate_forest  : num 691




#####
## ##  DRAW THE MAPS
#####

	# convert land use to factors
	m = esa
	m[m == 0] = NA
	m = as.factor(m)
	levels(m) = data.frame(
		id    = c(1, 2, 3),
		label = c('Cropland', 'Pasture', 'Forest')
	)

	# assign some colours
	cols = c('goldenrod1', 'sienna3', 'darkgreen')

	# load a world map
	source_maps()
	world = get_world_map('medium')


	# plot Figure S31a
	dev.new(width=14)
	good_graphics(mar=rep(0.25,4))
	# show the global land use
	plot(m, col=cols, legend=TRUE, levels=TRUE, colNA='transparent', xlim=c(-170,180), ylim=c(-60,85), asp=1.15,
	     axes=FALSE, box=FALSE, plg=list(x=-170, y=-32, y.intersp=1.8), maxcell=10800*21600/3/3)
	# make non-tropical areas super pale
	a = (biome != 1); a[ a==0 ] = NA
	# cut out a box in the pacific for our legend
	a[ ext(-180, -120, -60, -30) ] = NA
	plot(a, col=ALF('white', 0.9), add=TRUE, legend=FALSE, axes=FALSE, box=FALSE, maxcell=10800*21600/3/3)
	add_map(world$map, lwd=0.3)
	text(-160, 80, font=2, cex=0.9, '(a)')
	good_png('m:/work/wedges_fig_gis_esa_tropical_v3.png', height=8*1200/2.54, width=16*1200/2.54, res=1200/1.7)


	# plot Figure S32a
	good_graphics(mar=rep(0.25,4))
	# show the global land use
	plot(m, col=cols, legend=TRUE, levels=TRUE, colNA='transparent', xlim=c(-170,180), ylim=c(-60,85), asp=1.15,
	     axes=FALSE, box=FALSE, plg=list(x=-170, y=-32, y.intersp=1.8), maxcell=10800*21600/3/3)
	# make non-temperate areas super pale
	a = (biome != 2); a[ a==0 ] = NA
	# cut out a box in the pacific for our legend
	a[ ext(-180, -120, -60, -30) ] = NA
	plot(a, col=ALF('white', 0.9), add=TRUE, legend=FALSE, axes=FALSE, box=FALSE, maxcell=10800*21600/3/3)
	add_map(world$map, lwd=0.3)
	text(-160, 80, font=2, cex=0.9, '(a)')
	good_png('m:/work/wedges_fig_gis_esa_temperate_v3.png', height=8*1200/2.54, width=16*1200/2.54, res=1200/1.7)
