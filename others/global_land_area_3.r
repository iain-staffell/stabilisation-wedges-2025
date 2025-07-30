
#####
## ##  LOAD AND PROCESS THE MODIS DATA
#####

	library(terra)
	r = rast('modis_1m.tif')

	# classify crops as 1, pasture as 2, forests as 3
	modis = r
	values(modis) = 0
	values(modis)[ values(r) %in% c(12, 14) ] = 1
	values(modis)[ values(r) %in% c(10) ] = 2
	values(modis)[ values(r) %in% c(1:5, 8) ] = 3

	plot(modis, xlim=c(-10,20), ylim=c(45,65))


	# calculate the total land areas
	cs = cellSize(modis, unit = 'ha')
	area = list()

	a = mask(cs, modis != 1, maskvalues = TRUE)
	area$cropland = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = mask(cs, modis != 2, maskvalues = TRUE)
	area$pasture = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = mask(cs, modis != 3, maskvalues = TRUE)
	area$forest = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6


	# the data for Table S22
	str(area)

	# Our World in Data has some general benchmarks we can compare to
	# for [forest cover](https://ourworldindata.org/forest-area)
	# and [agriculture uses](https://ourworldindata.org/land-use)
	#
	#             OWID    This   
	#   crops     1.63    1.35	(-17%)
	#   pasture   3.20    3.10  (-3%)
	#   forest    4.05    3.39  (-16%)




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
	a = (modis == 1) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_tropical_cropland = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (modis == 1) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_temperate_cropland = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (modis == 2) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_tropical_pasture = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (modis == 2) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_temperate_pasture = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (modis == 3) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$f_tropical_forest = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (modis == 3) & (biome == 2)
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
	a = (modis == 1) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_tropical_cropland = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (modis == 1) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_temperate_cropland = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (modis == 2) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_tropical_pasture = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (modis == 2) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_temperate_pasture = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (modis == 3) & (biome == 1)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_tropical_forest = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6

	a = (modis == 3) & (biome == 2)
	a = mask(cs, a != 1, maskvalues = TRUE)
	area$h_temperate_forest = global(a, 'sum', na.rm=TRUE)[1,1] / 1e6




#####
## ##  THE CORE RESULTS
#####

	# the data for Table S23
	str(area)

	# List of 15
	# $ cropland            : num 1352
	# $ pasture             : num 3105
	# $ forest              : num 3391
	# $ f_tropical_cropland : num 335
	# $ f_temperate_cropland: num 385
	# $ f_tropical_pasture  : num 516
	# $ f_temperate_pasture : num 277
	# $ f_tropical_forest   : num 1444
	# $ f_temperate_forest  : num 596
	# $ h_tropical_cropland : num 278
	# $ h_temperate_cropland: num 379
	# $ h_tropical_pasture  : num 491
	# $ h_temperate_pasture : num 297
	# $ h_tropical_forest   : num 1382
	# $ h_temperate_forest  : num 599




#####
## ##  DRAW THE MAPS
#####

	# convert land use to factors
	m = modis
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


	# plot Figure S31b
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
	text(-160, 80, font=2, cex=0.9, '(b)')
	good_png('m:/work/wedges_fig_gis_modis_tropical_v3.png', height=8*1200/2.54, width=16*1200/2.54, res=1200/1.7)


	# plot Figure S32b
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
	text(-160, 80, font=2, cex=0.9, '(b)')
	good_png('m:/work/wedges_fig_gis_modis_temperate_v3.png', height=8*1200/2.54, width=16*1200/2.54, res=1200/1.7)
