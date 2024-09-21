#############################################################
#####       ____    _                       _____       #####
#####      |  _ \  | |                     |  __ \      #####
#####      | |_) | | |   __ _   _ __ ___   | |__) |     #####
#####      |  _ <  | |  / _` | | '_ ` _ \  |  _  /      #####
#####      | |_) | | | | (_| | | | | | | | | | \ \      #####
#####      |____/  |_|  \__,_| |_| |_| |_| |_|  \_\     #####
#####                                                   #####
#############################################################
#####      14-09-2024       #####     IAIN STAFFELL     #####
#############################################################


list_to_csv = function(l, filename)
{
	# find the length of the longest vector
	max_length = max(sapply(l, length))

	# pad shorter vectors with NAs
	padded_list = lapply(l, function(x) { length(x) = max_length; x })

	# convert to data frame
	df = as.data.frame(padded_list)

	# write to CSV with blanks in place of nas
	write.csv(df, filename, row.names=FALSE, na='')
}


# concatenate strings
`%&%` = function(a, b) paste0(a, b)


# find what things aren't in something else
`%notin%` = Negate(`%in%`)


# find what strings contain something or several things
`%contains%` = function(a, b)
{
	ok = rep(FALSE, length(a))

	for (each_b in b)
	{
		ok = ok | grepl(each_b, a, fixed=TRUE)
	}

	ok
}


# console - flush line and clear line
flush = function(...)
{
	cat(...)
	flush.console()
}


# get a smooth spline, with prediction intervals
# basically a wrapper for mgcv::gam() to make it convenient
# if xout is a single number, give that many breaks
# if xout is a vector, use that
# if xres is given, use that to determine xout
smooth_spline = function(x, y, xout=100, xres=NULL)
{
	# get the base fit
	library(mgcv)
	fit = gam(y ~ s(x))

	# get our x limits
	xlim = as.numeric( quantile(x, c(0.01, 0.99), na.rm=TRUE) )

	if (!is.null(xres))
	{
		xlim = round_n(xlim, xres)
		xout = seq(xlim[1], xlim[2], xres)

	} else {

		if (length(xout) == 1)
			xout = seq(min(x), max(x), length.out=xout)
	}

	# get the GAM prediction
	prd = predict(fit, newdata = data.frame(x=xout), type='link', se.fit=TRUE)

	# calculate the prediction interval
	sigma = sd(residuals(fit))
	stdev = sqrt(prd$se.fit^2 + sigma^2)

	# return
	data.frame(
		x = xout,
		fit = prd$fit,
		lo = prd$fit - 1.96 * stdev,
		hi = prd$fit + 1.96 * stdev
	)

}


# you have a bunch of numbers...
# you want them rounded to the nearest from a set of other numbers
# or the nearest of a multiple of numbers 
round_n = function(numbers, breaks, plot=FALSE)
{
	if (length(breaks) == 1) {

		# round our numbers to the nearest multiple
		rounded = round(numbers/breaks) * breaks

	} else {

		# filter out NA first
		nmbrs = numbers[ !is.na(numbers) ]

		# round our numbers to custom breaks
		breaks = c(-Inf, sort(breaks))
		nearest = findInterval(nmbrs, breaks)

		# get the upper/lower limits for each number
		lower = breaks[nearest]
		upper = c(breaks, Inf)[nearest+1]

		# pick the closest
		use_upper = (upper - nmbrs) < (nmbrs - lower)
		use_upper[ is.na(use_upper) ] = TRUE
		rndd = lower
		rndd[use_upper] = upper[use_upper]

		# splice them back in
		rounded = numbers
		rounded[ !is.na(rounded) ] = rndd

	}

	if (plot)
	{
		# show folks what's going on
		plot(numbers, lower, pch=16, cex=0.5, col='grey')
		points(numbers, upper, pch=16, cex=0.5, col='grey')
		for (r in breaks) abline(v=r, col='red')
		points(numbers, rounded, pch=16)
	}

	rounded
}


# generate a sequence of random numbers following a given distribution
# but, constrain the distribution to lie within set bounds (min to max)
# any numbers lying outside those bounds will be re-evaluated
# e.g. constrained_distro('rnorm', 1000, 1, 2, 0, 1)
constrained_distro = function(distro, length, min, max, ...)
{
	# e.g. rnorm(length, parm1, parm2)
	x = do.call(distro, list(length, ...) )
	while (1)
	{
		filter = (x < min | x > max)
		if (sum(filter) == 0) break
		x[filter] = do.call(distro, list(sum(filter), ...) )
	}
	x
}  

# monte carlo function
monte_carlo = function(N, mean, min=NULL, max=NULL)
{
	if (is.null(min) | is.null(max))
		return(rep(mean, N))

	constrained_distro('rnorm', N, min=min, max=max, mean=mean, sd=(max-min)/3)
}



###################################################################################################################
##################      GRAPHICS       ############################################################################
###################################################################################################################


###
# #  set up good graphics parameters..  
# #    pass your own values for mar (margins) or mgp (distance between axes and labels)
# #    options to keep the interior padding, or rotated y-axis labels
###
good_graphics = function(
	mar = c(2.5, 2.5, 2, 1.5),
	mar.left=NULL, mar.right=NULL, mar.top=NULL, mar.bottom=NULL,
	mgp = c(1.75, 0.5, 0),
	padding = FALSE,
	captions = TRUE,
	font='Segoe')
{
	# customise our mar if desired
	if (!is.null(mar.bottom)) mar[1] = mar.bottom
	if (!is.null(mar.left))   mar[2] = mar.left
	if (!is.null(mar.top))    mar[3] = mar.top
	if (!is.null(mar.right))  mar[4] = mar.right

	# maximise graphics space
	par(bg="white")					# white background for drawing pngs
	par(mar=mar)					# shrink the margins (bottom, left, top, right)
	par(tck=0.01)					# tick marks go inwards
	par(mgp=mgp)					# move axis captions and labels closer
	par(font.lab=2)					# bold axis captions

	# big man font ting
	if (!is.null(font))
	{
		if (font == 'Segoe')     font = 'Segoe UI'
		if (font == 'Univers')   font = 'Univers Next Pro'
		if (font == 'Helvetica') font = 'HelveticaNeueLT Com 55 Roman'
		if (font == 'Avenir')    font = 'Avenir LT 55 Roman'
		if (font == 'Bierstadt') font = 'Bierstadt'
		if (font == 'Noto')      font = 'Noto Sans'

		windowsFonts(sans=font)
		par(family='sans')

		par.font <<- font
	}
	

	if (captions == TRUE)  par(las=1)				# make all tick-mark labels horizontal
	if (captions == FALSE) par(las=0)

	if (padding == FALSE) par(xaxs='i', yaxs='i')	# remove padding between the graph and the axes)
	if (padding == 'y')   par(xaxs='i', yaxs='r')	# just remove padding from the x axis (so y axis is padded)
	if (padding == 'x')   par(xaxs='r', yaxs='i')	# just remove padding from the y axis (so x axis is padded)
	if (padding == TRUE)  par(xaxs='r', yaxs='r')
}



###
# #  print a decent quality png of your graph
# #    pass the filename to save as
# #    pass your own height and width (in pixels)
# #    pass your own resolution (in ppi), or more easily a 'zoom' level
# #    the defaults equate to ~8pt font at 8.5cm width
# #    if you have GraphicsMagick installed you can also oversample your image - generate a larger image and then resize
# #     - e.g. set oversample to 2, 3, 5, 9, etc.
###
good_png = function(filename, height=2500, width=2500, zoom=1, res=NULL, oversample=1)
{
	if (is.null(res))
		res = 300 * zoom * sqrt(height*oversample * width*oversample) / 2500
	
	dev.print(png, file=filename, height=height*oversample, width=width*oversample, res=res)

	if (oversample > 1)
	{
		cmd = paste0('gm convert "', filename, '" -resize ', width, 'x', height, ' "', filename, '" -density', round(res/oversample))
		cmd = paste0('gm convert "', filename, '" -resize ', width, 'x', height, ' "', filename, '"')
		n = system(cmd)

		if (n != 0)
			stop('good_png: oversample command did not work. check you have graphicsmagick installed!\n')
	}
}


# set paper = A4r for landscape
good_pdf = function(filename, height=2500, width=2500, zoom=1, paper='special')
{
	height = height / width * 10.5 / zoom
	width  = 10.5 / zoom

	dev.print(pdf, file=filename, height=height, width=width, paper=paper)
}


xlab = function(text, at=1.5)
{
	title(xlab=text, mgp=c(at,1,0))
}

ylab = function(text, at=1.5, cex.main=1)
{
	title(ylab=text, mgp=c(at,1,0), cex.main=cex.main)
}

ylab_top = function(text, at=0.1, cex=1, adj=0, font=2)
{
	if (max(par('mfrow')) == 2) cex = cex * 0.83
	if (max(par('mfrow')) > 2) cex = cex * 0.66

	mtext(side=3, text, line=at, cex=cex, font=font, adj=adj)
}


##
##  simple plot functions for lazy fuckers
##
plot_p = function(x, y=NULL, pch=16, cex=0.75, col=rgb(0,0,0,0.1), xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, xlim=NULL, ylim=NULL, pad=c(0,0,0,0), percent='', ylab='', ylab_top=FALSE, yline=FALSE, cex.main=1, cex.lab=1, ...)
{
	# deal with passing a single variable
	if (is.null(y))
	{
		if (is.vector(x))
		{
			y = x
			x = seq_along(y)

		} else {

			y = x[ , 2]
			x = x[ , 1]
		}

	}

	# default axis limits
	if (is.null(xlim)) xlim = range(x, na.rm=TRUE)
	if (is.null(ylim)) ylim = range(y, na.rm=TRUE)

	# inject padding
	xlim = xlim + diff(range(x, na.rm=TRUE)) * c(-0.02, 0.02) * pad[c(2,4)]
	ylim = ylim + diff(range(y, na.rm=TRUE)) * c(-0.02, 0.02) * pad[c(1,3)]

	# modified axis limits
	if (!is.null(xmin)) xlim[1] = xmin
	if (!is.null(xmax)) xlim[2] = xmax
	if (!is.null(ymin)) ylim[1] = ymin
	if (!is.null(ymax)) ylim[2] = ymax

	# deal with having ylab at the top
	if (ylab_top == FALSE) {
		ytop = ''
	} else {
		ytop = ylab
		ylab = ''
	}

	# deal with putting the ylab on a different line
	if (yline)
	{
		yline_lab = ylab
		ylab = ''
	}

	# plot
	if (percent == '')
	{
		plot(x, y, pch=pch, cex=cex, col=col, xlim=xlim, ylim=ylim, ylab=ylab, ...)
	}

	if (percent == 'x')
	{
		plot(x, y, pch=pch, cex=cex, col=col, xlim=xlim, ylim=ylim, xaxt='n', ylab=ylab, ...)
		axis_percent(1)
	}

	if (percent == 'y')
	{
		plot(x, y, pch=pch, cex=cex, col=col, xlim=xlim, ylim=ylim, yaxt='n', ylab=ylab, ...)
		axis_percent(2)
	}

	if (percent == 'xy')
	{
		plot(x, y, pch=pch, cex=cex, col=col, xlim=xlim, ylim=ylim, xaxt='n', yaxt='n', ylab=ylab, ...)
		axis_percent(1)
		axis_percent(2)
	}

	if (ylab_top)
		ylab_top(ytop, cex=cex.lab)
	
	if (yline)
		ylab(yline_lab, at=yline, cex.main=cex.main)
}


plot_l = function(x, y=NULL, xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, xlim=NULL, ylim=NULL, pad=c(0,0,0,0), percent='', ylab='', ylab_top=FALSE, yline=FALSE, cex.main=1, cex.lab=1, ...)
{
	# deal with passing a single variable
	if (is.null(y))
	{
		if (is.vector(x))
		{
			y = x
			x = seq_along(y)

		} else {

			y = x[ , 2]
			x = x[ , 1]
		}

	}

	# default axis limits
	if (is.null(xlim)) xlim = range(x, na.rm=TRUE)
	if (is.null(ylim)) ylim = range(y, na.rm=TRUE)

	# inject padding
	xlim = xlim + diff(range(x, na.rm=TRUE)) * c(-0.02, 0.02) * pad[c(2,4)]
	ylim = ylim + diff(range(y, na.rm=TRUE)) * c(-0.02, 0.02) * pad[c(1,3)]

	# modified axis limits
	if (!is.null(xmin)) xlim[1] = xmin
	if (!is.null(xmax)) xlim[2] = xmax
	if (!is.null(ymin)) ylim[1] = ymin
	if (!is.null(ymax)) ylim[2] = ymax

	# deal with having ylab at the top
	if (ylab_top == FALSE) {
		ytop = ''
	} else {
		ytop = ylab
		ylab = ''
	}

	# deal with putting the ylab on a different line
	if (yline)
	{
		yline_lab = ylab
		ylab = ''
	}

	# plot
	if (percent == '')
	{
		plot(x, y, type='l', xlim=xlim, ylim=ylim, ylab=ylab, ...)
	}

	if (percent == 'x')
	{
		plot(x, y, type='l', xlim=xlim, ylim=ylim, xaxt='n', ylab=ylab, ...)
		axis_percent(1)
	}

	if (percent == 'y')
	{
		plot(x, y, type='l', xlim=xlim, ylim=ylim, yaxt='n', ylab=ylab, ...)
		axis_percent(2)
	}

	if (percent == 'xy')
	{
		plot(x, y, type='l', xlim=xlim, ylim=ylim, xaxt='n', yaxt='n', ylab=ylab, ...)
		axis_percent(1)
		axis_percent(2)
	}

	if (ylab_top)
		ylab_top(ytop, cex=cex.lab)
	
	if (yline)
		ylab(yline_lab, at=yline, cex.main=cex.main)
}


# my office colour palette
library(grDevices)
office = list(
	yellow   = rgb(255, 207,   1, max=255),
	blue     = rgb( 24,  44, 196, max=255),
	red      = rgb(220,   0,  26, max=255),
	green    = rgb( 46, 140,  57, max=255),
	purple   = rgb( 95,  35, 135, max=255),
	turqoise = rgb( 11, 173, 217, max=255),
	orange   = rgb(255, 133,  41, max=255),
	metal    = rgb(196, 189, 151, max=255),

	lightblue = rgb( 99, 115, 236, max=255)
)


# plot error bars
error_bars = function(x, ylo, yhi, width=0.02, ...)
{
	arrows(x, ylo, x, yhi, code=3, length=width, angle=90, ...)
}
error_bars_x = function(xlo, xhi, y, width=0.02, ...)
{
	arrows(xlo, y, xhi, y, code=3, length=width, angle=90, ...)
}


# a list of parameters to make boxplots nice
# see http://stat.ethz.ch/R-manual/R-devel/library/graphics/html/bxp.html
boxplot.pars = list(boxfill = 'grey90', whisklty=1, whiskcol='grey40', outpch=16, outcex=0.4, outcol='grey70')

#  add transparency to a given colour (e.g. named one)
#  http://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color
alpha = function(colour, alpha=0.5)
{
	if ( all(alpha <= 1) ) alpha = alpha * 255
	colour = col2rgb(colour)
	apply(colour, 2, function(c) { rgb(red=c[1], green=c[2], blue=c[3], alpha=alpha, maxColorValue=255) } )
}


# some defaults to make legends better.  enables 'above' as a position, meaning centered in the top margin
add_legend = function(position, legend, inset=c(0, 0), horiz=TRUE, bty='n', ncol=1, nrow=1, y.intersp=1.5, ...)
{
	if (position %contains% 'above')
	{
		position = gsub('above', 'top', position)
		inset = inset + c(0, -0.02 * (par('mar')[3] + 1))
	}

	if (horiz & ncol > 1)
	{
		horiz = FALSE
	}

	if (horiz & nrow > 1)
	{
		horiz = FALSE
		ncol = ceiling(length(legend) / nrow)
		inset = inset - nrow * 0.01
	}

	legend(position, legend, inset=inset, horiz=horiz, ncol=ncol, bty=bty, xpd=NA, y.intersp=y.intersp, ...)
}


plot_area = function(x, y1, y2, ...)
{
	if (length(y1) == 1)
		y1 = rep(y1, length(x))
	if (length(y2) == 1)
		y2 = rep(y2, length(x))
	
	# if necessary, split into seperate groups based on NA
	# to avoid it drawing wrong
	if (any(is.na(c(x,y1,y2))))
	{
		pos = which(is.na(x) | is.na(y1) | is.na(y2))
		pos = findInterval(seq_along(x), pos+1)

		s = split(data.frame(x=x,y1=y1,y2=y2), pos)
		s = lapply(s, na.everyone.remove)

		for (ss in s)
			plot_area(ss$x, ss$y1, ss$y2, ...)

	} else {

		# now draw it
		x = c(x, rev(x))
		y = c(y1, rev(y2))
		polygon(x, y, ...)
	}
}

