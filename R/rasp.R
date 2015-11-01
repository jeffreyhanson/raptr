#' @include RcppExports.R raspr-internal.R misc.R generics.R RaspReliableOpts.R RaspUnreliableOpts.R GurobiOpts.R RaspData.R RaspUnsolved.R RaspResults.R RaspSolved.R
NULL

#' Generate prioritisations using RASP
#'
#' This is a general function to create Rasp objects from scratch and solve them to generate solutions.
#'
#' @param pus \code{SpatialPolyogns} object representing planning units.
#' @param species \code{Raster} object with species distribution data.
#' @param ... arguments are passed to \code{GurobiOpts}, \code{RaspData}, and \code{RaspReliableOpts} or \code{RaspUnreliableOpts} functions.
#' @param formulation \code{character}  to indicate if 'unreliable' or 'reliable' formulation should be used to generate prioritisations. Defaults to 'unreliable'.
#' @param solve \code{logical} should solutions be generated?
#' @export
#' @note Type \code{vignette('raspr')} tpo see the package vignette for help.
#' @return \code{RaspSolved} object if \code{solve} is \code{TRUE}, else \code{RaspUnsolved}.
#' @seealso \code{\link{GurobiOpts}}, \code{\link{RaspReliableOpts}}, \code{\link{RaspUnreliableOpts}} \code{\link{RaspData}}, \code{\link{RaspResults}}, \code{\link{RaspUnsolved}}, \code{\link{RaspSolved}}.
rasp<-function(pus, species, ..., formulation='unreliable', solve=TRUE) {
	# set formulation
	match.arg(formulation, c('unreliable', 'reliable'))
	if (formulation=='unreliable') {
		opts<-'RaspUnreliableOpts'
	} else {
		opts<-'RaspReliableOpts'
	}
	# create unsolved object
	x<-RaspUnsolved(
		opts=do.call(opts, parseArgs(opts, object=NULL, ...)),
		data=do.call(make.RaspData, append(
			list(pus=pus, species=species),
			parseArgs('make.RaspData', object=NULL, ...)
		))
	)
	# solve object if specified
	if (solve) {
		x<-do.call('solve', append(
			append(
				list(a=x),
				parseArgs2(c('b', 'verbose'), ...)
			),
			parseArgs('GurobiOpts', object=NULL, ...)
		))
	}
	# return object
	return(x)
}
