#' @include RcppExports.R rapr-internal.R misc.R generics.R RapReliableOpts.R RapUnreliableOpts.R GurobiOpts.R RapData.R RapUnsolved.R RapResults.R RapSolved.R
NULL

#' Generate prioritisations using RASP
#'
#' This is a general function to create Rap objects from scratch and solve them to generate solutions.
#'
#' @param pus \code{SpatialPolyogns} object representing planning units.
#' @param species \code{Raster} object with species distribution data.
#' @param spaces \code{list} of \code{RasterLayer} or \code{RasterStack} objects. Each elements denotes the spatial distribution for each space. Defaults to \code{NULL}.
#' @param ... arguments are passed to \code{GurobiOpts}, \code{RapData}, and \code{RapReliableOpts} or \code{RapUnreliableOpts} functions.
#' @param formulation \code{character}  to indicate if 'unreliable' or 'reliable' formulation should be used to generate prioritisations. Defaults to 'unreliable'.
#' @param solve \code{logical} should solutions be generated?
#' @export
#' @note Type \code{vignette('rapr')} to see the package vignette for help.
#' @return \code{RapSolved} object if \code{solve} is \code{TRUE}, else \code{RapUnsolved}.
#' @seealso \code{\link{GurobiOpts}}, \code{\link{RapReliableOpts}}, \code{\link{RapUnreliableOpts}} \code{\link{RapData}}, \code{\link{RapResults}}, \code{\link{RapUnsolved}}, \code{\link{RapSolved}}.
rap<-function(pus, species, spaces=NULL, ..., formulation='unreliable', solve=TRUE) {
	# set formulation
	match.arg(formulation, c('unreliable', 'reliable'))
	if (formulation=='unreliable') {
		opts<-'RapUnreliableOpts'
	} else {
		opts<-'RapReliableOpts'
	}
	# create unsolved object
	x<-RapUnsolved(
		opts=do.call(opts, parseArgs(opts, object=NULL, ...)),
		data=do.call(make.RapData, append(
			list(pus=pus, species=species, spaces=spaces),
			parseArgs('make.RapData', object=NULL, ...)
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
