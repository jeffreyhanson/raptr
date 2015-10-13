#' @include RcppExports.R raspr-internal.R misc.R generics.R RaspOpts.R GurobiOpts.R RaspData.R RaspUnsolved.R RaspResults.R RaspSolved.R
NULL

#' Generate prioritisations using RASP
#'
#' This is a general function to create Rasp objects from scratch and solve them to generate solutions.
#'
#' @param pus \code{SpatialPolyogns} object representing planning units.
#' @param species \code{Raster} object with species distribution data.
#' @param ... arguments are passed to GurobiOpts, RaspData, and RaspOpts functions.
#' @param solve \code{logical} should solutions be generated?
#' @export
#' @note Type \code{vignette('raspr')} tpo see the package vignette for help.
#' @return \code{RaspSolved} object if \code{solve} is \code{TRUE}, else \code{RaspUnsolved}.
#' @seealso \code{\link{GurobiOpts}}, \code{\link{RaspOpts}}, \code{\link{RaspData}}, \code{\link{RaspResults}}, \code{\link{RaspUnsolved}}, \code{\link{RaspSolved}}.
rasp<-function(pus, species, ..., solve=TRUE) {
	args<-list(...)
	x<-RaspUnsolved(
		do.call(GurobiOpts, args[which(names(args) %in% slotNames('GurobiOpts'))]),
		do.call(RaspOpts, args[which(names(args) %in% slotNames('RaspOpts'))]),
		make.RaspData(pus=pus, species=species, ...)
	)
	if (solve)
		x<-solve(x)
	return(x)
}
	