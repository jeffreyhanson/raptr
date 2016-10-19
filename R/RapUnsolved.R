#' @include RcppExports.R raptr-internal.R misc.R generics.R RapReliableOpts.R RapUnreliableOpts.R RapData.R
NULL

#' RapUnsolved: An S4 class to represent RAP inputs
#'
#' This class is used to store RAP input data and input parameters.
#'
#' @slot opts \code{RapReliableOpts} or \code{RapUnreliableOpts} object used to store input parameters.
#' @slot data \code{RapData} object used to store input data.
#' @export
#' @seealso  \code{\link{RapReliableOpts-class}}, \code{\link{RapUnreliableOpts-class}}, \code{\link{RapData-class}}.
setClass("RapUnsolved",
	representation(
		opts="RapOpts",
		data="RapData"
	)
)

#' Create a new RapUnsolved object
#'
#' This function creates a \code{RapUnsolved} object using a \code{GurobiOpts}, a \code{RapReliableOpts} or \code{RapUnreliableOpts} object, and a \code{RapData} object.
#'
#' @param opts \code{RapReliableOpts} or \code{RapUnreliableOpts} object.
#' @param data \code{RapData} object.
#' @return \code{RapUnsolved} object.
#' @export
#' @seealso \code{\link{RapReliableOpts-class}}, \code{\link{RapUnreliableOpts-class}}, \code{\link{RapData-class}}.
#' @examples
#' # load data
#' \dontrun{
#' data(cs_pus, cs_spp)
#' # create inputs for RapUnsolved
#' ro <- RapUnreliableOpts()
#' rd <- make.RapData(cs_pus[1:10,], cs_spp, NULL, include.geographic.space=TRUE,n.demand.points=5L)
#' # create RapUnsolved object
#' ru <- RapUnsolved(ro, rd)
#' print(ru)
#' }
RapUnsolved<-function(opts, data) {
	return(new("RapUnsolved", opts=opts, data=data))
}

#' @method print RapUnsolved
#' @rdname print
#' @export
print.RapUnsolved <- function(x, ...) {
	cat("RapUnsolved object\n\n")
	cat("Parameters\n")
	print(x@opts, header=FALSE)
	cat("Data\n")
	print.RapData(x@data, header=FALSE)
}

#' @rdname show
#' @export
setMethod(
	'show',
	'RapUnsolved',
	function(object)
		print.RapUnsolved(object)
)

#' @rdname spp.plot
#' @method spp.plot RapUnsolved
#' @export
spp.plot.RapUnsolved<-function(
	x,
	species,
	prob.color.palette='YlGnBu',
	pu.color.palette=c('#4D4D4D', '#00FF00', '#FFFF00', '#FF0000'),
	basemap='none',
	alpha=ifelse(basemap=="none", 1, 0.7),
	grayscale=FALSE,
	main=NULL,
	force.reset=FALSE,
	...
) {
	# set title
	if (is.null(main)) {
		if ('name' %in% names(x@data@species) & is.numeric(species)) {
			main <- paste0(x@data@species$name[species])
		} else if (is.numeric(species)) {
			main <- paste0('Species ', species)
		} else {
			main <- paste0(species)
		}
	}
	spp.plot(x=x@data, species=species, prob.color.palette=prob.color.palette, pu.color.palette=pu.color.palette, basemap=basemap, alpha=alpha, grayscale=grayscale, main=main, force.reset=force.reset, ...)
}

#' @rdname space.plot
#' @method space.plot RapUnsolved
#' @export
space.plot.RapUnsolved<-function(
	x,
	species,
	space=1,
	pu.color.palette=c('#4D4D4D4D', '#00FF0080', '#FFFF0080', '#FF00004D'),
	main=NULL,
	...
) {
	# set title
	if (is.null(main)) {
		if ('name' %in% names(x@data@species) & is.numeric(species)) {
			main <- paste0(x@data@species$name[species], ' in space ', space)
		} else if (is.numeric(species)) {
			main <- paste0('Species ', species, ' in space ', space)
		} else {
			main <- paste0(species, ' in space ', space)
		}
	}
	space.plot.RapData(x@data, species, space, pu.color.palette, main, ...)
}

