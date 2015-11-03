#' @include RcppExports.R rapr-internal.R misc.R generics.R RapReliableOpts.R RapUnreliableOpts.R RapData.R
NULL

#' RapUnsolved: An S4 class to represent RASP inputs
#'
#' This class is used to store RASP input data and input parameters.
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
#' data(cs_pus, cs_spp)
#' # create inputs for RapUnsolved
#' ro <- RapUnreliableOpts()
#' rd <- make.RapData(cs_pus[1:10,], cs_spp, NULL, include.geographic.space=TRUE,n.demand.points=5L)
#' # create RapUnsolved object
#' ru <- RapUnsolved(ro, rd)
#' print(ru)
RapUnsolved<-function(opts, data) {
	return(new("RapUnsolved", opts=opts, data=data))
}

#' @method print RapUnsolved
#' @rdname print
#' @export
print.RapUnsolved=function(x, ...) {
	cat("RapUnsolved object\n\n")
	cat("Parameters\n")
	print(x@opts, header=FALSE)
	cat("Data\n")
	print.RapData(x@data, header=FALSE)
}

#' @describeIn show
#' @export
setMethod(
	'show',
	'RapUnsolved',
	function(object)
		print.RapUnsolved(object)
)

#' @rdname spp.subset
#' @method spp.subset RapUnsolved
#' @export
spp.subset.RapUnsolved<-function(x, species) {
	return(
		RapUnsolved(
			opts=x@opts,
			data=spp.subset(x@data, species)
			)
	 )
}

#' @rdname pu.subset
#' @method pu.subset RapUnsolved
#' @export
pu.subset.RapUnsolved<-function(x, pu) {
	return(
		RapUnsolved(
			opts=x@opts,
			data=pu.subset(x@data, pu)
			)
	 )
}

#' @rdname dp.subset
#' @method dp.subset RapUnsolved
#' @export
dp.subset.RapUnsolved<-function(x, space, species, points) {
	return(
		RapUnsolved(
			opts=x@opts,
			data=dp.subset(x@data, space, species, points)
			)
	 )
}



#' @rdname spp.plot
#' @method spp.plot RapUnsolved
#' @export
spp.plot.RapUnsolved<-function(x, species, prob.color.palette='YlGnBu', basemap='none', alpha=ifelse(basemap=="none", 1, 0.7), grayscale=FALSE, force.reset=FALSE, ...) {
	spp.plot(x@data, species, prob.color.palette, basemap, alpha, grayscale, force.reset)
}

#' @rdname space.plot
#' @method space.plot RapUnsolved
#' @export
space.plot.RapUnsolved<-function(
	x,
	species,
	space=1,
	pu.color.palette='RdYlGn',
	locked.in.color="#000000FF",
	locked.out.color="#D7D7D7FF",
	...
) {
	space.plot.RapData(x@data, species, space, pu.color.palette, locked.in.color, locked.out.color, ...)
}
