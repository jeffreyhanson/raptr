#' @include RcppExports.R raspr-internal.R misc.R generics.R RaspReliableOpts.R RaspUnreliableOpts.R RaspData.R
NULL

#' RaspUnsolved: An S4 class to represent RASP inputs
#'
#' This class is used to store RASP input data and input parameters.
#'
#' @slot opts \code{RaspReliableOpts} or \code{RaspUnreliableOpts} object used to store input parameters.
#' @slot data \code{RaspData} object used to store input data.
#' @export
#' @seealso  \code{\link{RaspReliableOpts-class}}, \code{\link{RaspUnreliableOpts-class}}, \code{\link{RaspData-class}}.
setClass("RaspUnsolved",
	representation(
		opts="RaspOpts",
		data="RaspData"
	)
)

#' Create a new RaspUnsolved object
#'
#' This function creates a \code{RaspUnsolved} object using a \code{GurobiOpts}, a \code{RaspReliableOpts} or \code{RaspUnreliableOpts} object, and a \code{RaspData} object.
#'
#' @param opts \code{RaspReliableOpts} or \code{RaspUnreliableOpts} object.
#' @param data \code{RaspData} object.
#' @return \code{RaspUnsolved} object.
#' @export
#' @seealso \code{\link{RaspReliableOpts-class}}, \code{\link{RaspUnreliableOpts-class}}, \code{\link{RaspData-class}}.
#' @examples
#' # load data
#' data(cs_pus, cs_spp)
#' # create inputs for RaspUnsolved
#' ro <- RaspUnreliableOpts()
#' rd <- make.RaspData(cs_pus[1:10,], cs_spp, NULL, include.geographic.space=TRUE,n.demand.points=5L)
#' # create RaspUnsolved object
#' ru <- RaspUnsolved(ro, rd)
#' print(ru)
RaspUnsolved<-function(opts, data) {
	return(new("RaspUnsolved", opts=opts, data=data))
}

#' @method print RaspUnsolved
#' @rdname print
#' @export
print.RaspUnsolved=function(x, ...) {
	cat("Parameters\n")
	print(x@opts, header=FALSE)
	cat("Data\n")
	print.RaspData(x@data, header=FALSE)
}

#' @describeIn show
#' @export
setMethod(
	'show',
	'RaspUnsolved',
	function(object)
		print.RaspUnsolved(object)
)

#' @rdname spp.subset
#' @method spp.subset RaspUnsolved
#' @export
spp.subset.RaspUnsolved<-function(x, species) {
	return(
		RaspUnsolved(
			opts=x@opts,
			data=spp.subset(x@data, species)
			)
	 )
}

#' @rdname pu.subset
#' @method pu.subset RaspUnsolved
#' @export
pu.subset.RaspUnsolved<-function(x, pu) {
	return(
		RaspUnsolved(
			opts=x@opts,
			data=pu.subset(x@data, pu)
			)
	 )
}

#' @rdname spp.plot
#' @method spp.plot RaspUnsolved
#' @export
spp.plot.RaspUnsolved<-function(x, species, prob.color.palette='YlGnBu', basemap='none', alpha=ifelse(basemap=="none", 1, 0.7), grayscale=FALSE, force.reset=FALSE) {
	spp.plot(x@data, species, prob.color.palette, basemap, alpha, grayscale, force.reset)
}

#' @rdname space.plot
#' @method space.plot RaspUnsolved
#' @export
space.plot.RaspUnsolved<-function(
	x,
	y,
	space=1,
	pu.color.palette='RdYlGn',
	locked.in.color="#000000FF",
	locked.out.color="#D7D7D7FF"
) {
	space.plot.RaspData(x@data, y, space, pu.color.palette, locked.in.color, locked.out.color)
}
