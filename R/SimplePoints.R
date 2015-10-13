#' @include RcppExports.R raspr-internal.R generics.R misc.R
NULL

#' SimplePoints: An S4 class to represent points
#'
#' This class is used to store \code{d}-dimensional point coordinates.
#'
#' @slot coords \code{matrix} coordinates for each point.
#' @seealso \code{\link{DemandPoints}}, \code{\link{AttributeSpace}}.
#' @export
setClass("SimplePoints",
	representation(
		coords='matrix'
	),
	validity=function(object) {
		# coords
		if (!is.matrix(object@coords))
			stop('argument to coords must be matrix')
		if (!all(is.finite(object@coords)))
			stop('argument to coords contains NA or non-finite values')
	}
)

#' Create new SimplePoints object
#'
#' This function creates a new \code{SimplePoints} object.
#'
#' @param coords \code{matrix} coordinates for each point.
#' @seealso \code{\link{DemandPoints-class}}, \code{\link{AttributeSpace-class}}.
#' @export
SimplePoints<-function(coords) {
	sp<-new("SimplePoints", coords=coords)
	validObject(sp, test=FALSE)
	return(sp)
}


