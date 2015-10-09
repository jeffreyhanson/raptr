#' @include RcppExports.R raspr-internal.R generics.R misc.R SimplePoints.R DemandPoints.R
NULL

#' AttributeSpace: An S4 class to represent an attribute space.
#'
#' This class is used to store planning unit points and demand points in an attribute space.
#'
#' @slot dp \code{SimplePoints} coordinates for planning unit in the space.
#' @slot pu \code{list} of \code{DemandPoints} for each species.
#' @seealso \code{\link{SimplePoints-class}}, \code{\link{DemandPoints-class}}.
#' @export
setClass("AttributeSpace",
	representation(
		pu='SimplePoints',
		dp='list'
	),
	validity=function(object) {
		# check that demand points and pu all have same dimensions
		if (!all(laply(object@dp, function(x) {return(ncol(x@points@coords)==ncol(object@pu@coords))})))
			stop('planning units and/or demand points have different numbers of columns in the coords slot')
		return(TRUE)
	}
)

#' Create new AttributeSpace object
#'
#' This function creates a new \code{AttributeSpace} object.
#'
#' @param pu \code{SimplePoints} coordinates for planning unit in the space.
#' @param dp \code{list} of \code{DemandPoints} for each species.
#' @seealso \code{\link{SimplePoints-class}}, \code{\link{DemandPoints-class}}.
#' @export
AttributeSpace<-function(pu, dp) {
	as<-new("AttributeSpace", pu=pu, dp=dp)
	validObject(as, test=FALSE)
	return(as)
}


