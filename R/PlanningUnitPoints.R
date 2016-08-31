#' @include RcppExports.R raptr-internal.R generics.R misc.R
NULL

#' PlanningUnitPoints: An S4 class to represent planning units in an attribute space
#'
#' This class is used to planning units in an attribute space.
#'
#' @slot coords \code{matrix} coordinates for each point.
#' @slot ids \code{integer} planning unit ids.
#' @seealso \code{\link{AttributeSpace}}.
#' @export
setClass("PlanningUnitPoints",
	representation(
		coords='matrix',
		ids='integer'
	),
	validity=function(object) {
		# coords
		expect_true(all(is.finite(object@coords)), info='argument to coords contains NA or non-finite values')
		expect_true(nrow(object@coords)>0, info='argument to coords must contain at least one row')
		# ids
		expect_true(all(is.finite(object@ids)), info='argument to ids contains NA of non-finite values')
		expect_true(length(object@ids)>0, info='argument to ids must contain at least one element')
		# cross checks
		expect_equal(length(object@ids), nrow(object@coords), info='length of ids is not equal to number of rows in coordinates')
		return(TRUE)
	}
)

#' Create new PlanningUnitPoints object
#'
#' This function creates a new \code{PlanningUnitPoints} object.
#'
#' @param coords \code{matrix} coordinates for each point.
#' @param ids \code{integer} planning unit ids.
#' @seealso \code{\link{AttributeSpace-class}}.
#' @export
#' @examples
#' # create PlanningUnitPoints object
#' x <- PlanningUnitPoints(
#'	matrix(rnorm(150), ncol=1),
#'	seq_len(150)
#' )
PlanningUnitPoints<-function(coords, ids) {
	sp<-new("PlanningUnitPoints", coords=coords, ids=ids)
	validObject(sp, test=FALSE)
	return(sp)
}
 
