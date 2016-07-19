#' @include RcppExports.R raptr-internal.R generics.R misc.R PlanningUnitPoints.R DemandPoints.R
NULL

#' AttributeSpace: An S4 class to represent an attribute space.
#'
#' This class is used to store planning unit points and demand points for a single species in an attribute space.
#'
#' @slot planning.unit.points \code{PlanningUnitPoints} for planning unit in the space.
#' @slot demand.points \code{DemandPoints} object for the space.
#' @slot species \code{integer} species id to indicate which species the space is associated with.
#' @seealso \code{\link{DemandPoints-class}}, \code{\link{PlanningUnitPoints-class}}.
#' @export
setClass("AttributeSpace",
	representation(
		planning.unit.points='PlanningUnitPoints',
		demand.points='DemandPoints',
		species='integer'
	),
	validity=function(object) {
		# check that demand points and pu all have same number of columns
		expect_equal(ncol(object@demand.points@coords),ncol(object@planning.unit.points@coords), info='argument to pu must have same dimensionality as argument to demand.points')
		
		# check species is valid
		expect_false(all(is.na(object@species)), info='argument to species contains NA or non-finite value')
		expect_equal(length(object@species), 1, info='argument to species must contain a single integer')
		return(TRUE)
	}
)

#' Create new AttributeSpace object
#'
#' This function creates a new \code{AttributeSpace} object.
#'
#' @param planning.unit.points \code{PlanningUnitPoints} for planning unit in the space.
#' @param demand.points \code{DemandPoints} object for the space.
#' @param species \code{integer} species id to indicate which species the space is associated with.
#' @seealso \code{\link{DemandPoints-class}}, \code{\link{PlanningUnitPoints-class}}.
#' @export
#' @examples
#' space <- AttributeSpace(
#'	PlanningUnitPoints(
#'		matrix(rnorm(100), ncol=2),
#'		seq_len(50)
#'	),
#'	DemandPoints(
#'		matrix(rnorm(100), ncol=2),
#'		runif(50)
#'	),
#'	species=1L
#' )
AttributeSpace<-function(planning.unit.points, demand.points, species) {
	as<-new("AttributeSpace", planning.unit.points=planning.unit.points, demand.points=demand.points, species=species)
	validObject(as, test=FALSE)
	return(as)
}

#' @method print AttributeSpace
#' @rdname print
#' @export
print.AttributeSpace<-function(x, ..., header=TRUE) {
	if (header)
		cat("AttributeSpace object.\n")
	cat(ifelse(header, "  ", "    "),"Species: ",x@species,"\n", sep="")
	cat(ifelse(header, "  ", "      "), "Number of planning unit points: ",nrow(x@planning.unit.points@coords),"\n", sep="")
	cat(ifelse(header, "  ", "      "), "Number of demand points: ",nrow(x@demand.points@coords),"\n", sep="")
}

