#' @include RcppExports.R raptr-internal.R generics.R misc.R PlanningUnitPoints.R DemandPoints.R
NULL

#' AttributeSpace: An S4 class to represent an attribute space.
#'
#' This class is used to store planning unit points and demand points for a
#' single species in an attribute space.
#'
#' @slot planning.unit.points [PlanningUnitPoints()] object for planning
#'   unit in the space.
#'
#' @slot demand.points [DemandPoints()] object for the space.
#'
#' @slot species `integer` species id to indicate which species the space
#'   is associated with.
#'
#' @seealso [DemandPoints-class],
#'   [PlanningUnitPoints-class].
#'
#' @name AttributeSpace-class
#'
#' @rdname AttributeSpace-class
#'
#' @exportClass AttributeSpace
methods::setClass("AttributeSpace",
  methods::representation(
    planning.unit.points = "PlanningUnitPoints",
    demand.points = "DemandPoints",
    species = "integer"
  ),
  validity = function(object) {
    assertthat::assert_that(
      identical(
        ncol(object@demand.points@coords),
        ncol(object@planning.unit.points@coords)
      ),
      msg = paste0(
        "argument to pu must have same dimensionality as ",
        "argument to demand.points"
      )
    )
    assertthat::assert_that(
      all(is.finite(object@species)),
      msg = "argument to species contains NA or non-finite value"
    )
    assertthat::assert_that(
      assertthat::is.scalar(object@species),
      msg = "argument to species must contain a single integer"
    )
    return(TRUE)
  }
)

#' Create new AttributeSpace object
#'
#' This function creates a new `AttributeSpace` object.
#'
#' @param planning.unit.points [PlanningUnitPoints()] for planning
#'   unit in the space.
#'
#' @param demand.points [DemandPoints()] object for the space.
#'
#' @param species `integer` species identifier to indicate which species the
#'   space is associated with.
#'
#' @return A new `AttributeSpace` object.
#'
#' @seealso [DemandPoints-class], [PlanningUnitPoints-class].
#'
#' @examples
#' \dontrun{
#' space <- AttributeSpace(
#'  PlanningUnitPoints(
#'    matrix(rnorm(100), ncol = 2),
#'    seq_len(50)
#'  ),
#'  DemandPoints(
#'    matrix(rnorm(100), ncol = 2),
#'    runif(50)
#'   ),
#'  species = 1L
#' )
#' }
#' @export
AttributeSpace <- function(planning.unit.points, demand.points, species) {
  asp <- methods::new(
    "AttributeSpace",
    planning.unit.points = planning.unit.points,
    demand.points = demand.points,
    species = species
  )
  methods::validObject(asp, test = FALSE)
  return(asp)
}

#' @method print AttributeSpace
#'
#' @rdname print
#'
#' @export
print.AttributeSpace <- function(x, ..., header = TRUE) {
  assertthat::assert_that(assertthat::is.flag(header))
  if (header)
    message("AttributeSpace object.")
  message(ifelse(header, "  ", "    "), "Species: ", x@species)
  message(ifelse(header, "  ", "      "),
          "Number of planning unit points: ",
          nrow(x@planning.unit.points@coords))
  message(ifelse(header, "  ", "      "),
          "Number of demand points: ",
          nrow(x@demand.points@coords))
  return(invisible())
}
