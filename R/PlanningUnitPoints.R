#' @include RcppExports.R raptr-internal.R generics.R misc.R
NULL

#' PlanningUnitPoints: An S4 class to represent planning units in an attribute
#' space
#'
#' This class is used to planning units in an attribute space.
#'
#' @slot coords \code{\link[base]{matrix}} coordinates for each point.
#'
#' @slot ids \code{integer} planning unit ids.
#'
#' @seealso \code{\link{AttributeSpace}}.
#'
#' @name PlanningUnitPoints-class
#'
#' @rdname PlanningUnitPoints-class
#'
#' @exportClass PlanningUnitPoints
methods::setClass("PlanningUnitPoints",
  methods::representation(coords = "matrix", ids = "integer"),
  validity = function(object) {
    # coords
    assertthat::assert_that(all(is.finite(object@coords)),
                            msg = paste0("argument to coords contains NA or ",
                                         "non-finite values"))
    assertthat::assert_that(nrow(object@coords) > 0,
                            msg = paste0("argument to coords must contain at ",
                                         "least one row"))
    # ids
    assertthat::assert_that(all(is.finite(object@ids)),
                            msg = paste0("argument to ids contains NA of ",
                                         "non-finite values"))
    assertthat::assert_that(length(object@ids) > 0,
                            msg = paste0("argument to ids must contain at ",
                                         "least one element"))
    # cross checks
    assertthat::assert_that(length(object@ids) == nrow(object@coords),
                            msg = paste0("length of ids is not equal to ",
                                         "number of rows in coordinates"))
    return(TRUE)
  }
)

#' Create new PlanningUnitPoints object
#'
#' This function creates a new \code{PlanningUnitPoints} object.
#'
#' @param coords \code{\link[base]{matrix}} coordinates for each point.
#'
#' @param ids \code{integer} planning unit ids.
#'
#' @seealso \code{\link{AttributeSpace-class}}.
#'
#' @examples
#' # create PlanningUnitPoints object
#' x <- PlanningUnitPoints(matrix(rnorm(150), ncol = 1), seq_len(150))
#'
#' # print object
#' print(x)
#'
#' @export
PlanningUnitPoints <- function(coords, ids) {
  sp <- methods::new("PlanningUnitPoints", coords = coords, ids = ids)
  methods::validObject(sp, test = FALSE)
  return(sp)
}
