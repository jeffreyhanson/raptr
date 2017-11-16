#' @include RcppExports.R raptr-internal.R generics.R misc.R PlanningUnitPoints.R DemandPoints.R AttributeSpace.R
NULL

#' AttributeSpaces: An S4 class to represent a collection of attribute spaces
#' for different species.
#'
#' This class is used to store a collection of attribute spaces for different
#' species.
#'
#' @slot spaces \code{list} of \code{\link{AttributeSpace}} objects for
#'   different species.
#'
#' @slot name \code{character} name to identify the attribute space.
#'
#' @seealso \code{\link{AttributeSpace-class}}.
#'
#' @name AttributeSpaces-class
#'
#' @rdname AttributeSpaces-class
#'
#' @exportClass AttributeSpace
methods::setClass("AttributeSpaces",
  methods::representation(spaces = "list", name = "character"),
  validity = function(object) {
    # check that all elements in the list are AttributeSpace objects
    assertthat::assert_that(all(sapply(object@spaces, inherits,
                                       what = "AttributeSpace")),
                            msg = paste0("argument to object@spaces must be ",
                                         "a list of AttributeSpace objects"))
    # expect name is not NA
    assertthat::assert_that(!is.na(object@name),
                            msg = "argument to name must not be NA")
    assertthat::assert_that(isTRUE(length(object@name) == 1),
                            msg = paste0("argument to name must have a single ",
                                         "element"))
    return(TRUE)
  }
)

#' Create new AttributeSpaces object
#'
#' This function creates a new \code{AttributeSpaces} object.
#'
#' @param spaces \code{list} of \code{\link{AttributeSpace}} objects for
#'   different species.
#'
#' @param name \code{character} name to identify the attribute space.
#'
#' @seealso \code{\link{AttributeSpace-class}}.
#'
#' @examples
#' space1 <- AttributeSpace(
#'   PlanningUnitPoints(
#'     matrix(rnorm(100), ncol = 2),
#'     seq_len(50)),
#'   DemandPoints(
#'     matrix(rnorm(100), ncol = 2),
#'     runif(50)),
#'   species = 1L)
#'
#' space2 <- AttributeSpace(
#'   PlanningUnitPoints(
#'     matrix(rnorm(100), ncol = 2),
#'     seq_len(50)),
#'   DemandPoints(
#'     matrix(rnorm(100), ncol = 2),
#'     runif(50)),
#'   species = 2L)
#'
#' spaces <- AttributeSpaces(list(space1, space2), "spaces")
#'
#' @export
AttributeSpaces <- function(spaces, name) {
  asp <- methods::new("AttributeSpaces", spaces = spaces,
                      name = as.character(name))
  methods::validObject(asp, test = FALSE)
  return(asp)
}

#' @method print AttributeSpaces
#'
#' @rdname print
#'
#' @export
print.AttributeSpaces <- function(x, ..., header = TRUE) {
  if (header)
    message("AttributeSpaces object.")
  message("  Name: ", x@name)
  message("  Number of species: ", length(x@spaces))
  sapply(x@spaces, print, header = FALSE)
  return(invisible())
}
