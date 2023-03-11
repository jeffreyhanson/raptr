#' @include RcppExports.R raptr-internal.R generics.R misc.R PlanningUnitPoints.R DemandPoints.R AttributeSpace.R
NULL

#' AttributeSpaces: An S4 class to represent a collection of attribute spaces
#' for different species.
#'
#' This class is used to store a collection of attribute spaces for different
#' species.
#'
#' @slot spaces `list` of [AttributeSpace()] objects for
#'   different species.
#'
#' @slot name `character` name to identify the attribute space.
#'
#' @seealso [AttributeSpace-class].
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
    assertthat::assert_that(
      all(sapply(object@spaces, inherits, what = "AttributeSpace")),
      msg = paste0(
        "argument to object@spaces must be ",
        "a list of AttributeSpace objects"
      )
    )
    # expect name is a non-NA string
    assertthat::assert_that(
      assertthat::is.string(object@name),
      assertthat::noNA(object@name)
    )
    return(TRUE)
  }
)

#' Create new AttributeSpaces object
#'
#' This function creates a new `AttributeSpaces` object.
#'
#' @param spaces `list` of [AttributeSpace()] objects for
#'   different species.
#'
#' @param name `character` name to identify the attribute space.
#'
#' @return A new `AttributeSpaces` object.
#'
#' @seealso [AttributeSpace-class].
#'
#' @examples
#' \dontrun{
#' space1 <- AttributeSpace(
#'   PlanningUnitPoints(
#'     matrix(rnorm(100), ncol = 2),
#'     seq_len(50)
#'   ),
#'   DemandPoints(
#'     matrix(rnorm(100), ncol = 2),
#'     runif(50)
#'   ),
#'   species = 1L
#' )
#'
#' space2 <- AttributeSpace(
#'   PlanningUnitPoints(
#'     matrix(rnorm(100), ncol = 2),
#'     seq_len(50)
#'   ),
#'   DemandPoints(
#'     matrix(rnorm(100), ncol = 2),
#'     runif(50)
#'   ),
#'   species = 2L
#' )
#'
#' spaces <- AttributeSpaces(list(space1, space2), "spaces")
#' }
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
