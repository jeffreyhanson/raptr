 #' @include RcppExports.R raptr-internal.R misc.R
NULL

#' ManualOpts: An S4 class to represent parameters for manually specified
#' solutions
#'
#' This class is used to store parameters.
#'
#' @slot NumberSolutions `integer` number of solutions.
#'
#' @seealso [ManualOpts()].
#'
#' @name ManualOpts-class
#'
#' @rdname ManualOpts-class
#'
#' @exportClass ManualOpts
methods::setClass("ManualOpts",
  methods::representation(NumberSolutions = "integer"),
  prototype = list(NumberSolutions = 1L),
  contains = "SolverOpts",
  validity = function(object) {
    # NumberSolutions
    assertthat::assert_that(assertthat::is.count(object@NumberSolutions),
                            is.finite(object@NumberSolutions))
    return(TRUE)
  }
)

#' Create ManualOpts object
#'
#' This function creates a new ManualOpts object.
#'
#' @param NumberSolutions `integer` number of solutions to generate.
#'   Defaults to 1L.
#'
#' @return [ManualOpts()] object
#'
#' @seealso [ManualOpts-class].
#'
#' @examples
#' # create ManualOpts object
#' ManualOpts(NumberSolutions = 1L)
#'
#' @export
ManualOpts <- function(NumberSolutions = 1L) {
  mo <- methods::new("ManualOpts", NumberSolutions = NumberSolutions)
  methods::validObject(mo, test = FALSE)
  return(mo)
}

#' @method print ManualOpts
#'
#' @rdname print
#'
#' @export
print.ManualOpts <- function(x, ..., header=TRUE) {
  assertthat::assert_that(assertthat::is.flag(header))
  if (header) {
    message("ManualOpts object.")
  } else {
    message("  Method: manual")
  }
  message("  NumberSolutions: ", x@NumberSolutions)
}

#' @rdname show
#'
#' @name show
#'
#' @usage \S4method{show}{ManualOpts}(object)
#'
#' @aliases show,ManualOpts-method
setMethod("show", "ManualOpts", function(object) print.ManualOpts(object))

#' @rdname update
#'
#' @method update ManualOpts
#'
#' @export
update.ManualOpts <- function(object, NumberSolutions = NULL, ...) {
  if (!is.null(NumberSolutions))
    stop(paste0("Solver parameters cannot be updated for manually ",
                "specified solutions."))
  return(object)
}
