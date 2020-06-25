#' @include RcppExports.R raptr-internal.R misc.R
NULL

#' RapUnreliableOpts: An S4 class to represent parameters for the unreliable
#' RAP problem
#'
#' This class is used to store input parameters for the unreliable RAP problem
#' formulation.
#'
#' @slot BLM `numeric` boundary length modifier. Defaults to 0.
#'
#' @name RapUnreliableOpts-class
#'
#' @rdname RapUnreliableOpts-class
#'
#' @exportClass RapUnreliableOpts
methods::setClass("RapUnreliableOpts", contains = "RapOpts",
  validity = function(object) {
    assertthat::assert_that(
        assertthat::is.scalar(object@BLM),
        msg = "argument to BLM is not a scalar number")
    assertthat::assert_that(
        all(is.finite(object@BLM)),
        msg = "argument to BLM is is NA or non-finite value")
    return(TRUE)
  }
)

#' Create RapUnreliableOpts object
#'
#' This function creates a new RapUnreliableOpts object.
#'
#' @param BLM `numeric` boundary length modifier. Defaults to 0.
#'
#' @return [RapUnreliableOpts()] object
#'
#' @seealso [RapUnreliableOpts-class].
#'
#' @examples
#' # create RapUnreliableOpts using defaults
#' RapUnreliableOpts(BLM = 0)
#'
#' @export
RapUnreliableOpts <- function(BLM = 0) {
  ro <- methods::new("RapUnreliableOpts", BLM = BLM)
  methods::validObject(ro, test = FALSE)
  return(ro)
}

#' @method print RapUnreliableOpts
#'
#' @rdname print
#'
#' @export
print.RapUnreliableOpts <- function(x, ..., header = TRUE) {
  assertthat::assert_that(assertthat::is.flag(header))
  if (header)
    message("RapUnreliableOpts object.")
  message("  BLM: ", x@BLM)
  invisible()
}

#' @rdname show
#'
#' @usage \S4method{show}{RapUnreliableOpts}(object)
#'
#' @name show
#'
#' @aliases show,RapUnreliableOpts-method
methods::setMethod("show", "RapUnreliableOpts",
                   function(object) print.RapUnreliableOpts(object))

#' @rdname update
#'
#' @method update RapUnreliableOpts
#'
#' @export
update.RapUnreliableOpts <- function(object, BLM = NULL, ...) {
  if (!is.null(BLM))
    object@BLM <- BLM
  # check object for validity
  methods::validObject(object, test = FALSE)
  # return object
  return(object)
}
