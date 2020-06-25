#' @include RcppExports.R raptr-internal.R misc.R
NULL

#' RapReliableOpts: An S4 class to represent input parameters for the reliable
#' formulation of RAP.
#'
#' This class is used to store input parameters for the reliable formulation of
#' RAP.
#'
#' @slot BLM `numeric` boundary length modifier. Defaults to 0.
#'
#' @slot failure.multiplier `numeric` multiplier for failure planning
#'   unit. Defaults to 1.1.
#'
#' @slot max.r.level `numeric` maximum R failure level for approximation.
#'   Defaults to 5L.
#'
#' @seealso [RapReliableOpts()].
#'
#' @name RapReliableOpts-class
#'
#' @rdname RapReliableOpts-class
#'
#' @exportClass RapReliableOpts
methods::setClass("RapReliableOpts",
  methods::representation(failure.multiplier = "numeric",
                          max.r.level = "integer"),
  prototype = list(failure.multiplier = 1.1, max.r.level = 5L),
  contains = "RapOpts",
  validity = function(object) {
    # failure.multiplier
    assertthat::assert_that(
      assertthat::is.scalar(object@failure.multiplier),
      msg = "argument to failure.multiplier is not a scalar")
    assertthat::assert_that(
      is.finite(object@failure.multiplier),
      msg = "argument to failure.multiplier is NA or non-finite value")
    # BLM
    assertthat::assert_that(
      assertthat::is.scalar(object@BLM),
      msg = "argument to BLM is not a scalar")
    assertthat::assert_that(
      is.finite(object@BLM),
      msg = "argument to BLM is NA or non-finite value")
    # max.r.level
    assertthat::assert_that(
      is.integer(object@max.r.level), length(object@max.r.level) == 1,
      msg = "argument to max.r.level is not a scalar integer")
    assertthat::assert_that(
      is.finite(object@max.r.level),
      msg = "argument to max.r.level is NA or non-finite value")
    return(TRUE)
  }
)

#' Create RapReliableOpts object
#'
#' This function creates a new RapReliableOpts object.
#'
#' @param BLM `numeric` boundary length modifier. Defaults to 0.
#'
#' @param failure.multiplier `numeric` multiplier for failure planning
#'   unit. Defaults to 1.1.
#'
#' @param max.r.level `numeric` maximum R failure level for approximation.
#'   Defaults to 5L.
#'
#' @return `RapReliableOpts` object
#'
#' @seealso [RapReliableOpts-class].
#'
#' @examples
#' # create RapReliableOpts using defaults
#' RapReliableOpts(BLM = 0, failure.multiplier = 1.1, max.r.level = 5L)
#'
#' @export
RapReliableOpts <- function(BLM = 0, failure.multiplier = 1.1,
                            max.r.level = 5L) {
  ro <- methods::new("RapReliableOpts", BLM = BLM,
                     failure.multiplier = failure.multiplier,
                     max.r.level = max.r.level)
  methods::validObject(ro, test=  FALSE)
  return(ro)
}

#' @method print RapReliableOpts
#'
#' @rdname print
#'
#' @export
print.RapReliableOpts <- function(x, ..., header=TRUE) {
  assertthat::assert_that(assertthat::is.flag(header))
  if (header)
    message("RapReliableOpts object.")
  message("  BLM: ", x@BLM)
  message("  failure.multiplier: ", x@failure.multiplier)
  message("  max.r.level: ", x@max.r.level)
}

#' @rdname show
#'
#' @usage \S4method{show}{RapReliableOpts}(object)
#'
#' @name show
#'
#' @aliases show,RapReliableOpts-method
methods::setMethod("show", "RapReliableOpts",
                   function(object) print.RapReliableOpts(object))

#' @rdname update
#'
#' @method update RapReliableOpts
#'
#' @export
update.RapReliableOpts <- function(object, BLM = NULL,
                                   failure.multiplier = NULL,
                                   max.r.level = NULL, ...) {
  # update params
  if (!is.null(BLM))
    object@BLM <- BLM
  if (!is.null(failure.multiplier))
    object@failure.multiplier <- failure.multiplier
  if (!is.null(max.r.level))
    object@max.r.level <- max.r.level
  # check object for validity
  methods::validObject(object, test = FALSE)
  # return object
  return(object)
}
