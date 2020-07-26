#' @include misc.R raptr-internal.R generics.R
NULL

#' RapResults: An S4 class to represent RAP results
#'
#' This class is used to store RAP results.
#'
#' @slot summary [base::data.frame()] with summary information on
#'  solutions.
#'
#' @slot selections [base::matrix()] with binary selections. The cell
#'   \eqn{x_{ij}} denotes if planning unit \eqn{j} is selected in the
#'   \eqn{i}'th solution.
#'
#' @slot amount.held [base::matrix()] with the amount held for each
#'   species in each solution.
#'
#' @slot space.held [base::matrix()] with the proportion of attribute
#'   space sampled for each species in each solution.
#'
#' @slot best `integer` with index of best solution.
#'
#' @slot logging.file `character` Gurobi log files.
#'
#' @slot .cache [base::environment()] used to store extra data.
#'
#' @details The `summary` table follows Marxan conventions
#' (<https://marxansolutions.org/>). The columns
#' are:
#' \describe{
#' \item{Run_Number}{The index of each solution in the object.}
#' \item{Status}{The status of the solution. The values in this column
#' correspond to outputs from the Gurobi software package (<http://www.gurobi.com/documentation/6.5/refman/optimization_status_codes.html>).}
#' \item{Score}{The objective function for the solution.}
#' \item{Cost}{Total cost associated with a solution.}
#' \item{Planning_Units}{Number of planning units selected in a solution.}
#' \item{Connectivity_Total}{The total amount of shared boundary length between
#' all planning units. All solutions in the same object should have equal
#' values for this column.}
#' \item{Connectivity_In}{The amount of shared boundary length among planning
#' units selected in the solution.}
#' \item{Connectivity_Edge}{The amount of exposed boundary length in the
#' solution.}
#' \item{Connectivity_Out}{The number of shared boundary length among planning
#' units not selected in the solution.}
#' \item{Connectivity_Fraction}{The ratio of shared boundary length in the
#' solution (`Connectivity_In`) to the total amount of boundary length
#' (`Connectivity_Edge`). This ratio is an indicator of solution quality.
#' Solutions with a lower ratio will have less planning units and will be more
#' efficient.}
#' }
#'
#' @seealso [RapResults()], [read.RapResults()].
#'
#' @name RapResults-class
#'
#' @rdname RapResults-class
#'
#' @exportClass RapResults
methods::setClass("RapResults",
  methods::representation(summary = "data.frame", selections = "matrix",
                          amount.held = "matrix", space.held = "matrix",
                          logging.file = "character", best = "integer",
                          .cache = "environment"),
  validity = function(object) {
    # summary
    assertthat::assert_that(
      all(unlist(sapply(object@summary, function(x) all(!is.na(x))))),
      msg = "summary contains NA or non-finite values")

    # selections
    assertthat::assert_that(
      all(object@selections %in% c(0, 1)),
      msg = "selections contains values that are not 0 or 1")

    # amount.held
    assertthat::assert_that(
      all(c(is.finite(object@amount.held))),
      msg = "amount.held contains NA or non-finite values")
    assertthat::assert_that(
      all(object@amount.held >= 0 & object@amount.held <= 1),
      msg = "amount.held contains values less than 0 or greater than 1")

    # space.held
    if (any(na.omit(object@space.held) < 0))
      warning(paste0("some species have space.held values less than 0, ",
                     "and thus are poorly represented"))
    if (any(na.omit(object@space.held) > 1))
      warning(paste0("some species have space.held values greater than 1, ",
                     "due to low precision in the calculations. Increase the ",
                     "failure.multiplier parameter to fix this"))

    # logging.file
    assertthat::assert_that(
      all(!is.na(object@logging.file)),
      msg = "logging.file contains NA values")

    # best
    assertthat::assert_that(
      length(object@best) == 1,
      msg = "best contains more than one value")
    assertthat::assert_that(
      all(is.finite(object@best)),
      msg = "best contains NA or non-finite values")
    assertthat::assert_that(
      object@best %in% seq_len(nrow(object@space.held)),
      msg = "best is not an index of a solution in object")
    # cross-slot dependencies
    assertthat::assert_that(
      nrow(object@summary) == length(object@logging.file),
      msg = "summary has different number of solutions to logging.file")
    assertthat::assert_that(
      nrow(object@summary) == nrow(object@selections),
      msg = paste0("object@summary has different number of solutions to ",
                   "object@selections"))
    assertthat::assert_that(
      nrow(object@summary) == nrow(object@amount.held),
      msg = paste0("summary has different number of solutions to ",
                   "amount.held"))
    assertthat::assert_that(
      nrow(object@summary) == nrow(object@space.held),
      msg = "summary has different number of solutions to space.held")
    return(TRUE)
  }
)

#' Create RapResults object
#'
#' This function creates a new [RapResults()] object.
#'
#' @param summary [base::data.frame()] with summary information on
#'   solutions. See details below for more information.
#'
#' @param selections [base::matrix()] with binary selections. The
#'   cell \eqn{x_{ij}} denotes if planning unit \eqn{j} is selected in the
#'   \eqn{i}'th solution.
#'
#' @param amount.held [base::matrix()] with the amount held for each
#'   species in each solution.
#'
#' @param space.held [base::matrix()] with the proportion of
#'   attribute space sampled for each species in each solution.
#'
#' @param logging.file `character` Gurobi log files.
#'
#' @param .cache [base::environment()] used to cache calculations.
#'
#' @details The `summary` table follows Marxan conventions (
#' <https://marxansolutions.org/>). The columns
#' are:
#' \describe{
#' \item{Run_Number}{The index of each solution in the object.}
#' \item{Status}{The status of the solution. The values in this column
#' correspond to outputs from the Gurobi software package (<http://www.gurobi.com/documentation/6.5/refman/optimization_status_codes.html>).}
#' \item{Score}{The objective function for the solution.}
#' \item{Cost}{Total cost associated with a solution.}
#' \item{Planning_Units}{Number of planning units selected in a solution.}
#' \item{Connectivity_Total}{The total amount of shared boundary length between
#' all planning units. All solutions in the same object should have equal
#' values for this column.}
#' \item{Connectivity_In}{The amount of shared boundary length among planning
#' units selected in the solution.}
#' \item{Connectivity_Edge}{The amount of exposed boundary length in the
#' solution.}
#' \item{Connectivity_Out}{The number of shared boundary length among planning
#' units not selected in the solution.}
#' \item{Connectivity_Fraction}{The ratio of shared boundary length in the
#' solution (`Connectivity_In`) to the total amount of boundary length
#' (`Connectivity_Edge`). This ratio is an indicator of solution quality.
#' Solutions with a lower ratio will have less planning units and will be more
#' efficient.}
#' }
#'
#' @note slot `best` is automatically determined based on data in
#'   `summary`.
#'
#' @return `RapResults` object
#'
#' @seealso [RapResults-class] [read.RapResults()].
#'
#' @export
RapResults <- function(summary, selections, amount.held, space.held,
                       logging.file, .cache = new.env()) {
  methods::new("RapResults", summary = summary, selections = selections,
               amount.held = amount.held, space.held = space.held,
               logging.file = logging.file, best = which.min(summary$Score),
               .cache = new.env())
}

#' @rdname selections
#'
#' @export
selections.RapResults <- function(x, y = 0) {
  if (is.null(y))
    return(x@selections)
  if (y == 0)
    return(x@selections[x@best, ])
  return(x@selections[y, ])
}


#' @rdname score
#'
#' @export
score.RapResults <- function(x, y = 0) {
  if (is.null(y))
    return(x@summary$Score)
  if (y == 0)
    return(x@summary$Score[x@best])
  return(x@summary$Score[y])
}

#' @method summary RapResults
#'
#' @export summary
summary.RapResults <- function(object) {
  return(object@summary)
}

#' @rdname logging.file
#'
#' @export
logging.file.RapResults <- function(x, y = 0) {
  if (is.null(y))
    return(x@logging.file)
  if (y == 0)
    return(x@logging.file[x@best])
  return(x@logging.file[y])
}

#' @method print RapResults
#'
#' @rdname print
#'
#' @export
print.RapResults <- function(x, ..., header = TRUE) {
  assertthat::assert_that(assertthat::is.flag(header))
  if (header)
    message("RapResults object.")
  message("  Number of solutions: ", nrow(x@summary))
  message(paste0("  Best solution score: ", score(x, 0),
                 " (", sum(selections(x, 0)), " planning units)"))
  invisible()
}

#' @rdname show
#'
#' @usage \S4method{show}{RapResults}(object)
#'
#' @name show
#'
#' @aliases show,RapResults-method
methods::setMethod("show", "RapResults",
                   function(object) print.RapResults(object))


#' @rdname is.cached
#'
#' @name is.cached
methods::setMethod("is.cached",
                   methods::signature(x = "RapResults", name = "character"),
                   function(x, name) !is.null(x@.cache[[name]]))

#' @rdname cache
#'
#' @name cache
methods::setMethod("cache",
                   methods::signature(x = "RapResults", name = "character",
                                      y = "ANY"),
                   function(x, name, y) x@.cache[[name]] <- y)

#' @rdname cache
#'
#' @name cache
methods::setMethod("cache",
                   methods::signature(x = "RapResults", name = "character",
                                      y = "missing"),
                   function(x, name, y) x@.cache[[name]])
