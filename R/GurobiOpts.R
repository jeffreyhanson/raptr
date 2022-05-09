 #' @include RcppExports.R raptr-internal.R misc.R
NULL

#' GurobiOpts: An S4 class to represent Gurobi parameters
#'
#' This class is used to store Gurobi input parameters.
#'
#' @slot Threads `integer` number of cores to use for processing. Defaults
#'   to 1L.
#'
#' @slot MIPGap `numeric` MIP gap specifying minimum solution quality.
#'   Defaults to 0.1.
#'
#' @slot Method `integer` Algorithm to use for solving model. Defaults to
#'   0L.
#'
#' @slot Presolve `integer` code for level of computation in presolve.
#'  Defaults to 2.
#'
#' @slot TimeLimit `integer` number of seconds to allow for solving.
#'   Defaults to NA_integer_, and so a time limit is not imposed.
#'
#' @slot NumberSolutions `integer` number of solutions to generate.
#'   Defaults to 1L.
#'
#' @slot MultipleSolutionsMethod `integer` name of method to obtain
#'   multiple solutions (used when `NumberSolutions` is greater than one).
#'   Available options are `"benders.cuts"`, `"solution.pool.0"`,
#'   `"solution.pool.1"`, and `"solution.pool.2"`. The
#'   `"benders.cuts"` method produces a set of distinct solutions that
#'   are all within the optimality gap. The `"solution.pool.0"`
#'   method returns all solutions identified whilst trying to find
#'   a solution that is within the specified optimality gap. The
#'   `"solution.pool.1"` method finds one solution within the optimality
#'   gap and a number of additional solutions that are of any level of quality
#'   (such that the total number of solutions is equal to
#'   `number_solutions`). The `"solution.pool.2"` finds a
#'   specified number of solutions that are nearest to optimality. The
#'   search pool methods correspond to the parameters used by the Gurobi
#'   software suite (see <https://www.gurobi.com/documentation/8.0/refman/poolsearchmode.html#parameter:PoolSearchMode>).
#'   Defaults to `"benders.cuts"`.
#'
#' @seealso [GurobiOpts()].
#'
#' @name GurobiOpts-class
#'
#' @rdname GurobiOpts-class
#'
#' @exportClass GurobiOpts
methods::setClass("GurobiOpts",
  methods::representation(Threads = "integer", MIPGap = "numeric",
                          Method = "integer", Presolve = "integer",
                          TimeLimit = "integer", NumberSolutions = "integer",
                          MultipleSolutionsMethod = "character"),
  prototype = list(Threads = 1L, MIPGap = 0.1, Method = 0L, Presolve = 2L,
                   TimeLimit = NA_integer_, NumberSolutions = 1L,
                   MultipleSolutionsMethod = "benders.cuts"),
  contains = "SolverOpts",
  validity = function(object) {
    # NumberSolutions
    assertthat::assert_that(assertthat::is.count(object@NumberSolutions),
                            is.finite(object@NumberSolutions))
    # TimeLimit
    assertthat::assert_that(is.integer(object@NumberSolutions))
    # Threads
    assertthat::assert_that(assertthat::is.count(object@Threads),
                            is.finite(object@Threads),
                            object@Threads <= parallel::detectCores(logical =
                                                                      TRUE))
    # MultipleSolutionsMethod
    assertthat::assert_that(
      assertthat::is.string(object@MultipleSolutionsMethod),
      object@MultipleSolutionsMethod %in%
      c("benders.cuts", "solution.pool.0", "solution.pool.1",
        "solution.pool.2"))
    # Presolve
    assertthat::assert_that(assertthat::is.scalar(object@Presolve),
                            is.finite(object@Presolve),
                            is.integer(object@Presolve),
                            object@Presolve <= 2, object@Presolve >= -1)
    # Method
    assertthat::assert_that(assertthat::is.scalar(object@Method),
                            is.finite(object@Method),
                            is.integer(object@Method),
                            object@Method <= 4, object@Method >= -1)
    # MIPGap
    assertthat::assert_that(assertthat::is.scalar(object@MIPGap),
                            is.finite(object@MIPGap), object@MIPGap >= 0)
    return(TRUE)
  }
)

#' Create GurobiOpts object
#'
#' This function creates a new GurobiOpts object.
#'
#' @param Threads `integer` number of cores to use for processing.
#'   Defaults to 1L.
#'
#' @param MIPGap `numeric` MIP gap specifying minimum solution quality.
#'   Defaults to 0.1.
#'
#' @param Method `integer` Algorithm to use for solving model. Defaults to
#'   0L.
#'
#' @param Presolve `integer` code for level of computation in presolve
#'   (lp_solve parameter). Defaults to 2.
#'
#' @param TimeLimit `integer` number of seconds to allow for solving.
#'   Defaults to `NA_integer_`, and so a time limit is not imposed.
#'
#' @param NumberSolutions `integer` number of solutions to generate.
#'   Defaults to 1L.
#'
#' @param MultipleSolutionsMethod `integer` name of method to obtain
#'   multiple solutions (used when `NumberSolutions` is greater than one).
#'   Available options are `"benders.cuts"`, `"solution.pool.0"`,
#'   `"solution.pool.1"`, and `"solution.pool.2"`. The
#'   `"benders.cuts"` method produces a set of distinct solutions that
#'   are all within the optimality gap. The `"solution.pool.0"`
#'   method returns all solutions identified whilst trying to find
#'   a solution that is within the specified optimality gap. The
#'   `"solution.pool.1"` method finds one solution within the optimality
#'   gap and a number of additional solutions that are of any level of quality
#'   (such that the total number of solutions is equal to
#'   `number_solutions`). The `"solution.pool.2"` finds a
#'   specified number of solutions that are nearest to optimality. The
#'   search pool methods correspond to the parameters used by the Gurobi
#'   software suite (see <https://www.gurobi.com/documentation/8.0/refman/poolsearchmode.html#parameter:PoolSearchMode>).
#'   Defaults to `"benders.cuts"`.
#'
#' @return `GurobiOpts` object
#'
#' @seealso [GurobiOpts-class].
#'
#' @examples
#' # create GurobiOpts object using default parameters
#' GurobiOpts(Threads = 1L, MIPGap = 0.1, Method = 0L, Presolve=2L,
#'            TimeLimit = NA_integer_, NumberSolutions = 1L)
#'
#' @export
GurobiOpts <- function(Threads = 1L, MIPGap = 0.1, Method = 0L, Presolve = 2L,
                       TimeLimit = NA_integer_, NumberSolutions = 1L,
                       MultipleSolutionsMethod = c("benders.cuts",
                                                   "solution.pool.0",
                                                   "solution.pool.1",
                                                   "solution.pool.2")[1]) {
  go <- methods::new("GurobiOpts", Threads = Threads, MIPGap = MIPGap,
                     Method = Method, Presolve = Presolve,
                     TimeLimit = TimeLimit, NumberSolutions = NumberSolutions,
                     MultipleSolutionsMethod = MultipleSolutionsMethod)
  methods::validObject(go, test = FALSE)
  return(go)
}

#' @method print GurobiOpts
#'
#' @rdname print
#'
#' @export
print.GurobiOpts <- function(x, ..., header=TRUE) {
  assertthat::assert_that(assertthat::is.flag(header))
  if (header) {
    message("GurobiOpts object.")
  } else {
    message("  Method: Gurobi")
  }
  message("  Threads: ", x@Threads)
  message("  MIPGap: ", x@MIPGap)
  message("  Method: ", x@Method)
  message("  Presolve: ", x@Presolve)
  message("  TimeLimit: ", x@TimeLimit)
  message("  NumberSolutions: ", x@NumberSolutions)
  message("  MultipleSolutionsMethod: ", x@MultipleSolutionsMethod)
}

#' @rdname show
#'
#' @usage \S4method{show}{GurobiOpts}(object)
#'
#' @name show
#'
#' @aliases show,GurobiOpts-method
methods::setMethod("show", "GurobiOpts",
                   function(object) print.GurobiOpts(object))

#' @method as.list GurobiOpts
#'
#' @rdname as.list
#'
#' @export
as.list.GurobiOpts <- function(x, ...) {
  y <- list(Threads = x@Threads, MIPGap = x@MIPGap, Presolve = x@Presolve,
            Method = x@Method)
  if (is.finite(x@TimeLimit))
    y$TimeLimit <- x@TimeLimit
  return(y)
}

#' @rdname update
#'
#' @method update GurobiOpts
#'
#' @export
update.GurobiOpts <- function(object, Threads = NULL, MIPGap = NULL,
                              Method = NULL, Presolve = NULL, TimeLimit = NULL,
                              NumberSolutions = NULL,
                              MultipleSolutionsMethod = NULL, ...) {
  # update arguments
  if (!is.null(Threads))
    object@Threads <- Threads
  if (!is.null(MIPGap))
    object@MIPGap <- MIPGap
  if (!is.null(Method))
    object@Method <- Method
  if (!is.null(Presolve))
    object@Presolve <- Presolve
  if (!is.null(TimeLimit))
    object@TimeLimit <- TimeLimit
  if (!is.null(NumberSolutions))
    object@NumberSolutions <- NumberSolutions
  if (!is.null(MultipleSolutionsMethod))
    object@MultipleSolutionsMethod <- MultipleSolutionsMethod
  # check object for validity
  methods::validObject(object, test = FALSE)
  # return object
  return(object)
}
