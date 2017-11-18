#' @include RcppExports.R raptr-internal.R misc.R generics.R RapReliableOpts.R RapUnreliableOpts.R RapData.R RapUnsolved.R RapResults.R
NULL

#' RapSolved: An S4 class to represent RAP inputs and outputs
#'
#' This class is used to store RAP input and output data in addition to input
#' parameters.
#'
#' @slot opts \code{\link{RapReliableOpts}} or \code{\link{RapUnreliableOpts}}
#'   object used to store input parameters.
#'
#' @slot solver \code{\link{GurobiOpts}} or \code{\link{ManualOpts}} object
#'   used to store solver information/parameters.
#'
#' @slot data \code{\link{RapData}} object used to store input data.
#'
#' @slot results \code{\link{RapResults}} object used to store results.
#'
#' @seealso \code{\link{RapReliableOpts-class}},
#'   \code{\link{RapUnreliableOpts-class}}, \code{\link{RapData-class}},
#'   \code{\link{RapResults-class}}.
#'
#' @name RapSolved-class
#'
#' @rdname RapSolved-class
#'
#' @exportClass RapSolved
methods::setClass("RapSolved", methods::representation(opts = "RapOpts",
                                                       solver = "SolverOpts",
                                                       data = "RapData",
                                                       results = "RapResults"))

methods::setClassUnion("RapUnsolOrSol", c("RapSolved", "RapUnsolved"))

#' Create new RapSolved object
#'
#' This function creates a \code{\link{RapSolved}} object.
#'
#' @param unsolved \code{\link{RapUnsolved}} object.
#'
#' @param solver \code{\link{GurobiOpts}} or \code{\link{ManualOpts}} object.
#'
#' @param results \code{\link{RapResults}} object.
#'
#' @return \code{\link{RapSolved}} object.
#'
#' @seealso \code{\link{RapSolved-class}}, \code{\link{RapResults-class}},
#'   \code{link{solve}}.
#'
#' @export
RapSolved <- function(unsolved, solver, results) {
  methods::new("RapSolved", opts = unsolved@opts, solver = solver,
               data = unsolved@data, results = results)
}

#' @rdname solve
#'
#' @usage \S4method{solve}{RapUnsolOrSol,missing}(a, b, ..., verbose = FALSE)
#'
#' @name solve
#'
#' @aliases solve,RapUnsolOrSol,missing-method
methods::setMethod("solve",
  methods::representation(a = "RapUnsolOrSol", b = "missing"),
  function(a, b, ..., verbose = FALSE)
    solve(a, b = GurobiOpts(...), verbose))


#' @rdname solve
#'
#' @usage \S4method{solve}{RapUnsolOrSol,GurobiOpts}(a, b, verbose = FALSE)
#'
#' @name solve
#'
#' @aliases solve,RapUnsolOrSol,GurobiOpts-method
methods::setMethod("solve",
  methods::representation(a = "RapUnsolOrSol", b = "GurobiOpts"),
  function(a, b, verbose = FALSE) {
    ## init
    # run object checks
    if (!a@data@skipchecks) methods::validObject(a@data, test = FALSE)
    methods::validObject(a@opts, test = FALSE)
    # check that gurobi is installed
    if (!is.null(options()$GurobiInstalled))
        is.GurobiInstalled()
    if (!options()$GurobiInstalled$gurobi) {
      is.GurobiInstalled()
      if (!options()$GurobiInstalled$gurobi)
        stop(paste0("The Gurobi software package and the \"gurobi\" R package ",
                    "must be intalled."))
    }
    if (!options()$GurobiInstalled$rgurobi &&
        b@MultipleSolutionsMethod == "solution.pool") {
      is.GurobiInstalled()
      stop(paste0("The \"rgurobi\" R package must be intalled to access the ",
                  "solution pool."))
    }

    # generate model object
    model <- rcpp_generate_model_object(a@opts,
                                        inherits(a@opts, "RapUnreliableOpts"),
                                        a@data, verbose)
    model$A <- Matrix::sparseMatrix(i = model$Ar$row + 1, j = model$Ar$col + 1,
                                    x = model$Ar$value,
                                    dims = c(max(model$Ar$row) + 1,
                                             length(model$obj)))
    # run basic checks that the model matrix has been constructed correctly
    assertthat::assert_that(
      sum(!is.finite(model$Ar$value)) == 0,
      msg = "Invalid model matrix constructed.")
    ## Initial run
    # run model
    log.pth <- tempfile(fileext = ".log")
    gparams <- append(as.list(b), list("LogFile" = log.pth))
    if (b@MultipleSolutionsMethod == "benders.cuts") {
      if (options()$GurobiInstalled$gurobi) {
        solution <- gurobi::gurobi(model, gparams)
      }
    } else {
      if (options()$GurobiInstalled$rgurobi) {
        solution <- rgurobi::gurobi(model, gparams,
                                    NumberSolutions = b@NumberSolutions)
      }
    }
    if (file.exists("gurobi.log")) unlink("gurobi.log")

    # check solution object
    if (!is.null(solution$status))
      if (solution$status == "INFEASIBLE") {
        stop(paste0("No solution found because the problem cannot be solved ",
                    "because space-based targets are too high. Try setting ",
                    "lower space-based targets. See ?maximum.targets"))
      }
    if (is.null(solution$x)) {
      stop(paste0("No solution found because Gurobi parameters do not allow ",
                  "sufficient time."))
    }

    ## Subsequent runs if using Bender's cuts to obtain multiple solutions
    if (b@MultipleSolutionsMethod == "benders.cuts") {
      # store results
      results <- list(read.RapResults(a@opts, a@data, model,
                                      paste(readLines(log.pth),
                                            collapse = "\n"),
                                      solution, verbose))
      existing.solutions <- list(selections(results[[1]]))
      ## subsequent runs
      for (i in seq_len(b@NumberSolutions - 1)) {
        # create new model object, eacluding existing solutions as valid
        # solutions to ensure a different solution is obtained
        model <- rcpp_append_model_object(
          model, existing.solutions[length(existing.solutions)])
        model$A <- Matrix::sparseMatrix(i = model$Ar$row + 1,
                                        j = model$Ar$col + 1,
                                        x = model$Ar$value,
                                        dims = c(max(model$Ar$row) + 1,
                                                 length(model$obj)))
        # run model
        solution <- gurobi::gurobi(model, gparams)
        if (file.exists("gurobi.log")) unlink("gurobi.log")
        # load results
        if (!is.null(solution$status))
          if (solution$status == "INFEASIBLE") {
            warning(paste0("only ", i, " solutions found"))
            break
          }
        if (is.null(solution$x)) {
          warning(paste0("only ", i, " solutions found"))
          break
        }

        # store results
        currResult <- read.RapResults(a@opts, a@data, model,
                                      paste(readLines(log.pth),
                                            collapse = "\n"), solution, verbose)
        results <- append(results, currResult)
        existing.solutions <- append(existing.solutions,
                                     list(selections(currResult)))
      }
    } else {
      # init results list
      results <- list()
      # extract solutions
      for (i in seq_along(solution$obj)) {
        # generate result object
        currList <- solution
        currList$x <- solution$x[i,, drop = TRUE]
        currList$objval <- solution$objval[i]
        currResult <- read.RapResults(a@opts, a@data, model,
                                      paste(readLines(log.pth),
                                            collapse = "\n"),
                                      currList, verbose)
        results <- append(results, currResult)
      }
    }
    # return RapSolved object
    RapSolved(unsolved = a, solver = b, results = mergeRapResults(results))
})

#' @rdname solve
#'
#' @usage \S4method{solve}{RapUnsolOrSol,matrix}(a, b, verbose = FALSE)
#'
#' @name solve
#'
#' @aliases solve,RapUnsolOrSol,matrix-method
methods::setMethod("solve",
  methods::representation(a = "RapUnsolOrSol", b = "matrix"),
  function(a, b, verbose = FALSE) {
    # check arguments for validity
    assertthat::assert_that(
      ncol(b) == nrow(a@data@pu),
      msg = "argument to b has different number of planning units to a")
    assertthat::assert_that(
      all(is.finite(b)),
      msg = "argument to b must not contain any NA values")
    assertthat::assert_that(
      all(b %in% c(0, 1)),
      msg = "argument to b must be binary selections when b is a matrix")
    # generate result objects
    model <- rcpp_generate_model_object(a@opts,
                                        inherits(a@opts, "RapUnreliableOpts"),
                                        a@data, verbose)
    results <- list()
    for (i in seq_len(nrow(b))) {
      # generate result object
      currResult <- read.RapResults(a@opts, a@data, model,
                                    "User specified solution",
                                     list(x = b[i, ], objval = NA,
                                          status = "MANUAL"), verbose)
      results <- append(results, currResult)
    }
    # return RapSolved object
    RapSolved(unsolved = a, solver = ManualOpts(NumberSolutions = nrow(b)),
              results = mergeRapResults(results))
})

#' @rdname solve
#'
#' @usage \S4method{solve}{RapUnsolOrSol,numeric}(a, b, verbose = FALSE)
#'
#' @name solve
#'
#' @aliases solve,RapUnsolOrSol,numeric-method
methods::setMethod("solve",
  methods::representation(a = "RapUnsolOrSol",  b = "numeric"),
  function(a, b, verbose = FALSE) {
    # check arguments for validity
    assertthat::assert_that(all(b %in% seq_len(nrow(a@data@pu))),
      msg = "argument to b refers to planning unit indices not in a")
    # return RapSolved object
    solve(a, b = matrix(replace(rep(0, nrow(a@data@pu)), b,
                                rep(1, length(b))), nrow = 1),
          verbose = verbose)
})

#' @rdname solve
#'
#' @usage \S4method{solve}{RapUnsolOrSol,logical}(a, b, verbose = FALSE)
#'
#' @name solve
#'
#' @aliases solve,RapUnsolOrSol,logical-method
methods::setMethod("solve",
  methods::representation(a = "RapUnsolOrSol", b = "logical"),
  function(a, b, verbose = FALSE) {
    # check arguments for validity
    assertthat::assert_that(
      length(b) == nrow(a@data@pu),
      msg = "argument to b has different number of planning units to a")
    # generate RapSolved object with user-specified solution
    solve(a, b = matrix(as.numeric(b), nrow = 1), verbose = verbose)
})

#' @rdname selections
#'
#' @inheritParams selections
#'
#' @export
selections.RapSolved <- function(x, y = 0) {
  selections.RapResults(x@results, y)
}

#' @rdname score
#'
#' @inheritParams score
#'
#' @export
score.RapSolved <- function(x, y = 0) {
  score.RapResults(x@results, y)
}

#' @method summary RapSolved
#'
#' @export
summary.RapSolved <- function(object, ...) {
  summary.RapResults(object@results)
}

#' @export
#'
#' @inheritParams amount.held
#'
#' @rdname amount.held
amount.held.RapSolved <- function(x, y = 0, species = NULL) {
  # get solution numbers
  if (is.null(y))
    y <- seq_len(nrow(x@results@selections))
  if (all(y == 0))
    y <- x@results@best
  # get species numbers
  if (is.null(species))
    species <- seq_len(nrow(x@data@species))
  if (is.character(species))
    species <- match(species, x@data@species$name)
  # return named vector
  structure(x@results@amount.held[y, species],
            .Dim = c(length(y), length(species)),
            .Dimnames = list(seq_along(y),
                             x@data@species$name[species]))
}

#' @rdname space.held
#'
#' @inheritParams space.held
#'
#' @export
space.held.RapSolved <- function(x, y = 0, species = NULL, space = NULL) {
  # get solution numbers
  if (is.null(y))
    y <- seq_len(nrow(x@results@selections))
  if (all(y == 0))
    y <- x@results@best
  # convert species to numeric
  if (is.character(species)) {
    species <- match(species, x@data@species[[1]])
    assertthat::assert_that(
      all(!is.na(species)),
      msg = "argument to species not found in argument to x")
  }
  # convert space to numeric
  if (is.character(space)) {
    space <- match(space, sapply(x@data@attribute.spaces, methods::slot,
                                 "name"))
    assertthat::assert_that(
      all(!is.na(space)),
      msg = "argument to species not found in argument to x")
  }
  # get species number
  if (is.null(species))
    species <- seq_len(nrow(x@data@species))
  # get space numbers
  if (is.null(space))
    space <- seq_along(x@data@attribute.spaces)
  # return named array
  as_ind <- rep(seq_along(x@data@attribute.spaces), nrow(x@data@species))
  sp_ind <- rep(seq_len(nrow(x@data@species)),
                each = length(x@data@attribute.spaces))
  structure(c(x@results@space.held[y, sp_ind %in% species & as_ind %in% space]),
            dim = c(length(y), length(species) * length(space)),
            dimnames = list(seq_along(y),
                            paste0(rep(x@data@species$name[species],
                                       each = length(space)),
                                   rep(paste0(" (Space ", space, ")"),
                                       length(species)))))
}

#' @rdname logging.file
#'
#' @inheritParams logging.file
#'
#' @export
logging.file.RapSolved <- function(x, y = 0) {
  logging.file.RapResults(x@results, y)
}

#' @method print RapSolved
#'
#' @rdname print
#'
#' @export
print.RapSolved <- function(x, ...) {
  message("RapSolved object\n")
  message("Parameters")
  print(x@opts, header = FALSE)
  message("Solver settings")
  print(x@solver, header = FALSE)
  message("Data")
  print.RapData(x@data, header = FALSE)
  cat("Results")
  print.RapResults(x@results, header = FALSE)
  invisible()
}

#' @rdname spp.subset
#'
#' @method spp.subset RapUnsolOrSol
#'
#' @export
spp.subset.RapUnsolOrSol <- function(x, species) {
  RapUnsolved(opts = x@opts, data = spp.subset(x@data, species))
}

#' @rdname pu.subset
#'
#' @method pu.subset RapUnsolOrSol
#'
#' @export
pu.subset.RapUnsolOrSol <- function(x, pu) {
  RapUnsolved(opts = x@opts, data = pu.subset(x@data, pu))
}

#' @rdname dp.subset
#'
#' @method dp.subset RapUnsolOrSol
#'
#' @export
dp.subset.RapUnsolOrSol <- function(x, space, species, points) {
  RapUnsolved(opts = x@opts, data = dp.subset(x@data, space, species, points))
}

#' @rdname prob.subset
#'
#' @method prob.subset RapUnsolOrSol
#'
#' @export
prob.subset.RapUnsolOrSol <- function(x, species, threshold) {
  RapUnsolved(opts = x@opts, data = prob.subset(x@data, species, threshold))
}

#' @rdname show
#'
#' @usage \S4method{show}{RapSolved}(object)
#'
#' @name show
#'
#' @aliases show,RapSolved-method
methods::setMethod("show", "RapSolved",
                   function(object) print.RapSolved(object))

#' @rdname is.comparable
#'
#' @usage \S4method{is.comparable}{RapUnsolOrSol,RapUnsolOrSol}(x, y)
#'
#' @name is.comparable
#'
#' @aliases is.comparable,RapUnsolOrSol,RapUnsolOrSol-method
methods::setMethod("is.comparable",
  methods::signature(x = "RapUnsolOrSol", y = "RapUnsolOrSol"),
  function(x, y) is.comparable(x@data, y@data))

#' @rdname is.comparable
#'
#' @usage \S4method{is.comparable}{RapData,RapUnsolOrSol}(x, y)
#'
#' @name is.comparable
#'
#' @aliases is.comparable,RapData,RapUnsolOrSol-method
setMethod("is.comparable",
  methods::signature(x = "RapData", y = "RapUnsolOrSol"),
  function(x, y) is.comparable(x, y@data))

#' @rdname is.comparable
#'
#' @usage \S4method{is.comparable}{RapUnsolOrSol,RapData}(x, y)
#'
#' @name is.comparable
#'
#' @aliases is.comparable,RapUnsolOrSol,RapData-method
methods::setMethod("is.comparable",
  methods::signature(x = "RapUnsolOrSol", y = "RapData"),
  function(x, y) is.comparable(x@data, y))

#' @rdname basemap
#'
#' @export
basemap.RapSolved <- function(x, basemap = "none", grayscale = FALSE,
                              force.reset = FALSE) {
  basemap.RapData(x@data, basemap, grayscale, force.reset)
}

#' @rdname plot
#'
#' @usage \S4method{plot}{RapSolved,numeric}(x, y, basemap = "none",
#'  pu.color.palette = c("#e5f5f9", "#00441b", "#FFFF00", "#FF0000"), alpha =
#'  ifelse(basemap == "none", 1, 0.7), grayscale = FALSE, main = NULL,
#'  force.reset = FALSE)
#'
#' @name plot
#'
#' @aliases plot,RapSolved,numeric-method
methods::setMethod("plot",
  methods::signature(x = "RapSolved", y = "numeric"),
  function(x, y, basemap = "none",
           pu.color.palette = c("#e5f5f9", "#00441b", "#FFFF00", "#FF0000"),
           alpha = ifelse(basemap == "none", 1, 0.7),
           grayscale = FALSE, main = NULL, force.reset = FALSE) {
    # check for issues
    assertthat::assert_that(nrow(x@data@polygons) > 0,
                            assertthat::is.scalar(alpha),
                            isTRUE(alpha <= 1), isTRUE(alpha >= 0),
                            is.character(pu.color.palette),
                            length(pu.color.palette) == 4,
                            all(!is.na(pu.color.palette)),
                            assertthat::is.flag(grayscale),
                            assertthat::is.string(main) || is.null(main),
                            assertthat::is.flag(force.reset),
                            assertthat::is.count(y + 1),
                            y <= nrow(x@results@selections))
    # get basemap data
    if (basemap != "none")
      basemap <- basemap.RapData(x@data, basemap, grayscale, force.reset)
    # main processing
    if (y == 0)
      y <- x@results@best
    values <- x@results@selections[y, ]
    cols <- character(length(values))
    cols[which(values == 0)] <- pu.color.palette[1]
    cols[which(values == 1)] <- pu.color.palette[2]
    cols[which(x@data@pu$status == 2)] <- pu.color.palette[3]
    cols[which(x@data@pu$status == 3)] <- pu.color.palette[4]
    # set title
    if (is.null(main)) {
      if (y == x@results@best) {
        main <- paste0("Best solution (", y, ")")
      } else {
        main <- paste0("Solution ", y)
      }
    }
    prettyGeoplot(x@data@polygons, cols, basemap, main = main,
                  categoricalLegend(pu.color.palette[c(4, 1, 2, 3)],
                                    c("Locked Out", "Not Selected",
                                      "Selected", "Locked In")),
                  beside = FALSE,
                  border = "gray30")
})

#' @rdname plot
#'
#' @usage \S4method{plot}{RapSolved,missing}(x, y, basemap = "none",
#' pu.color.palette = c("PuBu", "#FFFF00", "#FF0000"),
#' alpha = ifelse(basemap == "none", 1, 0.7),
#' grayscale = FALSE, main = NULL,
#' force.reset = FALSE)
#'
#' @name plot
#'
#' @aliases plot,RapSolved,missing-method
methods::setMethod("plot",
  methods::signature(x = "RapSolved", y = "missing"),
  function(x, y, basemap = "none",
           pu.color.palette = c("PuBu", "#FFFF00", "#FF0000"),
           alpha = ifelse(basemap == "none", 1, 0.7),
           grayscale = FALSE, main = NULL,
           force.reset = FALSE) {
    # check for issues
    basemap <- match.arg(basemap, c("none", "roadmap", "mobile", "satellite",
                                    "terrain", "hybrid", "mapmaker-roadmap",
                                    "mapmaker-hybrid"))
    assertthat::assert_that(nrow(x@data@polygons) > 0,
                            assertthat::is.string(basemap),
                            is.character(pu.color.palette),
                            length(pu.color.palette) == 3,
                            pu.color.palette[1] %in%
                              rownames(RColorBrewer::brewer.pal.info),
                            all(!is.na(pu.color.palette)),
                            assertthat::is.scalar(alpha),
                            isTRUE(alpha >= 0), isTRUE(alpha <= 1),
                            assertthat::is.flag(grayscale),
                            assertthat::is.string(main) || is.null(main),
                            assertthat::is.flag(force.reset))
    # get basemap data
    if (basemap != "none")
      basemap <- basemap.RapData(x@data, basemap, grayscale, force.reset)
    # set title
    if (is.null(main)) {
      main <- "Selection frequencies (%)"
    }
    # main processing
    if (force.reset || !is.cached(x@results, "selectionfreqs")) {
      cache(x@results, "selectionfreqs", colMeans(x@results@selections))
    }
    values <- cache(x@results, "selectionfreqs")[which(x@data@pu$status < 2)]
    cols <- character(length(cache(x@results, "selectionfreqs")))
    cols[which(x@data@pu$status < 2)] <- brewerCols(
      scales::rescale(values, from = range(values), to = c(0, 1)),
      pal = pu.color.palette[1], alpha = alpha)
    cols[which(x@data@pu$status == 2)] <- pu.color.palette[2]
    cols[which(x@data@pu$status == 3)] <- pu.color.palette[3]
    # make plot
    prettyGeoplot(x@data@polygons, cols, basemap, main = main,
                  continuousLegend(values, pu.color.palette[1],
                                   posx = c(0.3, 0.4),posy = c(0.1, 0.9)),
                  beside = TRUE, border = "gray30")
})

#' @rdname plot
#'
#' @usage \S4method{plot}{RapSolved,RapSolved}(x, y, i = NULL, j = i,
#' basemap = "none",
#' pu.color.palette = ifelse(is.null(i), c("RdYlBu", "#FFFF00",
#' "#FF0000"), "Accent"),
#' alpha = ifelse(basemap == "none", 1, 0.7),
#' grayscale = FALSE, main = NULL, force.reset = FALSE)
#'
#' @name plot
#'
#' @aliases plot,RapSolved,RapSolved-method
methods::setMethod("plot",
  methods::signature(x = "RapSolved", y = "RapSolved"),
  function(x, y, i = NULL, j = i, basemap = "none",
           pu.color.palette = ifelse(is.null(i), c("RdYlBu", "#FFFF00",
                                                   "#FF0000"), "Accent"),
           alpha = ifelse(basemap == "none", 1, 0.7),
           grayscale = FALSE, main = NULL, force.reset = FALSE) {
    # check for issues
    basemap <- match.arg(basemap, c("none", "roadmap", "mobile", "satellite",
                                    "terrain", "hybrid", "mapmaker-roadmap",
                                    "mapmaker-hybrid"))
    assertthat::assert_that(nrow(x@data@polygons) > 0,
                            is.comparable(x, y),
                            assertthat::is.string(basemap),
                            is.character(pu.color.palette),
                            length(pu.color.palette) == 1 ||
                            length(pu.color.palette) == 3,
                            all(!is.na(pu.color.palette)),
                            pu.color.palette[1] %in%
                              rownames(RColorBrewer::brewer.pal.info),
                            assertthat::is.scalar(alpha),
                            isTRUE(alpha >= 0), isTRUE(alpha <= 1),
                            assertthat::is.flag(grayscale),
                            assertthat::is.string(main) || is.null(main),
                            assertthat::is.flag(force.reset))
    # get basemap data
    if (basemap != "none")
      basemap <- basemap.RapData(x@data, basemap, grayscale, force.reset)
    # main processing
    cols <- character(nrow(x@data@pu))
    if (is.null(i) || is.null(j)) {
      if (is.null(main)) main <- "Difference in selection frequencies (%)"
      cols[which(x@data@pu$status == 2)] <- pu.color.palette[2]
      cols[which(y@data@pu$status == 2)] <- pu.color.palette[2]
      cols[which(x@data@pu$status == 3)] <- pu.color.palette[3]
      cols[which(y@data@pu$status == 3)] <- pu.color.palette[3]

      if (force.reset || !is.cached(x@results, "selectionfreqs"))
        cache(x@results, "selectionfreqs", colMeans(x@results@selections))
      xsc <- cache(x@results, "selectionfreqs")[which(nchar(cols) == 0)]
      if (force.reset || !is.cached(y@results, "selectionfreqs"))
        cache(y@results, "selectionfreqs", colMeans(y@results@selections))
      ysc <- cache(y@results, "selectionfreqs")[which(nchar(cols) == 0)]
      values <- xsc - ysc
      col.pos <- which(nchar(cols) == 0)
      cols[col.pos] <- brewerCols(scales::rescale(values, to = c(0, 1)),
                                                  pu.color.palette[1], alpha)
      # determine legend function
      if (length(unique(round(values, 5))) > 1) {
        legend.fun <- continuousLegend(values, pu.color.palette[1],
                                       posx = c(0.3, 0.4),
                                       posy = c(0.1, 0.9),
                                       center = TRUE,
                                       endlabs = c("+X", "+Y"))
        beside <- TRUE
      } else {
        # create legend entries
        leg.cols <- c(cols[col.pos[1]])
        leg.labs <- c(values[1])
        if (any(x@data@pu$status == 2) | any(y@data@pu$status == 2)) {
          leg.cols <- c(leg.cols, pu.color.palette[2])
          leg.labs <- c(leg.labs, "Locked in")
        }
        if (any(x@data@pu$status == 3) | any(y@data@pu$status == 3)) {
          leg.cols <- c(leg.cols, pu.color.palette[3])
          leg.labs <- c(leg.labs, "Locked out")
        }
        # create legend function
        legend.fun <- categoricalLegend(leg.cols, leg.labs, ncol = 1)
        beside <- FALSE
      }
      prettyGeoplot(x@data@polygons, cols, basemap, main = main,
                    fun = legend.fun, beside = beside, border = "gray30")
    } else {
      if (i == 0)
        i <- x@results@best
      if (j == 0)
        j <- y@results@best
      cols2 <- brewerCols(seq(0, 1, length.out = 8), pu.color.palette, alpha,
                          n = 8)
      cols[which(x@results@selections[i, ] == 1 &
                 y@results@selections[j, ] == 0)] <- cols2[1]
      cols[which(x@results@selections[i, ] == 0 &
                 y@results@selections[j, ] == 1)] <- cols2[2]
      cols[which(x@results@selections[i, ] == 1 &
                 y@results@selections[j, ] == 1)] <- cols2[3]
      cols[which(x@results@selections[i, ] == 0 &
                 y@results@selections[j, ] == 0)] <- cols2[4]

      cols[which(x@data@pu$status == 2)] <- cols2[5]
      cols[which(y@data@pu$status == 2)] <- cols2[6]
      cols[which(x@data@pu$status == 3)] <- cols2[7]
      cols[which(y@data@pu$status == 3)] <- cols2[8]

      if (is.null(main)) {
        main <- paste0("Difference between solution ",
                       i, ifelse(i == x@results@best, " (best)", ""),
                       " and solution ", j,
                       ifelse(j == y@results@best, " (best)", ""))
      }

      prettyGeoplot(x@data@polygons, cols, basemap, main=main,
                    categoricalLegend(c(cols2), c("Selected in X",
                                                  "Selected in Y",
                                                  "Both", "Neither",
                                                  "Locked in X",
                                                  "Locked in Y",
                                                  "Locked out X",
                                                  "Locked out Y"), ncol = 4),
                    beside = FALSE)
    }
})

#' @rdname spp.plot
#'
#' @method spp.plot RapSolved
#'
#' @export
spp.plot.RapSolved<-function(x, species, y = 0, prob.color.palette = "YlGnBu",
                             pu.color.palette = c("#4D4D4D", "#00FF00",
                                                  "#FFFF00", "#FF0000"),
                             basemap = "none",
                             alpha = ifelse(basemap == "none", 1, 0.7),
                             grayscale = FALSE, main = NULL,
                             force.reset = FALSE, ...) {
  # data checks
  basemap <- match.arg(basemap, c("none", "roadmap", "mobile", "satellite",
                                  "terrain", "hybrid", "mapmaker-roadmap",
                                  "mapmaker-hybrid"))
  assertthat::assert_that(nrow(x@data@polygons) > 0,
                          assertthat::is.count(y + 1),
                          y %in% seq(0, nrow(x@results@selections)),
                          assertthat::is.count(species) ||
                            assertthat::is.string(species),
                          assertthat::is.string(basemap),
                          is.character(pu.color.palette),
                          length(pu.color.palette) %in% c(1, 4),
                          all(!is.na(pu.color.palette)),
                          assertthat::is.scalar(alpha),
                          isTRUE(alpha >= 0), isTRUE(alpha <= 1),
                          assertthat::is.flag(grayscale),
                          assertthat::is.string(main) || is.null(main),
                          assertthat::is.flag(force.reset))
  if (is.character(species)) {
    spp_pos <- match(species, x@data@species$name)
    assertthat::assert_that(
      !is.na(spp_pos),
      msg = "argument to species is not a species name in argument to x")
  } else {
    if (is.numeric(species)) {
      assertthat::assert_that(
        species %in% seq_along(x@data@species$name),
        msg = paste0("argument to species is not a valid index for species ",
                     "in argument to x"))
      spp_pos <- species
    }
  }
  # set title
  if (is.null(main)) {
    if ("name" %in% names(x@data@species) & is.numeric(species)) {
      main <- paste0(x@data@species$name[species])
    } else if (is.numeric(species)) {
      main <- paste0("Species ", species)
    } else {
      main <- paste0(species)
    }
  }
  # get basemap
  if (basemap != "none")
    basemap <- basemap.RapData(x, basemap, grayscale, force.reset)
  ## main processing
  # extract planning fill unit colors
  values <- numeric(nrow(x@data@pu))
  rows <- which(x@data@pu.species.probabilities$species == spp_pos)
  values[x@data@pu.species.probabilities$pu[rows]] <-
    x@data@pu.species.probabilities$value[rows]
  if (length(unique(values)) > 1) {
    cols <- brewerCols(scales::rescale(values, to = c(0,1)),
                       prob.color.palette, alpha)
  } else {
    cols <- brewerCols(rep(values[1], length(values)), prob.color.palette,
                       alpha)
    values <- c(0, values[1])
  }
  # get selected rows
  sel.pu.ids <- which(as.logical(selections(x, y)))
  unsel.pu.ids <- which(!as.logical(selections(x, y)))
  # extract planning unit border colors
  border.cols <- rep(pu.color.palette[1], nrow(x@data@pu))
  border.cols[sel.pu.ids] <- pu.color.palette[2]
  border.cols[which(x@data@pu$status == 2)] <- pu.color.palette[3]
  border.cols[which(x@data@pu$status == 3)] <- pu.color.palette[4]
  # make plot
  prettyGeoplot(list(x@data@polygons[x@data@polygons$PID %in% unsel.pu.ids, ],
                     x@data@polygons[x@data@polygons$PID %in% sel.pu.ids, ]),
                list(cols[unsel.pu.ids], cols[sel.pu.ids]),
                basemap, main,
                continuousLegend(values, prob.color.palette,
                                 posx = c(0.3, 0.4),posy = c(0.1, 0.9)),
                beside = TRUE, border = list(border.cols[unsel.pu.ids],
                                             border.cols[sel.pu.ids]),
                lwd = list(1, 5))
}

#' @rdname space.plot
#'
#' @method space.plot RapSolved
#'
#' @export
space.plot.RapSolved <- function(x, species, space = 1, y = 0,
                                 pu.color.palette = c("#4D4D4D4D", "#00FF0080",
                                                      "#FFFF0080", "#FF00004D"),
                                 main = NULL, ...) {
  # data checks
  assertthat::assert_that(assertthat::is.count(species) ||
                            assertthat::is.string(species),
                          assertthat::is.count(space),
                          assertthat::is.count(y + 1),
                          y %in% seq(0, nrow(x@results@selections)),
                          is.character(pu.color.palette),
                          length(pu.color.palette) %in% c(1, 4),
                          all(!is.na(pu.color.palette)),
                          assertthat::is.string(main) || is.null(main))
  if (is.character(species)) {
    spp_pos <- match(species, x@data@species$name)
    assertthat::assert_that(
      !is.na(spp_pos),
      msg = "argument to species is not a species name in argument to x")
  } else{
    if (is.numeric(species)) {
      assertthat::assert_that(
        species %in% seq_along(x@data@species$name),
        msg = paste0("argument to species is not a valid index for species ",
                     "in argument to x"))
      spp_pos <- species
    }
  }
  # set title
  if (is.null(main)) {
    if ("name" %in% names(x@data@species) & is.numeric(species)) {
      main <- paste0(x@data@species$name[species], " in space ", space)
    } else if (is.numeric(species)) {
      main <- paste0("Species ", species, " in space ", space)
    } else {
      main<-paste0(species, " in space ", space)
    }
  }
  # extract pu data
  pu <- as.data.frame(x@data@attribute.spaces[[space]]@spaces[[spp_pos]]@
                      planning.unit.points@coords)
  names(pu) <- paste0("X", seq_len(ncol(pu)))
  pu_ids <- x@data@attribute.spaces[[space]]@spaces[[spp_pos]]@
              planning.unit.points@ids
  pu$status <- "Not Selected"
  pu$status[as.logical(selections(x, y)[pu_ids])] <- "Selected"
  pu$status[which(x@data@pu$status[pu_ids] == 2)] <- "Locked In"
  pu$status[which(x@data@pu$status[pu_ids] == 3)] <- "Locked Out"
  # extract dp data
  dp <- as.data.frame(x@data@attribute.spaces[[space]]@
                        spaces[[spp_pos]]@demand.points@coords)
  names(dp) <- paste0("X",seq_len(ncol(dp)))
  dp$weights <- x@data@attribute.spaces[[space]]@spaces[[spp_pos]]@
                  demand.points@weights
  # make plots
  n_dim <- ncol(x@data@attribute.spaces[[space]]@spaces[[spp_pos]]@
                  planning.unit.points@coords)
  do.call(paste0("spacePlot.", n_dim, 'd' ),
          list(pu, dp, pu.color.palette, main))
}

#' @rdname update
#'
#' @method update RapUnsolOrSol
#'
#' @export
update.RapUnsolOrSol <- function(object, ..., formulation  =NULL,
                                 solve = TRUE) {
  assertthat::assert_that(is.null(formulation) ||
                            assertthat::is.string(formulation),
                          assertthat::is.flag(solve))
  # update formulation
  opts <- object@opts
  if (!is.null(formulation)) {
    match.arg(formulation, c("unreliable", "reliable"))
    # create new opts object
    if (formulation == "unreliable") {
      opts <- RapUnreliableOpts()
    } else {
      opts <- RapReliableOpts()
    }
    # fill in matching slots
    for (i in methods::slotNames(object@opts)) {
      if (i %in% methods::slotNames(opts))
        methods::slot(opts, i) <- methods::slot(object@opts, i)
    }
  }
  # return updated object
  object <- RapUnsolved(opts = do.call("update", append(list(object = opts),
                                                        parseArgs("update",
                                                                  opts, ...))),
                        data = do.call("update", append(list(object =
                                                               object@data),
                                                        parseArgs("update",
                                                          object@data, ...))))
  # solve it
  if (solve) {
    # get any new specified GurobiOpts
    goLST <- parseArgs2(c("Threads", "MIPGap", "NumberSolutions", "TimeLimit",
                          "Presolve", "Method", "MultipleSolutionsMethod"), ...)
    # get old GurobiOpt
    if (inherits(object, "RapSolved")) {
      oldGoLST <- list(Threads = object@Threads, MIPGap = object@MIPGap,
                       NumberSolutions = object@NumberSolutions,
                       TimeLimit = object@TimeLimit,
                       Presolve = object@Presolve, Method = object@Method,
                       MultipleSolutionsMethod = object@MultipleSolutionsMethod)
      if (any(!names(oldGoLST %in% names(goLST)))) {
        goLST<-append(goLST, oldGoLST[!names(oldGoLST %in% names(goLST))])
      }
    }
    # generate new RapSolved object
    object <- do.call(raptr::solve,
                      append(append(list(a = object), goLST),
                             parseArgs2(c("verbose", "b"), ...)))
  }
  object
}

#' @rdname amount.target
#'
#' @method amount.target RapUnsolOrSol
#'
#' @export
amount.target.RapUnsolOrSol <- function(x, species = NULL) {
  amount.target.RapData(x@data, species)
}

#' @rdname space.target
#'
#' @method space.target RapUnsolOrSol
#'
#' @export
space.target.RapUnsolOrSol <- function(x, species = NULL, space = NULL) {
  space.target.RapData(x@data, species, space)
}

#' @rdname amount.target
#'
#' @method amount.target<- RapUnsolOrSol
#'
#' @export
`amount.target<-.RapUnsolOrSol` <- function(x,species = NULL, value) {
  x@data<-`amount.target<-.RapData`(x@data, species, value)
  x
}

#' @rdname space.target
#' @export
`space.target<-.RapUnsolOrSol`<-function(x, species = NULL, space = NULL,
                                         value) {
  x@data<-`space.target<-.RapData`(x@data, species, space, value)
  x
}

#' @rdname names
#'
#' @export
`names<-.RapUnsolOrSol` <- function(x, value) {
  x@data <- `names<-`(x@data, value)
  x
}

#' @rdname names
#'
#' @export
names.RapUnsolOrSol <- function(x) {
  names(x@data)
}

#' @rdname maximum.targets
#'
#' @export
maximum.targets.RapUnsolOrSol <- function(x, verbose = FALSE) {
  assertthat::assert_that(assertthat::is.flag(verbose))
  # generate model object
  model <- rcpp_generate_model_object(x@opts,
                                      inherits(x@opts, "RapUnreliableOpts"),
                                      x@data, verbose)
  # create data.frame
  retDF <- data.frame(species = rep(seq_along(x@data@species$name),
                                    each = length(x@data@attribute.spaces)),
                      target = rep(seq_along(x@data@attribute.spaces),
                                   length(x@data@species$name)),
                      proportion = c(dump_object(
                        model$cache$species_space_best_DBL, "numeric")))
  # merge with targets to get target names
  if ("name" %in% names(x@data@targets))
    retDF <- base::merge(retDF, x@data@targets[, c(1, 2, 4),drop = FALSE],
                         by = c("species", "target"), all = TRUE)
  # set amount-based targets to 1
  retDF[which(retDF$target == 0), "proportion"] <- 1
  # return object
  retDF
}
