#' @include RcppExports.R
NULL

#' Affine transformation
#'
#' @noRd
affineTrans <- function(OldValue, OldMax, OldMin, NewMax, NewMin) {
  OldRange <- (OldMax - OldMin)
  NewRange <- (NewMax - NewMin)
  NewValue <- (((OldValue - OldMin) * NewRange) / OldRange) + NewMin
  NewValue
}

#' Normal niche simulation function
#'
#' @noRd
normal_niche <- function(x, y) {
  40 * mvtnorm::dmvnorm(
    x = matrix(c(x, y), ncol = 2),
    mean = c(0, 0),
    sigma = matrix(c(8.5, 0, 0, 8.5), ncol = 2)
  )
}

#' Uniform niche simulation function
#'
#' @noRd
uniform_niche <- function(x, y) {
  rep(0.5, length(x))
}

#' Bimodal niche simulation function
#'
#' @noRd
bimodal_niche <- function(x, y) {
  apply(
    matrix(
      c(
        mvtnorm::dmvnorm(
          x = matrix(c(x, y), ncol = 2),
          mean = c(-2, -2),
          sigma = matrix(c(5, 0, 0, 5), ncol = 2)
        ) * 30,
        mvtnorm::dmvnorm(
          x = matrix(c(x, y), ncol = 2),
          mean = c(3, 3),
          sigma = matrix(c(3, 0, 0, 3), ncol = 2)
        ) * 12
      ),
      ncol = 2),
    1,
    max
  )
}

#' Function to hash function call
#'
#' @noRd
hashCall <- function(expr, skipargs = c(), env = parent.frame()) {
  expr <- expr[c((skipargs * -1L) - 1L)]
  expr <- expr[which(names(expr) != "force.reset")]
  for (i in seq_along(names(expr)))
    if (inherits(expr[[i]], c("name")))
      expr[[i]] <- eval(expr[[i]], envir = env)
  paste(deparse(expr), collapse = ";")
}

#' Make pretty geoplot
#' @noRd
prettyGeoplot <- function(polygons, col, basemap, main, fun, beside = TRUE,
                          border = NULL, lwd = 1) {
  # make layout
  defpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = c(1, 1, 1, 1), oma = c(0, 0, 0, 0))
  if (beside) {
    graphics::layout(
      matrix(c(1, 1, 3, 2), ncol = 2, byrow = TRUE),
      widths = c(0.8, 0.2),
      heights = c(0.1, 0.9)
    )
  } else {
    graphics::layout(
      matrix(c(1, 3, 2), ncol = 1, byrow = TRUE),
      widths = c(1),
      heights = c(0.1, 0.8, 0.1)
    )
  }
  # convert to list if not
  if (!inherits(polygons, "list")) {
    polygons <- list(polygons)
    col <- list(col)
    border <- list(border)
    lwd <- list(lwd)
  }

  # make title
  graphics::plot(
    1, 1, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
    axes = FALSE, xlab = "", ylab = ""
  )
  graphics::mtext(side = 1, line = -0.5, main, cex = 1.5)
  # make legend
  graphics::plot(
    1, 1, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
    axes = FALSE, xlab = "", ylab = ""
  )
  fun()
  # make geoplot
  if (is.list(basemap)) {
    assertthat::assert_that(
      requireNamespace("RgoogleMaps", quietly = TRUE),
      msg = "please install the \"RgoogleMaps\" package"
    )
    RgoogleMaps::PlotOnStaticMap(basemap)
    for (i in seq_along(polygons)) {
      suppressWarnings(RgoogleMaps::PlotPolysOnStaticMap(
        basemap, polygons[[i]], col = col[[i]], border = border[[i]],
        add = TRUE, lwd = lwd[[i]]))
    }
  } else {
    allpolygons <- do.call(rbind, polygons)
    PBSmapping::plotPolys(
      polygons[[1]], col = col[[1]], axes = FALSE,
      xlab = "", ylab = "", border = border[[1]],
      lwd = lwd[[1]], xlim = range(allpolygons$X),
      ylim = range(allpolygons$Y)
    )
    for (i in seq_along(polygons)[-1]) {
      if (nrow(polygons[[i]]) > 0) {
        suppressWarnings(
          PBSmapping::addPolys(
            polygons[[i]], col = col[[i]], xlab = "", ylab = "",
            border = border[[i]], lwd = lwd[[i]]
          )
        )
      }
    }
  }
  graphics::par(defpar)
  invisible()
}

#' Extract colors from RcolorBrewer palette functions
#'
#' @noRd
brewerCols <- function(values, pal, alpha = 1, n = NULL) {
  if (is.null(n) & length(pal) == 1) {
    idx <- which(rownames(RColorBrewer::brewer.pal.info) == pal)
    n <- RColorBrewer::brewer.pal.info$maxcolors[idx]
  } else {
    n <- length(values)
  }
  if (length(pal) == 1) {
    suppressWarnings({
      r <- grDevices::colorRamp(RColorBrewer::brewer.pal(n, pal))(values)
    })
  } else{
    suppressWarnings({
      r <- grDevices::colorRamp(pal)(values)
    })
  }
  grDevices::rgb(
    r, maxColorValue = 255,
    alpha = scales::rescale(alpha, from = c(0, 1), to = c(0, 255))
  )
}

#' Add continuous legend to plot
#'
#' @noRd
continuousLegend <- function(values, pal, posx, posy, center = FALSE,
                             endlabs = NULL) {
  function() {
    if (center) {
      vabs <- max(abs(range(values)))
      values <- seq(-vabs, vabs, length.out = 100)
    }
    xdiff <- diff(graphics::par()$usr[1:2])
    ydiff <- diff(graphics::par()$usr[3:4])
    zvals <- pretty(values)
    zvals <- zvals[which(zvals > min(values) & zvals < max(values))]
    if (max(zvals) < 1) {
      digit <- 2
    } else {
      digit <- 1
    }
    shape::colorlegend(
      zlim = range(values), digit = digit,
      col = brewerCols(seq(0, 1, length.out = 100), pal),
      zval = zvals, posx = posx, posy = posy, xpd = TRUE
    )
    if (!is.null(endlabs)) {
      xcoord <-
        graphics::par()$usr[1] +
        mean(graphics::par()$usr[2:1] * posx * 2.2)
      ycoords <-
        (graphics::par()$usr[3] +
        diff(graphics::par()$usr[3:4]) * posy) +
        (diff(graphics::par()$usr[3:4]) * c(-0.02, 0.02))
      graphics::text(
        x = rep(xcoord, 2), y = ycoords, rev(endlabs), cex = 1.2,
        ad = c(0.5, 0.5)
      )
    }
  }
}

#' Add categorical legend to plot
#'
#' @noRd
categoricalLegend <- function(col, labels, ncol = 1) {
  function() {
    if (ncol == 1) {
      graphics::legend(
        "top", bg = "white", legend = labels, fill = col,
        bty = "n", horiz = TRUE, cex = 1.5
      )
    } else {
      graphics::legend(
        y = graphics::par()$usr[3] + (diff(graphics::par()$usr[3:4]) * 0.6),
        x = graphics::par()$usr[1] + (diff(graphics::par()$usr[1:2]) * 0.5),
        bg = "white", legend = labels, fill = col, bty = "n",
        ncol = ncol, cex = 1.5, xjust = 0.5, yjust = 0.5,
        xpd = TRUE
      )
    }
  }
}

#' Create 1-dimensional demand points
#'
#' @noRd
demand.points.density1d <- function(pts, n, quantile = 0.95, ...) {
  # transform pts
  curr.mean <- mean(pts[, 1])
  curr.sd <- stats::sd(pts[, 1])
  pts[, 1] <- (pts[, 1] - curr.mean) / curr.sd
  # generate points
  quants <- stats::quantile(
    pts[, 1], c( (1 - quantile) / 2, quantile + (1 - quantile) / 2)
  )
  dp <- stats::runif(n, quants[[1]], quants[[2]])
  # density kernel
  est <- ks::kde(pts[, 1], eval.points = dp, ...)
  # back-transform demand point coordinates
  dp.pts <- (matrix(est$eval.points, ncol = 1) * curr.sd) + curr.mean
  # return object
  list(coords = dp.pts, weights = est$estimate)
}

#' Create 2-dimensional demand points
#'
#' @noRd
demand.points.density2d <- function(pts, n, quantile = 0.95, ...) {
  # transform pts
  curr.mean <- apply(pts, 2, mean)
  curr.sd <- apply(pts, 2, stats::sd)
  pts <- sweep(pts, MARGIN = 2, FUN = "-", curr.mean)
  pts <- sweep(pts, MARGIN = 2, FUN = "/", curr.sd)
  # generate points
  dp <- sp::spsample(
    adehabitatHR::mcp(sp::SpatialPoints(coords = pts),
    percent = quantile * 100, unin = c("m"), unout = c("m2")),
    n * 1.1, type = "random")@coords[seq_len(n), ]
  # fit density kernel
  est <- ks::kde(pts, eval.points = dp, ...)
  # back-transform dps
  dp <- sweep(dp, MARGIN = 2, FUN = "*", curr.sd)
  dp <- sweep(dp, MARGIN = 2, FUN = "+", curr.mean)
  # prepare data to return
  list(coords = dp, weights = est$estimate)
}

#' Create n-dimensional demand points
#'
#' @noRd
demand.points.hypervolume <- function(pts, n, quantile = 0.95, ...) {
  # transform pts
  curr.mean <- apply(pts, 2, mean)
  curr.sd <- apply(pts, 2, stats::sd)
  pts <- sweep(pts, MARGIN = 2, FUN = "-", curr.mean)
  pts <- sweep(pts, MARGIN = 2, FUN = "/", curr.sd)
  # fit density kernel
  args <- list(...)
  if (!"samples.per.point" %in% names(args))
    args$samples.per.point <- 500 * ncol(pts)
  if (args$samples.per.point * nrow(pts) < n) {
    stop(paste0(
      "argument to n.demand.points is too high. Set a higher value ",
      "in the argument to samples.per.point (defaults to ",
      "500 * dimensions in attribute space)."
      )
    )
  }
  # estimate bandwidth for kernel
  if (!"kde.bandwidth" %in% names(args)) {
    args$kde.bandwidth <- hypervolume::estimate_bandwidth(pts)
  }
  # fit kernel
  hv <- do.call(
    hypervolume::hypervolume,
    append(list(data = pts, quantile.requested = quantile), args)
  )
  # extract random points
  rndpos <- sample.int(nrow(hv@RandomPoints), n)
  # extract coordinates and back-transform
  dp <- hv@RandomPoints[rndpos,, drop = FALSE]
  dp <- sweep(dp, MARGIN = 2, FUN = "*", curr.sd)
  dp <- sweep(dp, MARGIN = 2, FUN = "+", curr.mean)
  # return object
  list(coords = dp, weights = hv@ValueAtRandomPoints[rndpos])
}

#' Calculate zonal-means
#'
#' @noRd
zonalMean <- function(x, y, ids = names(y)) {
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(y, "SpatRaster"),
    is.character(ids) || is.numeric(ids),
    assertthat::noNA(ids)
  )
  # rename layers for consistency
  names(y) <- ids
  names(x) <- "pu"
  # compute zonal stats
  d <- terra::zonal(y, x, fun = "mean")
  # convert wide data frame to long format and exclude zeros
  d <- do.call(
    rbind,
    lapply(ids, function(x) {
      d$species <- x
      r <- d[, c("species", "pu", x), drop = FALSE]
      names(r)[3] <- "value"
      # remove zeros
      r[r$value > 0, , drop = FALSE]
    })
  )
  # coerce integers
  if (is.numeric(ids)) {
    d$species <- as.integer(d$species)
  }
  d$pu <- as.integer(d$pu)
  # exclude missing values
  d <- d[is.finite(d$value), , drop = FALSE]
  # return result
  d
}

#' Merge list of RapResults into a single object
#'
#' @noRd
mergeRapResults <- function(x) {
  x <- RapResults(
    summary = do.call(rbind, lapply(x, methods::slot, name = "summary")),
    selections = do.call(rbind, lapply(x, methods::slot, name = "selections")),
    amount.held = do.call(
      rbind,
      lapply(x, methods::slot, name = "amount.held")
    ),
    space.held = do.call(
      rbind,
      lapply(x, methods::slot, name = "space.held")
    ),
    logging.file = sapply(x, methods::slot, name = "logging.file"))
  x@summary$Run_Number <- seq_len(nrow(x@summary))
  x
}

#' Read RAP results
#'
#' This function reads files output from Gurobi and returns a `RapResults`
#' object.
#'
#' @param opts `RapReliableOpts` or `RapUnreliableOpts` object
#'
#' @param data `RapData` object
#'
#' @param model.list `list` object containing Gurobi model data
#'
#' @param logging.file `character` Gurobi log files.
#'
#' @param solution.list `list` object containing Gurobi solution data.
#'
#' @param verbose `logical` print progress messages? Defaults to
#'   `FALSE`.
#'
#' @keywords internal
#'
#' @return `RapResults` object
#'
#' @seealso [RapReliableOpts()], [RapUnreliableOpts()],
#'   [RapData()], [RapResults()].
read.RapResults <- function(opts, data, model.list, logging.file,
                            solution.list, verbose = FALSE) {
  x <- rcpp_extract_model_object(
    opts, inherits(opts, "RapUnreliableOpts"), data, model.list, logging.file,
    solution.list, verbose
  )
  x@.cache <- new.env()
  x
}

#' Compare Rap objects
#'
#' This function checks objects to see if they share the same input data.
#'
#' @param x `RapData`, `RapUnsolved`, or `RapSolved` object.
#'
#' @param y `RapData`, `RapUnsolved`, or `RapSolved` object.
#'
#' @return `logical` are the objects based on the same data?
#'
#' @keywords internal
#'
#' @seealso [RapData-class], [RapUnsolved-class],
#' [RapSolved-class].
#'
#' @name is.comparable
methods::setGeneric("is.comparable",
                    function(x, y) methods::standardGeneric("is.comparable"))

#' Basemap
#'
#' This function retrieves google map data for planning units. The google map
#' data is cached to provide fast plotting capabilities.
#'
#' @param x `RapData`, `RapUnsolved`, `RapSolved` object.
#'
#' @param basemap `character` type of base map to display. Valid names are
#'   `"roadmap"`, `"mobile"`, `"satellite"`, `"terrain"`,
#'   `"hybrid"`, `"mapmaker-roadmap"`, `"mapmaker-hybrid"`.
#'
#' @param grayscale `logical` should base map be gray scale?
#'
#' @param force.reset `logical` ignore data in cache? Setting this as
#'   ignore will make function slower but may avoid bugs in cache system.
#'
#' @return `list` with google map data.
#'
#' @keywords internal
#'
#' @seealso `RgoogleMaps::GetMap.bbox()`, [plot()].
basemap <- function(x, basemap = "hybrid", grayscale = FALSE,
                    force.reset = FALSE) UseMethod("basemap")

#' Test if hash is cached in a Rap object
#'
#' Tests if hash is cached in Rap object.
#'
#' @param x `RapData` or `RapResults` object
#'
#' @param name `character` hash.
#'
#' @note caches are implemented using environments, the hash is used as the
#'   name of the object in the environment.
#' @return `logical` Is it cached?
#'
#' @keywords internal
#'
#' @name is.cached
methods::setGeneric("is.cached",
                    function(x, name) methods::standardGeneric("is.cached"))

#' Get and set cache methods
#'
#' Getter and setter methods for caches in RapData and RapResults object.
#'
#' @param x `RapData` or `RapResults` object
#'
#' @param name `character` hash.
#'
#' @param y if `ANY` this object gets cached with name, else if
#'   `missing` the object hashed at name gets returned.
#'
#' @note caches are implemented using environments, the hash is used as the
#'   name of the object in the environment.
#'
#' @return `ANY` or `NULL` depends on `y` argument.
#'
#' @keywords internal
#'
#' @name cache
methods::setGeneric("cache",
                    function(x, name, y) methods::standardGeneric("cache"))

#' Plot a 1-dimensional attribute space
#'
#' @noRd
spacePlot.1d <- function(pu, dp, pu.color.palette, main) {
  # create X2 vars
  pu$X2 <- 0
  dp$X2 <- 0
  # create colors
  if (length(pu.color.palette) == 1)
    pu.color.palette <- brewerCols(seq(0, 1, 0.25), pu.color.palette, 4)
  # make plot
  ggplot2::ggplot() +
  ggplot2::geom_point(
    ggplot2::aes(
      x = .data[["X1"]], y = .data[["X2"]], alpha = .data[["weights"]]
    ),
    data = dp, color = "darkblue", size = 5,
    position = ggplot2::position_jitter(width = 0, height = 5)
  ) +
  ggplot2::scale_alpha_continuous(name = "Demand point weight") +
  ggplot2::geom_point(
    ggplot2::aes(
      x = .data[["X1"]], y = .data[["X2"]],
      color = .data[["status"]], size = .data[["status"]]
    ),
    data = pu,
    position = ggplot2::position_jitter(width = 0, height = 5)) +
  ggplot2::scale_color_manual(
    name = "Planning unit status",
    values = c(
      "Locked Out" = pu.color.palette[4],
      "Not Selected" = pu.color.palette[1],
      "Selected" = pu.color.palette[2],
      "Locked In" = pu.color.palette[3]
    )
  ) +
  ggplot2::scale_size_manual(
    values = c(
      "Locked Out" = 2, "Not Selected" = 2,
      "Selected" = 4.5, "Locked In" = 4.5
    ),
    guide = "none"
  ) +
  ggplot2::guides(
    alpha = ggplot2::guide_legend(order = 1),
    color = ggplot2::guide_legend(order = 2)
  ) +
  ggplot2::theme_classic() +
  ggplot2::coord_equal() +
  ggplot2::theme(
    legend.position = "right",
    axis.title.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_line(),
    axis.line.x = ggplot2::element_line()
  ) +
  ggplot2::ggtitle(main) +
  ggplot2::xlab("Dimension 1") +
  ggplot2::ylab("")
}

#' Plot a 2-d attribute space
#'
#' @noRd
spacePlot.2d <- function(pu, dp, pu.color.palette, main) {
  # create colors
  if (length(pu.color.palette) == 1)
    pu.color.palette <- brewerCols(seq(0, 1, 0.25), pu.color.palette, 4)
  # make plot
  ggplot2::ggplot() +
  ggplot2::geom_point(
    ggplot2::aes(
      x = .data[["X1"]], y = .data[["X2"]], alpha = .data[["weights"]]
    ),
    data = dp, color = "darkblue", size = 5
  ) +
  ggplot2::scale_alpha_continuous(name = "Demand point weight") +
  ggplot2::geom_point(
    ggplot2::aes(
      x = .data[["X1"]], y = .data[["X2"]],
      color = .data[["status"]], size = .data[["status"]]
    ),
    data = pu
  ) +
  ggplot2::scale_color_manual(
    name = "Planning unit status",
    values = c(
      "Locked Out" = pu.color.palette[4],
      "Not Selected" = pu.color.palette[1],
      "Selected" = pu.color.palette[2],
      "Locked In" = pu.color.palette[3]
    )
  ) +
  ggplot2::scale_size_manual(
    values = c(
      "Locked Out" = 2, "Not Selected" = 2,
      "Selected" = 4.5, "Locked In" = 4.5
    ),
    guide = "none"
  ) +
  ggplot2::guides(
    alpha = ggplot2::guide_legend(order = 1),
    color = ggplot2::guide_legend(order = 2)
  ) +
  ggplot2::theme_classic() +
  ggplot2::coord_equal() +
  ggplot2::theme(
    legend.position = "right",
    axis.line.y = ggplot2::element_line(),
    axis.line.x = ggplot2::element_line()
  ) +
  ggplot2::ggtitle(main) +
  ggplot2::xlab("Dimension 1") +
  ggplot2::ylab("Dimension 2")
}

#' Plot a 3-d attribute space
#'
#' @noRd
spacePlot.3d <- function(pu, dp, pu.color.palette, main) {
  # check if rgl is installed
  assertthat::assert_that(
    requireNamespace("rgl", quietly = TRUE),
    msg = "please install the \"rgl\" package"
  )
  # create frame
  rgl::open3d()
  # add pu points
  if (length(pu.color.palette) == 1)
    pu.color.palette <- brewerCols(seq(0, 1, 0.25), pu.color.palette, 4)
  pu.cols <- character(nrow(pu))
  pu.cols[which(pu$status == "Not Selected")] <- pu.color.palette[1]
  pu.cols[which(pu$status == "Selected")] <- pu.color.palette[2]
  pu.cols[which(pu$status == "Locked In")] <- pu.color.palette[3]
  pu.cols[which(pu$status == "Locked Out")] <- pu.color.palette[4]
  rgl::points3d(as.matrix(pu[, seq_len(3)]), col = pu.cols)
  # add dp points
  dp.cols <- ggplot2::alpha(
    rep("darkblue", nrow(dp)),
    affineTrans(dp$weights, min(dp$weights), max(dp$weights), 0.1, 1)
  )
  rgl::points3d(as.matrix(dp[, seq_len(3)]), col = dp.cols)
  rgl::title3d(main)
}

#' General argument parsing function
#'
#' @noRd
parseArgs <- function(fn, object = NULL, skip = -1, ...) {
  if (!is.null(object))
    fn <- paste0(fn, ".", class(object))
  ellipses.args <- list(...)
  fn.args <- names(formals(fn))
  if (!is.null(skip))
    fn.args <- fn.args[skip]
  ellipses.args[intersect(names(ellipses.args), fn.args)]
}

#' Alternative argument parsing function
#'
#' @noRd
parseArgs2 <- function(args, ...) {
  ellipses.args <- list(...)
  ellipses.args[intersect(names(ellipses.args), args)]
}

#' Create an empty PolySet object
#' @noRd
emptyPolySet <- function() {
  structure(
    list(
      PID = integer(0),
      SID = integer(0),
      POS = integer(0),
      X = numeric(0),
      Y = numeric(0)
    ),
    .Names = c("PID", "SID", "POS", "X", "Y"),
    row.names = integer(0),
    class = c("PolySet", "data.frame")
  )
}

#' Calculate distances between points using URAP
#'
#' @noRd
urap.squared.distance <- function(x, y, y.weights = rep(1, nrow(y))) {
  assertthat::assert_that(
    inherits(x, "matrix"),
    inherits(y, "matrix"),
    is.numeric(y.weights),
    nrow(y) == length(y.weights),
    all(is.finite(c(x))),
    all(is.finite(c(y))),
    all(is.finite(c(y.weights))), all(y.weights >= 0)
  )
  rcpp_squared_distance(x, y, y.weights)
}

#' Calculate distances between points using RRAP
#'
#' @noRd
rrap.squared.distance <- function(pu.coordinates, pu.probabilities,
                                  dp.coordinates, dp.weights, failure.distance,
                                  maximum.r.level =
                                    as.integer(length(pu.probabilities))) {
  # data integreity checks
  assertthat::assert_that(
    inherits(pu.coordinates, "matrix"),
    inherits(dp.coordinates, "matrix"),
    inherits(pu.probabilities, "numeric"),
    is.numeric(dp.weights),
    assertthat::is.scalar(failure.distance),
    assertthat::is.count(maximum.r.level),
    nrow(pu.coordinates) == length(pu.probabilities),
    nrow(dp.coordinates) == length(dp.weights),
    ncol(dp.coordinates) == ncol(pu.coordinates),
    all(is.finite(c(dp.weights))),
    all(is.finite(c(pu.probabilities))),
    all(is.finite(c(pu.coordinates))),
    all(is.finite(c(dp.coordinates))),
    all(is.finite(c(failure.distance))),
    all(is.finite(c(maximum.r.level))),
    maximum.r.level <= nrow(pu.coordinates),
    failure.distance >= 0,
    nrow(pu.coordinates) >= 1,
    nrow(dp.coordinates) >= 1
  )
  # main processing
  rcpp_rrap_squared_distance(
    pu.coordinates,
    pu.probabilities,
    dp.coordinates,
    dp.weights,
    failure.distance,
    maximum.r.level
  )
}

#' Dump object from model cache
#'
#' @noRd
dump_object <- function(x, mode = c("numeric", "integer", "character")) {
  assertthat::assert_that(inherits(x, "externalptr"))
  mode <- match.arg(mode)
  if (mode == "numeric") {
    return(rcpp_dump_numeric_object(x))
  }
  if (mode == "integer") {
    return(rcpp_dump_integer_object(x))
  }
  if (mode == "character") {
    return(rcpp_dump_character_object(x))
  }
}

#' Simulate a Gaussian random field
#'
#' @param n `integer` Number of simulations to generate.
#'
#' @param coords `matrix` Matrix containing coordinates for simulating data.
#'
#' @param mu `numeric` Parameter for Gaussian simulations.
#'
#' @param scale `numeric` Parameter for Gaussian simulations.
#'
#' @details
#' This function is largely inspired by: https://rpubs.com/jguelat/autocorr.
#'
#' @return `matrix` object with simulations.
#'
#' @noRd
simulate_gaussian_random_field <- function(n, coords, mu, scale) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.count(n),
    assertthat::noNA(n),
    is.matrix(coords),
    assertthat::noNA(c(coords)),
    nrow(coords) >= 1,
    ncol(coords) == 2,
    assertthat::is.number(mu),
    assertthat::noNA(mu),
    assertthat::is.number(scale),
    assertthat::noNA(scale)
  )
  # main processing
  mu <- rep(0, nrow(coords))
  p <- nrow(coords)
  chol_d <- chol(exp(-scale * as.matrix(stats::dist(coords))))
  out <- t(
    matrix(stats::rnorm(n * p), ncol = p) %*%
    chol_d + rep(mu, rep(n, p))
  )
  # ensure matrix output
  if (!is.matrix(out)) {
    out <- matrix(out, ncol = 1)
  }
  # return result
  out
}
