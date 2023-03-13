#' @include RcppExports.R raptr-internal.R misc.R generics.R RapReliableOpts.R RapUnreliableOpts.R RapData.R
NULL

#' RapUnsolved: An S4 class to represent RAP inputs
#'
#' This class is used to store RAP input data and input parameters.
#'
#' @slot opts [RapReliableOpts()] or [RapUnreliableOpts()]
#'   object used to store input parameters.
#'
#' @slot data [RapData()] object used to store input data.
#'
#'
#' @seealso  [RapReliableOpts-class],
#'   [RapUnreliableOpts-class], [RapData-class].
#'
#' @name RapUnsolved-class
#'
#' @rdname RapUnsolved-class
#'
#' @exportClass RapUnsolved
methods::setClass("RapUnsolved",
                  methods::representation(opts = "RapOpts", data = "RapData"))

#' Create a new RapUnsolved object
#'
#' This function creates a [RapUnsolved()] object using a
#' [GurobiOpts()], a [RapReliableOpts()] or
#' [RapUnreliableOpts()] object, and a [RapData()] object.
#'
#' @param opts [RapReliableOpts()] or [RapUnreliableOpts()]
#'   object.
#'
#' @param data [RapData()] object.
#'
#' @return [RapUnsolved()] object.
#'
#' @seealso [RapReliableOpts-class],
#'   [RapUnreliableOpts-class], [RapData-class].
#'
#' @examples
#' \dontrun{
#' # set random number generator seed
#' set.seed(500)
#'
#' # load data
#' cs_pus <- sf::read_sf(
#'  system.file("extdata", "cs_pus.gpkg", package = "raptr")
#' )
#' cs_spp <- terra::rast(
#'   system.file("extdata", "cs_spp.tif", package = "raptr")
#' )
#'
#' # create inputs for RapUnsolved
#' ro <- RapUnreliableOpts()
#' rd <- make.RapData(cs_pus[seq_len(10), ], cs_spp, NULL,
#'                    include.geographic.space = TRUE,n.demand.points = 5L)
#'
#' # create RapUnsolved object
#' ru <- RapUnsolved(ro, rd)
#'
#' # print object
#' print(ru)
#' }
#' @export
RapUnsolved <- function(opts, data) {
  methods::new("RapUnsolved", opts = opts, data = data)
}

#' @method print RapUnsolved
#'
#' @rdname print
#'
#' @export
print.RapUnsolved <- function(x, ...) {
  message("RapUnsolved object\n")
  message("Parameters")
  print(x@opts, header = FALSE)
  message("Data")
  print.RapData(x@data, header = FALSE)
  invisible()
}

#' @rdname show
#'
#' @usage \S4method{show}{RapUnsolved}(object)
#'
#' @name show
#'
#' @aliases show,RapUnsolved-method
methods::setMethod("show", "RapUnsolved",
                   function(object) print.RapUnsolved(object))

#' @rdname spp.plot
#'
#' @method spp.plot RapUnsolved
#'
#' @export
spp.plot.RapUnsolved <- function(x, species, prob.color.palette = "YlGnBu",
                                 pu.color.palette = c("#4D4D4D", "#00FF00",
                                                      "#FFFF00", "#FF0000"),
                                 basemap = "none",
                                 alpha = ifelse(basemap == "none", 1, 0.7),
                                 grayscale = FALSE, main = NULL,
                                 force.reset = FALSE, ...) {
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
  spp.plot(x = x@data, species = species,
           prob.color.palette = prob.color.palette,
           pu.color.palette = pu.color.palette, basemap = basemap,
           alpha = alpha, grayscale = grayscale, main = main,
           force.reset = force.reset, ...)
}

#' @rdname space.plot
#'
#' @method space.plot RapUnsolved
#'
#' @export
space.plot.RapUnsolved <- function(x, species, space=1,
                                   pu.color.palette = c("#4D4D4D4D",
                                                        "#00FF0080",
                                                        "#FFFF0080",
                                                        "#FF00004D"),
                                   main=NULL, ...) {
  # set title
  if (is.null(main)) {
    if ("name" %in% names(x@data@species) & is.numeric(species)) {
      main <- paste0(x@data@species$name[species], " in space ", space)
    } else if (is.numeric(species)) {
      main <- paste0("Species ", species, " in space ", space)
    } else {
      main <- paste0(species, " in space ", space)
    }
  }
  space.plot.RapData(x@data, species, space, pu.color.palette, main, ...)
}
