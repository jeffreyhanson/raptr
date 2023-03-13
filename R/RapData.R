#' @include RcppExports.R raptr-internal.R generics.R DemandPoints.R calcBoundaryData.R calcSpeciesAverageInPus.R
NULL

#' RapData: An S4 class to represent RAP input data
#'
#' This class is used to store RAP input data.
#'
#' @slot polygons [PBSmapping::PolySet()] planning unit spatial data
#'   or `NULL` if data not available.
#'
#' @slot pu [base::data.frame()] planning unit data. Columns must be
#'   "cost" (`numeric`), "area" (`numeric`), and "status"
#'   (`integer`).
#'
#' @slot species [base::data.frame()] with species data. Columns must
#'   be "name" (`character`.
#'
#' @slot targets [base::data.frame()] with species data. Columns must
#'   be "species" (`integer`), "target" (`integer`), "proportion"
#'   (`numeric`).
#'
#' @slot pu.species.probabilities [base::data.frame()] with data on
#'   the probability of species in each planning unit. Columns must be "species"
#'   (`integer`), "pu" (`integer`), and "value" (`numeric`).
#'
#' @slot attribute.spaces `list` of `AttributeSpaces` objects with
#'   the demand points and planning unit coordinates.
#'
#' @slot boundary [base::data.frame()] with data on the shared
#'   boundary length of planning units. Columns must be "id1" (`integer`),
#'   "id2" (`integer`), and "boundary" (`numeric`).
#'
#' @slot skipchecks `logical` Skip data integrity checks? May improve
#'   speed for big data sets.
#'
#' @slot .cache [base::environment()] used to cache calculations.
#'
#' @seealso [PBSmapping::PolySet()].
#'
#' @name RapData-class
#'
#' @rdname RapData-class
#'
#' @exportClass RapData
methods::setClass("RapData",
  methods::representation(
    polygons = "PolySet",
    pu = "data.frame",
    species = "data.frame",
    targets = "data.frame",
    pu.species.probabilities = "data.frame",
    attribute.spaces = "list",
    boundary = "data.frame",
    skipchecks = "logical",
    .cache = "environment"
  ),
  validity = function(object) {
    if (!object@skipchecks) {
      ### check column names of inputs
      # pu
      assertthat::assert_that(
        length(setdiff(names(object@pu), c("cost", "area", "status"))) == 0,
        msg = paste0("argument to pu is missing one of these columns: ",
                     "\"cost\", \"area\", or \"status\""))
      assertthat::assert_that(
        is.numeric(object@pu$cost),
        msg = "argument to pu$cost is not numeric")
      assertthat::assert_that(
        all(is.finite(object@pu$cost)),
        msg = "argument to pu$cost contains NA or non-finite values"
      )
      assertthat::assert_that(
        is.numeric(object@pu$area),
        msg = "argument to pu$area is not numeric"
      )
      assertthat::assert_that(
        all(is.finite(object@pu$area)),
        msg = c(
          "argument to pu$cost contains NA or non-finite values"
        )
      )
      assertthat::assert_that(
        is.integer(object@pu$status),
        msg = "argument to pu$status is not integer"
      )
      assertthat::assert_that(
        all(is.finite(object@pu$status)),
        msg = c(
          "argument to pu$status contains NA or non-finite values"
        )
      )
      assertthat::assert_that(
        all(object@pu$status %in% 0L:3L),
        msg = paste0(
          "argument to pu$status must not ",
          "contain values other than 0L, 1L, 2L, 3L"
         )
       )
      # species
      if (!is.null(object@species$name)) {
        if (is.factor(object@species$name))
          object@species$name <- as.character(object@species$name)
        object@species$name <- gsub("[[:punct:]]", "", object@species$name)
        assertthat::assert_that(
          is.character(object@species$name),
          msg = paste0("argument to species$name is not character"))
        assertthat::assert_that(
          all(!is.na(object@species$name)),
          msg = paste0("argument to species$name contains NA values")
        )
      }
      # targets
      assertthat::assert_that(
        identical(names(object@targets), c("species", "target", "proportion")),
        msg = paste0("argument to targets is missing one of these columns: ",
                     "\"species\", \"target\", or \"proportion\""))
      assertthat::assert_that(
        is.integer(object@targets$species),
        msg = paste0("argument to targets$species is not integer"))
      assertthat::assert_that(
        all(!is.na(object@targets$species)),
        msg = paste0(
          "argument to targets$species contains NA or non-finite values"
        )
      )
      assertthat::assert_that(
        is.integer(object@targets$target),
        msg = "argument to targets$target is not integer"
      )
      assertthat::assert_that(
        all(!is.na(object@targets$target)),
        msg = c(
          "argument to targets$target contains NA or non-finite values"
        )
      )
      assertthat::assert_that(
        is.numeric(object@targets$proportion),
        msg = "argument to targets$proportion is not numeric"
      )
      assertthat::assert_that(
        all(object@targets$proportion <= 1, na.rm = TRUE),
        msg = paste0("argument to targets$proportion contains values > 1")
      )
      # pu.species.probabilities
      assertthat::assert_that(
        identical(
          names(object@pu.species.probabilities),
          c("species", "pu", "value")
        ),
        msg = paste0("argument to pu.species.probabilities must have ",
                     "these columns: \"species\", \"pu\", or \"value\"")
      )
      assertthat::assert_that(
        is.integer(object@pu.species.probabilities$pu),
        msg = "argument to pu.species.probabilities$pu is not integer"
      )
      assertthat::assert_that(
        is.integer(object@pu.species.probabilities$species),
        msg = "argument to pu.species.probabilities$species is not integer"
      )
      assertthat::assert_that(
        all(!is.na(object@pu.species.probabilities$value)),
        msg = paste0("argument to pu.species.probabilities$value contains NA ",
                     "or non-finite values")
      )
      assertthat::assert_that(
        all(object@pu.species.probabilities$value >= 0 &
            object@pu.species.probabilities$value <= 1),
        msg = paste0("argument to pu.species.probabilities$value contains ",
                     "values greater than 1 or less than 0")
      )
      # attribute.space
      ## check that each attribute spaces object has different name
      assertthat::assert_that(
        sum(duplicated(sapply(object@attribute.spaces, slot, "name"))) == 0,
        msg = "AttributeSpaces should have unique names"
      )
      # boundary
      assertthat::assert_that(
        identical(names(object@boundary), c("id1", "id2", "boundary")),
        msg = paste0("argument to boundary is missing one of these columns: ",
                     "\"id1\", \"id2\", or \"boundary\"")
      )
      assertthat::assert_that(
        is.integer(object@boundary$id1),
        msg = "argument to boundary$id1 is not integer"
      )
      assertthat::assert_that(
        all(is.finite(object@boundary$id1)),
        msg = "argument to boundary$id1 contains NA or non-finite values"
      )
      assertthat::assert_that(
        is.integer(object@boundary$id2),
        msg = "argument to boundary$id2 is not integer"
      )
      assertthat::assert_that(
        all(is.finite(object@boundary$id2)),
        msg = "argument to boundary$id2 contains NA or non-finite values"
      )
      assertthat::assert_that(
        is.numeric(object@boundary$boundary),
        msg = "argument to boundary$boundary is not numeric"
      )
      assertthat::assert_that(
        all(is.finite(object@boundary$boundary)),
        msg = "argument to boundary$boundary contains NA or non-finite values"
      )
      ## cross table dependencies
      # check all planning units match
      assertthat::assert_that(
        all(object@boundary$id1 %in% seq_len(nrow(object@pu))),
        msg = paste0("argument to boundary$id1 must have values that ",
                     "correspond to rows in argument to pu")
      )
      assertthat::assert_that(
        all(object@boundary$id2 %in% seq_len(nrow(object@pu))),
        msg = paste0("argument to boundary$id2 must have values that ",
                     "correspond to rows in argument to pu")
      )
      assertthat::assert_that(
        all(object@pu.species.probabilities$pu %in% seq_len(nrow(object@pu))),
        msg = paste0("argument to pu.species.probabilities$pu must have ",
                     "values that correspond to rows in argument to pu")
      )
      for (i in seq_along(object@attribute.spaces)) {
        for (j in seq_along(object@attribute.spaces[[i]])) {
          currIds <- object@attribute.spaces[[i]]@spaces[[j]]
          currIds <- currIds@planning.unit.points@ids
          pos <- which(object@pu.species.probabilities$species ==
                       object@attribute.spaces[[i]]@spaces[[j]]@species)
          currPuIds <- object@pu.species.probabilities$pu[pos]
          assertthat::assert_that(
            all(currIds %in% seq_along(object@pu[[1]])),
            msg = paste0(
              "attribute.spaces[[", i, "]]@spaces[[", j,
              "]] contains planning units not in argument to pu"
            )
          )
          assertthat::assert_that(
            all(currIds %in% currPuIds),
            msg = paste0(
              "attribute.spaces[[", i, "]]@spaces[[", j,
              "]] contains planning units associated with ",
              "the species ",
              object@attribute.spaces[[i]]@spaces[[j]]@species,
              " not in argument to pu.species.probabilities"
            )
          )
        }
      }
      # check all species match
      assertthat::assert_that(
        all(object@pu.species.probabilities$species %in%
          seq_len(nrow(object@species))),
        msg = paste0(
          "argument to pu.species.probabilities$species must ",
          "have values that correspond to rows in argument to species"
        )
      )
      assertthat::assert_that(
        all(seq_len(nrow(object@species)) %in%
          object@pu.species.probabilities$species),
        msg = paste0(
          "argument to species has species that do not occur at ",
          "least once in pu.species.probabilities$species"
        )
      )
      for (i in seq_along(object@attribute.spaces)) {
        for (j in seq_along(object@attribute.spaces[[i]])) {
          currSpp <- object@attribute.spaces[[i]]@spaces[[j]]@species
          assertthat::assert_that(
            currSpp %in% seq_along(object@species[[1]]),
            msg = paste0(
              "attribute.spaces[[", i, "]]@spaces[[", j,
              "]] is associated with a species not in argument to ",
              "species"
            )
          )
        }
      }
      assertthat::assert_that(
        all(object@targets$species %in% seq_len(nrow(object@species))),
        msg = paste0(
          "arguments to targets must have species present in ",
          "argument to species"
        )
      )
      # check that attribute spaces match
      assertthat::assert_that(
        all(
          object@targets$target %in%
          seq(0, length(object@attribute.spaces))
        ),
        msg = paste0(
          "argument to targets must have values in values that ",
          "are zero or correspond to elements in argument to ",
          "attribute.spaces"
        )
      )
    }
    return(TRUE)
  }
)

#' Create new RapData object
#'
#' This function creates a "RapData" object using pre-processed data.
#'
#' @param polygons [PBSmapping::PolySet()] planning unit spatial data
#'   or `NULL` if data not available.
#'
#' @param pu [base::data.frame()] planning unit data. Columns must be
#'   "cost" (`numeric`), "area" (`numeric`), and "status"
#'   (`integer`).
#'
#' @param species [base::data.frame()] with species data. Columns
#'   must be "name" (`character`).
#'
#' @param targets [base::data.frame()] with species data.
#'   Columns must be "species" (`integer`), "target" (`integer`),
#'   "proportion" (`numeric`).
#'
#' @param pu.species.probabilities [base::data.frame()] with data on
#'   the probability of species in each planning unit. Columns must be
#'   "species", (`integer`), "pu" (`integer`), and "value"
#'   (`numeric`).
#'
#' @param attribute.spaces `list` of [AttributeSpaces()] objects
#'   with the demand points and planning unit coordinates.
#'
#' @param boundary [base::data.frame()] with data on the shared
#'   boundary length of planning units. Columns must be "id1"
#'   (`integer`), "id2" (`integer`), and "boundary" (`integer`).
#'
#' @param skipchecks `logical` Skip data integrity checks? May improve
#'   speed for big data sets.
#'
#' @param .cache [base::environment()] used to cache calculations.
#'
#' @note Generally, users are not encouraged to change arguments to
#'   `.cache`.
#'
#' @return A new `RapData` object.
#'
#' @seealso [PBSmapping::PolySet()], [sp::SpatialPoints()],
#'   [sp::SpatialPointsDataFrame()], [make.RapData()],
#'   [RapData-class].
#'
#' @examples
#' \dontrun{
#' # load data
#' cs_pus <- sf::read_sf(
#'  system.file("extdata", "cs_pus.gpkg", package = "raptr")
#' )
#' cs_spp <- terra::rast(
#'   system.file("extdata", "cs_spp.tif", package = "raptr")
#' )
#' cs_space <- terra::rast(
#'   system.file("extdata", "cs_space.tif", package = "raptr")
#' )
#'
#' # create data for RapData object
#' attribute.spaces <- list(
#'   AttributeSpaces(name = "geographic", list(
#'     AttributeSpace(
#'       planning.unit.points = PlanningUnitPoints(
#'         suppressWarnings(
#'           sf::st_coordinates(sf::st_centroid(cs_pus[1:10, ]))
#'         ),
#'         seq_len(10)
#'       ),
#'       demand.points = make.DemandPoints(
#'         randomPoints(cs_spp[[1]], n = 10, prob = TRUE)
#'       ),
#'       species = 1L
#'     ))
#'   ),
#'   AttributeSpaces(name = "environmental", list(
#'     AttributeSpace(
#'       planning.unit.points = PlanningUnitPoints(
#'         as.matrix(terra::extract(
#'           cs_space[[1]], as(cs_pus[1:10, ], "SpatVector"),
#'           fun = "mean",
#'           ID = FALSE
#'         )),
#'         seq_len(10)
#'       ),
#'       demand.points = make.DemandPoints(
#'         as.matrix(terra::as.data.frame(cs_space[[1]], na.rm = TRUE))
#'       ),
#'       species = 1L
#'     )
#'  ))
#' )
#' pu.species.probabilities <- calcSpeciesAverageInPus(
#'   cs_pus[1:10,], cs_spp[[1]]
#' )
#' polygons <- convert2PolySet(cs_pus[1:10, ])
#' boundary <- calcBoundaryData(cs_pus[1:10, ])
#'
#' # create RapData object
#' x <- RapData(
#'   pu = cs_pus[1:10, ], species = data.frame(name = "test"),
#'   target = data.frame(species = 1L, target = 0:2, proportion = 0.2),
#'   pu.species.probabilities = pu.species.probabilities,
#'   attribute.spaces = attribute.spaces,
#'   polygons = polygons,
#'   boundary = boundary
#' )
#'
#' # print object
#' print(x)
#' }
#' @export
RapData <- function(pu, species, targets, pu.species.probabilities,
                    attribute.spaces, boundary, polygons = NA,
                    skipchecks = FALSE, .cache = new.env()) {
  # validate data
  assertthat::assert_that(
    inherits(pu, c("sf", "data.frame")),
    inherits(species, "data.frame"),
    inherits(targets, "data.frame"),
    inherits(pu.species.probabilities, "data.frame"),
    inherits(attribute.spaces, "list"),
    inherits(boundary, "data.frame"),
    inherits(polygons, "PolySet"),
    assertthat::is.flag(skipchecks),
    inherits(.cache, "environment")
  )
  # convert pu to data.frame if needed
  if (inherits(pu, "sf")) {
    pu <- sf::st_drop_geometry(pu)
  }
  # convert factors to characters
  if (inherits(species$name, "factor"))
    species$name <- as.character(species$name)
  # remove extra columns
  pu <- pu[, names(pu) %in% c("cost", "area", "status"), drop = FALSE]
  species <- species[, names(species) == "name", drop = FALSE]
  targets <- targets[, names(targets) %in% c("species", "target", "proportion",
                                             "name"), drop = FALSE]
  pu.species.probabilities <- pu.species.probabilities[,
    names(pu.species.probabilities) %in% c("species", "pu", "value"),
    drop = FALSE]
  boundary <- boundary[, names(boundary) %in% c("id1", "id2", "boundary"),
                       drop = FALSE]
  # make object
  rd <- methods::new(
    "RapData",
    polygons = polygons,
    pu = pu,
    species = species,
    targets = targets,
    pu.species.probabilities = pu.species.probabilities,
    attribute.spaces = attribute.spaces,
    skipchecks = skipchecks,
    boundary = boundary,
    .cache = .cache
  )
  # test for validity
  methods::validObject(rd, test = FALSE)
  return(rd)
}

#' Make data for RAP using minimal inputs
#'
#' This function prepares spatially explicit planning unit, species data, and
#' landscape data layers for RAP processing.
#'
#' @param pus [sf::st_as_sf()] with planning unit data.
#'
#' @param species [terra::rast()] with species probability
#'   distribution data.
#'
#' @param spaces `list` of/or [terra::rast()] representing
#'   projects of attribute space over geographic space. Use a `list` to
#'   denote separate attribute spaces.
#'
#' @param amount.target `numeric` vector for area targets (%) for each
#'   species. Defaults to 0.2 for each attribute space for each species.
#'
#' @param space.target `numeric` vector for attribute space targets (%)
#'   for each species. Defaults to 0.2 for each attribute space for each
#'   species and each space.
#'
#' @param n.demand.points `integer` number of demand points to use for
#'   each attribute space for each species. Defaults to 100L.
#'
#' @param kernel.method `character` name of kernel method to use to
#'   generate demand points. Use either `"ks"` or `"hypervolume"`.
#'
#' @param quantile `numeric` quantile to generate demand points within. If
#    0 then demand points are generated across the full range of values the
#'   `species.points` intersect. Defaults to 0.5.
#'
#' @param include.geographic.space `logical` should the geographic space
#'   be considered an attribute space?
#'
#' @param species.points `list` of/or [sf::st_sf()] object species presence
#'   records. Use a
#'   `list` of objects to represent different species. Must have the same
#'   number of elements as `species`. If not supplied then use
#'   `n.species.points` to sample points from the species distributions.
#'
#' @param n.species.points `numeric` vector specifying the number points
#'   to sample the species distributions to use to generate demand points.
#'   Defaults to 20% of the distribution.
#'
#' @param verbose `logical` print statements during processing?
#'
#' @param scale `logical` scale the attribute spaces to unit mean and
#'   standard deviation? This prevents overflow. Defaults to `TRUE`.
#'
#' @param ... additional arguments to [calcBoundaryData()] and
#'   [calcSpeciesAverageInPus()].
#'
#' @return A new `RapData` object.
#'
#' @seealso [RapData-class], [RapData()].
#'
#' @examples
#' \dontrun{
#' # load data
#' cs_pus <- sf::read_sf(
#'  system.file("extdata", "cs_pus.gpkg", package = "raptr")
#' )
#' cs_spp <- terra::rast(
#'   system.file("extdata", "cs_spp.tif", package = "raptr")
#' )
#' cs_space <- terra::rast(
#'   system.file("extdata", "cs_space.tif", package = "raptr")
#' )
#' # make RapData object using the first 10 planning units in the dat
#' x <- make.RapData(cs_pus[1:10,], cs_spp, cs_space,
#'                   include.geographic.space = TRUE)
#' # print object
#' print(x)
#' }
#'
#' @export make.RapData
make.RapData <- function(pus, species, spaces = NULL, amount.target = 0.2,
                         space.target = 0.2, n.demand.points = 100L,
                         kernel.method = c("ks", "hypervolume")[1],
                         quantile = 0.5, species.points = NULL,
                         n.species.points = ceiling(0.2 *
                            terra::global(species, "sum", na.rm = TRUE)[[1]]
                         ),
                         include.geographic.space = TRUE, scale = TRUE,
                         verbose = FALSE, ...) {
  ## init
  # check inputs for validity
  assertthat::assert_that(
    inherits(species.points, c("sf", "NULL", "list")),
    inherits(pus, "sf"),
    inherits(species, "SpatRaster"),
    inherits(spaces, c("NULL", "SpatRaster", "list")),
    assertthat::is.flag(scale),
    assertthat::noNA(scale)
  )
  .cache <- new.env()

  # coerce non-list items to list
  if (!inherits(spaces, "list")) {
    spaces <- list(spaces)
  }

  # create species.points from species
  if (is.null(species.points)) {
    species.points <- lapply(
      seq_len(terra::nlyr(species)),
      function(i) {
        d <- randomPoints(species[[i]], n = n.species.points[[i]])
        sf::st_as_sf(
          stats::setNames(as.data.frame(d), c("x", "y")),
          coords = c("x", "y"),
          crs = sf::st_crs(terra::crs(species[[i]]))
        )
      }
    )
  } else {
    if (!inherits(species.points, "list")) {
      if (inherits(species.points, "sf")) {
        if ("id" %in% names(species.points)) {
          species.points <- lapply(sort(unique(species$id)),
            function(x) species.points[which(species.points$id == x), ])
        } else {
          species.points <- list(species.points)
        }
      } else {
        species.points <- list(species.points)
      }
    }
  }
  # set polygons
  if (verbose) {
    message("Projecting polygons to WGS1984 for rendering.")
  }
  geoPolygons <- sf::as_Spatial(sf::st_transform(pus, 4326))
  geoPolygons <- rcpp_Polygons2PolySet(geoPolygons@polygons)
  ## set pu
  validNames <- c("cost", "status", "area")
  if (inherits(pus, "sf") & any(c("cost", "status", "area") %in% names(pus))) {
    nms <- intersect(validNames, names(pus))
    pu <- sf::st_drop_geometry(pus)[, nms, drop = FALSE]
  } else {
    pu <- data.frame(x = rep(1, nrow(pus)))[, -1, drop = FALSE]
  }
  if (!"cost" %in% names(pu)) {
    pu$cost <- rep(1, nrow(pus))
    warning(
      paste0(
        "argument to pus does not have a \"cost\" column, ",
        "setting all costs to 1"
      ),
      immediate. = TRUE
    )
  }
  if (!"status" %in% names(pu)) {
    pu$status <- rep(0L, nrow(pus))
    warning(
      paste0(
        "argument to pus does not have a \"status\" column, ",
        "setting all planning unit statuses to 0."
      ),
      immediate. = TRUE
    )
  }
  if (!"area" %in% names(pu)) {
    pu$area <- as.numeric(sf::st_area(pus))
    warning(
      paste0(
        "argument to pus does not have a \"area\" column, ",
        "calculating planning unit areas using polygons"
      ),
      immediate. = TRUE
    )
    if (sf::st_is_longlat(pus)) {
      warning(
        paste0(
          "Planning unit areas are being calculated in a ",
          "geographic coordinate system"
        ),
        immediate. = TRUE
      )
    }
  }
  #### Attribute space data
  ## rescale spaces
  if (scale & !is.null(spaces[[1]])) {
    # calculate statistics
    for (i in seq_along(spaces)) {
      spaces.mean <- terra::global(spaces[[i]], "mean", na.rm = TRUE)[[1]]
      spaces.sd <- terra::global(spaces[[i]], "sd", na.rm = TRUE)[[1]]
      # replace NA values with 1 to account for bug in raster
      spaces.sd <- replace(spaces.sd, which(is.na(spaces.sd)), 1)
      spaces[[i]] <- (spaces[[i]] - spaces.mean) / spaces.sd
    }
  }
  ## set pu.points
  # set pu.points based spaces
  pu.points <- list()
  space.names <- c()
  # if spaces is not NULL
  if (!is.null(spaces[[1]])) {
    # create initial rasterized version of the pus
    pus$id <- seq_len(nrow(pus))
    pu.rast <- terra::rasterize(
      methods::as(pus, "SpatVector"), spaces[[1]][[1]], field = "id"
    )
    # extract means
    pu.points <- lapply(spaces, function(x) {
      # generate new raster if needed
      if (!terra::compareGeom(pu.rast, x, stopOnError = FALSE)) {
        pu.rast <- terra::rasterize(methods::as(pus, "SpatVector"), x)
      }
      # extract points
      coordMTX <- matrix(NA, nrow = nrow(pus), ncol = terra::nlyr(x))
      for (i in seq_len(ncol(coordMTX))) {
        vals <- rcpp_groupmean(
          c(terra::values(pu.rast)),
          c(terra::values(x[[i]]))
        )
        coordMTX[attr(vals, "ids"), i] <- c(vals)
      }
      ids <- which(rowSums(is.na(coordMTX)) == 0)
      PlanningUnitPoints(coords = coordMTX[ids, ], ids = ids)
    })
  }
  # calculate positions in geographic space
  if (isTRUE(include.geographic.space)) {
    # get pu centroids
    pu.coords <- suppressWarnings(
      as.matrix(sf::st_coordinates(sf::st_centroid(pus)))
    )
    if (scale) {
      # zscore pu coords
      pu.means <- apply(pu.coords, 2, mean, na.rm = TRUE)
      pu.sds <- apply(pu.coords, 2, stats::sd, na.rm = TRUE)
      pu.coords <- sweep(pu.coords, MARGIN = 2, FUN = "-", pu.means)
      pu.coords <- sweep(pu.coords, MARGIN = 2, FUN = "/", pu.sds)
    }
    pu.points <- append(
      pu.points,
      list(
        PlanningUnitPoints(coords = pu.coords, ids = seq_len(nrow(pu.coords)))
      )
    )
  }
  if (length(pu.points) == 0) {
    stop(paste0("Attribute spaces must be specified. Either ",
                "include.geographic.space=TRUE or spaces must contain at ",
                "least one terra::rast() object"))
  }
  ## set pu.species.probabilities
  projPolygons <- pus
  if (!isTRUE(sf::st_crs(projPolygons) == sf::st_crs(species))) {
    if (verbose) {
      message("Projecting polygons to rasters' CRS.")
    }
    projPolygons <- sf::st_transform(projPolygons, sf::st_crs(species))
  }
  if (verbose) {
    message("Calculating average species probability in planning units.")
  }
  pu.species.probabilities <- calcSpeciesAverageInPus(
    projPolygons, species, ...
  )
  ## set demand.points
  # include geographic space if set
  if (!is.null(spaces[[1]]) & include.geographic.space) {
    spaces <- append(spaces, list(NULL))
  }
  # generate demand points
  demand.points <- list()
  for (i in seq_along(spaces)) {
    dpLST <- list()
    for (j in seq_along(species.points)) {
      # extract space points
      if (is.null(spaces[[i]])) {
        # save name
        # get species coords
        curr.species.points <-
          as.matrix(sf::st_coordinates(species.points[[j]]))
        # zscore species coords
        if (scale) {
          curr.species.points <- sweep(
            curr.species.points, MARGIN = 2, FUN = "-", pu.means
          )
          curr.species.points <- sweep(
            curr.species.points, MARGIN = 2, FUN = "/", pu.sds
          )
        }
        # save species points in the space
        space.points <- curr.species.points
      } else {
        space.points <- as.matrix(
          terra::extract(spaces[[i]], species.points[[j]], ID = FALSE)
        )
        space.points <- space.points[
          is.finite(rowSums(space.points, na.rm = FALSE)), , drop = FALSE
        ]
      }
      # generate demand points
      dpLST[[j]] <- make.DemandPoints(points = space.points,
                                      kernel.method = kernel.method,
                                      n = n.demand.points, quantile = quantile)
    }
    demand.points[[i]] <- dpLST
  }
  # save space names
  space.names <- c()
  for (i in seq_along(spaces)) {
    if (is.null(spaces[[i]])) {
      space.names[i] <- "geographic"
    } else if (!is.null(names(spaces)[i]) && (nchar(names(spaces)[i]) > 0)) {
      space.names[i] <- names(spaces)[i]
    } else if (
        inherits(spaces[[i]], "Raster") && (nchar(spaces[[i]]@file@name) > 0)
      ) {
      space.names[i] <- basename(spaces[[i]]@file@name)
    } else {
      space.names[i] <- paste0("space_", i)
    }
  }
  # create AttributeSpace objects
  attribute.spaces <- lapply(seq_along(spaces), function(i) {
    AttributeSpaces(lapply(seq_along(demand.points[[i]]), function(d) {
        # subset planning unit points to the units occupied by the species
        curr.ids <- pu.species.probabilities$pu[
                      which(pu.species.probabilities$species == d)]
        curr.pos <- stats::na.omit(match(curr.ids, pu.points[[i]]@ids))
        curr.pu.points <- PlanningUnitPoints(
          coords = pu.points[[i]]@coords[curr.pos,, drop = FALSE],
          ids = pu.points[[i]]@ids[curr.pos]
        )
        # create AttributeSpace object
        AttributeSpace(
          planning.unit.points = curr.pu.points,
          demand.points = demand.points[[i]][[d]],
          species = d
        )
      }),
      name = space.names[i]
    )
  })
  ## set boundary
  if (sf::st_is_longlat(pus)) {
    warning(
      paste0(
        "creating boundary length data from pus in WGS1984; ",
        "consider supplying an object in a projected CRS."
      ),
      immediate. = TRUE
    )
  }
  if (verbose) {
    message("Calculating boundary data.")
  }
  boundary <- calcBoundaryData(pus, ...)
  ## set species
  species <- data.frame(name = names(species), stringsAsFactors = FALSE)
  ## set targets
  targets <- rbind(
    expand.grid(
      species = seq_len(nrow(species)),
      target = 0L,
      proportion = amount.target
    ),
    expand.grid(
      species = seq_len(nrow(species)),
      target = seq(1L, length(attribute.spaces)),
      proportion = space.target
    )
  )
  ## return object
  RapData(
    pu = pu,
    species = species,
    targets = targets,
    pu.species.probabilities = pu.species.probabilities,
    attribute.spaces = attribute.spaces,
    boundary = boundary,
    polygons = geoPolygons,
    .cache = .cache
  )
}

#' @rdname basemap
basemap.RapData <- function(x, basemap = "hybrid", grayscale = FALSE,
                            force.reset = FALSE) {
  assertthat::assert_that(
    requireNamespace("RgoogleMaps", quietly = TRUE),
    msg = "please install the \"RgoogleMaps\" package"
  )
  assertthat::assert_that(assertthat::is.string(basemap),
                          assertthat::is.flag(grayscale),
                          assertthat::is.flag(force.reset))
  callchar <- hashCall(match.call(), 1)
  match.arg(basemap, c("roadmap", "mobile", "satellite", "terrain", "hybrid",
                       "mapmaker-roadmap", "mapmaker-hybrid"))
  if (is.null(x@polygons)) {
    stop(paste0("Rap object is not associated with spatially explicit data ",
                "for the planning units."))
  }
  # fetch data from google or cache
  if (force.reset || !is.cached(x, callchar)) {
    cache(
      x,
      callchar,
      RgoogleMaps::GetMap.bbox(
        range(x@polygons[["X"]]),
        range(x@polygons[["Y"]]),
        destfile = tempfile(fileext =  ".png"),
        maptype = basemap,
        GRAYSCALE = grayscale
      )
    )
  }
  return(cache(x, callchar))
}

#' @method print RapData
#'
#' @rdname print
#'
#' @export
print.RapData <- function(x, ..., header = TRUE) {
  assertthat::assert_that(assertthat::is.flag(header))
  if (header)
    message("RapData object.")
  message("  Number of planning units: ", nrow(x@pu))
  message("  Number of species: ", nrow(x@species))
  message("  Number of attribute spaces: ", length(x@attribute.spaces))
}

#' @rdname show
#'
#' @usage \S4method{show}{RapData}(object)
#'
#' @name show
#'
#' @aliases show,RapData-method
methods::setMethod("show",
  methods::signature(object = "RapData"),
  function(object) print.RapData(object))

#' @rdname is.cached
#' @name is.cached
methods::setMethod("is.cached",
  methods::signature(x = "RapData", name = "character"),
  function(x, name) return(!is.null(x@.cache[[name]])))

#' @rdname cache
#' @name cache
methods::setMethod("cache",
  methods::signature(x = "RapData", name = "character", y = "ANY"),
  function(x, name, y) x@.cache[[name]] <- y)

#' @rdname cache
#' @name cache
methods::setMethod("cache",
  methods::signature(x = "RapData", name = "character", y = "missing"),
  function(x, name, y) return(x@.cache[[name]]))

#' @rdname is.comparable
#'
#' @usage \S4method{is.comparable}{RapData,RapData}(x, y)
#'
#' @name is.comparable
#'
#' @aliases is.comparable,RapData,RapData-method
methods::setMethod("is.comparable",
  methods::signature(x = "RapData", y = "RapData"),
  function(x, y) {
    return(identical(nrow(x@pu), nrow(y@pu)) &&
           identical(nrow(x@species), nrow(y@species)) &&
           identical(x@polygons, y@polygons) &&
           identical(x@boundary$id1, y@boundary$id1) &&
           identical(x@boundary$id2, y@boundary$id2))
})

#' @rdname spp.subset
#'
#' @method spp.subset RapData
#'
#' @export
spp.subset.RapData <- function(x, species) {
  # convert species names to integers
  if (inherits(species, "character"))
    species <- match(species, x@species$name)
  assertthat::assert_that(
    all(!is.na(species)), all(species %in% seq_len(nrow(x@species))),
    msg = "argument to species contains names or ids not present in object.")
  # create new objects
  pu.species.probabilities <- x@pu.species.probabilities[which(
    x@pu.species.probabilities$species %in% species), ]
  pu.species.probabilities$species <- match(pu.species.probabilities$species,
                                            species)
  targets <- x@targets[which(x@targets$species %in% species),, drop = FALSE]
  targets$species <- match(targets$species, species)
  # subset attribute spaces
  attribute.spaces <- list()
  as.pos <- c(0)
  for (i in seq_along(x@attribute.spaces)) {
    curr.species <- sapply(x@attribute.spaces[[i]]@spaces, methods::slot,
                           "species")
    if (any(species %in% curr.species)) {
      curr.spaces <- x@attribute.spaces[[i]]@spaces[match(species,
                                                          curr.species)]
      curr.spaces <- lapply(seq_along(curr.spaces), function(j) {
        a <- curr.spaces[[j]]
        a@species <- j
        return(a)
      })
      attribute.spaces <- append(attribute.spaces, list(
        AttributeSpaces(spaces = curr.spaces,
                        name = x@attribute.spaces[[i]]@name)))
      as.pos <- c(as.pos, i)
    }
  }
  # subset targets to include only remaining attribute spaces
  targets <- targets[which(targets$target %in% as.pos), ]
  # update relative indices of targets to reflect new ordering of spaces
  targets$target <- as.integer(match(targets$target, as.pos) - 1)
  # return new object
  RapData(
    pu = x@pu,
    species = x@species[species,, drop = FALSE],
    targets = targets,
    pu.species.probabilities = pu.species.probabilities,
    attribute.spaces = attribute.spaces,
    boundary = x@boundary,
    polygons = x@polygons
  )
}

#' @rdname pu.subset
#'
#' @method pu.subset RapData
#'
#' @export
pu.subset.RapData <- function(x, pu) {
  # check that all pus are valid
  assertthat::assert_that(is.integer(pu), all(is.finite(pu)),
    all(pu %in% seq_len(nrow(x@pu))),
    msg = "argument to pu includes ids for non-existent planning units")
  ## create objects
  # pu.species.probabilities
  pu.species.probabilities <- x@pu.species.probabilities[which(
      x@pu.species.probabilities$pu %in% pu), ]
  species <- unique(pu.species.probabilities$species)
  pu.species.probabilities$species <- match(pu.species.probabilities$species,
                                            species)
  pu.species.probabilities$pu <- match(pu.species.probabilities$pu, pu)
  # boundary
  boundary <- x@boundary[which(x@boundary$id1 %in% pu &
                               x@boundary$id2 %in% pu), ]
  boundary2 <- x@boundary[which(x@boundary$id1 %in% pu &
                                !x@boundary$id2 %in% pu), ]
  boundary2$id2 <- boundary2$id1
  boundary3 <- x@boundary[which(!x@boundary$id1 %in% pu &
                                x@boundary$id2 %in% pu), ]
  boundary3$id1 <- boundary3$id2
  boundary <- do.call(rbind, list(boundary, boundary2, boundary3))
  boundary$id1 <- match(boundary$id1, pu)
  boundary$id2 <- match(boundary$id2, pu)
  boundary <- rcpp_sum_duplicates(boundary[[1]], boundary[[2]], boundary[[3]])
  # polygons
  polygons <- x@polygons[x@polygons$PID %in% pu, ]
  polygons$PID <- match(polygons$PID, pu)
  # return new object
  RapData(
    pu = x@pu[pu,, drop = FALSE],
    species = x@species,
    targets = x@targets,
    pu.species.probabilities = pu.species.probabilities,
    attribute.spaces = lapply(x@attribute.spaces, function(z) {
      AttributeSpaces(
        spaces = lapply(z@spaces, function(y) {
          curr.ids <- stats::na.omit(match(y@planning.unit.points@ids, pu))
          attributes(curr.ids) <- NULL
          curr.pu <- pu[which(pu %in% y@planning.unit.points@ids)]
          AttributeSpace(
            planning.unit.points = PlanningUnitPoints(
              coords = y@planning.unit.points@coords[curr.pu,, drop = FALSE],
              ids = curr.ids
            ),
            demand.points = y@demand.points,
            species = y@species
          )
        }),
        name = z@name
      )
    }),
    boundary = boundary,
    polygons = polygons
  )
}

#' @rdname dp.subset
#'
#' @method dp.subset RapData
#'
#' @export
dp.subset.RapData <- function(x, space, species, points) {
  assertthat::assert_that(
    inherits(space, c("integer", "numeric", "character")),
    inherits(species, c("integer", "numeric", "character")),
    inherits(points, c("numeric", "integer"))
  )
  # coerce character arguments to ids
  if (is.character(space))
    space <- match(space, sapply(x@attribute.spaces, methods::slot, "name"))
  assertthat::assert_that(
    all(!is.na(space)), all(space %in% seq_along(x@attribute.spaces)),
    msg = "argument to space contains name for non-existent space")
  if (inherits(species, "character"))
    species <- match(species, x@species$name)
  assertthat::assert_that(
    all(!is.na(species)), all(species %in% seq_len(nrow(x@species))),
    msg = "argument to species contains names or ids not present in object.")
  # create objects
  attr.space <- x@attribute.spaces
  for (i in seq_along(space)) {
    for (j in seq_along(species)) {
      attr.space[[space[[i]]]]@spaces[[species[[j]]]]@demand.points <-
        DemandPoints(
          attr.space[[space[[i]]]]@spaces[[species[[j]]]]@demand.points@
            coords[points, , drop = FALSE],
          attr.space[[space[[i]]]]@spaces[[species[[j]]]]@demand.points@
            weights[points]
        )
    }
  }
  # return new object
  RapData(
    pu = x@pu,
    species = x@species,
    targets = x@targets,
    pu.species.probabilities = x@pu.species.probabilities,
    attribute.spaces = attr.space,
    boundary = x@boundary,
    polygons = x@polygons
  )
}

#' @rdname prob.subset
#'
#' @method prob.subset RapData
#'
#' @export
prob.subset.RapData <- function(x, species, threshold) {
  assertthat::assert_that(length(species) == length(threshold))
  # create new object
  pu.species.probs <- x@pu.species.probabilities
  for (i in seq_along(species)) {
    rows <- which(pu.species.probs$species == species[i] &
                  pu.species.probs[[3]] < threshold[i])
    if (length(rows) > 0)
      pu.species.probs <- pu.species.probs[-rows,, drop = FALSE]
  }
  # return new object
  RapData(
    pu = x@pu,
    species = x@species,
    targets = x@targets,
    pu.species.probabilities = pu.species.probs,
    attribute.spaces = x@attribute.spaces, boundary = x@boundary,
    polygons = x@polygons
  )
}

#' @rdname update
#'
#' @export
#'
#' @method update RapData
update.RapData <- function(object, species = NULL, space = NULL, name = NULL,
                         amount.target = NULL, space.target = NULL, pu = NULL,
                         cost = NULL, status = NULL, ...) {
  # deparse species
  if (is.null(species)) {
    species <- seq_len(nrow(object@species))
  } else {
    if (is.character(species))
      species <- match(species, object@species$name)
    if (is.na(species) | !species %in% seq_len(nrow(object@species)))
      stop("argument to species not found")
  }
  # deparse space
  if (is.null(space))
    space <- seq_along(object@attribute.spaces)
  # update species
  if (!is.null(name))
    object@species$name[species] <- name
  # update pu
  if (!is.null(pu) & is.null(status))
  if (!is.null(pu) & is.null(cost))
    object@pu$status[pu] <- status
    object@pu$cost[pu] <- cost
  # update amount targets
  if (!is.null(amount.target)) {
    pos <- which(object@targets$target == 0 &
                 object@targets$species %in% species)
    object@targets$proportion[pos] <- amount.target
  }
  # update space targets
  if (!is.null(space.target)) {
    pos <- which(object@targets$target %in% space &
                 object@targets$species %in% species)
    object@targets$proportion[pos] <- space.target
  }
  # check object for validity
  methods::validObject(object, test = FALSE)
  # return object
  return(object)
}

#' @rdname spp.plot
#'
#' @method spp.plot RapData
#'
#' @export
spp.plot.RapData <- function(x, species, prob.color.palette = "YlGnBu",
                             pu.color.palette = c("#4D4D4D", "#00FF00",
                                                  "#FFFF00", "#FF0000"),
                             basemap = "none",
                             alpha = ifelse(identical(basemap, "none"), 1, 0.7),
                             grayscale = FALSE, main = NULL,
                             force.reset = FALSE, ...
) {
  # validate inputs
  assertthat::assert_that(
    assertthat::is.count(species) || assertthat::is.string(species),
    assertthat::is.string(prob.color.palette),
    is.character(pu.color.palette),
    length(pu.color.palette) == 4,
    assertthat::is.string(basemap),
    assertthat::is.scalar(alpha),
    assertthat::is.flag(grayscale),
    is.null(main) || assertthat::is.string(main),
    assertthat::is.flag(force.reset),
    assertthat::noNA(force.reset)
  )
  if (nrow(x@polygons) == 0)
      stop("Spatial data for planning units not present in object")
  if (is.character(species)) {
    if (!species %in% x@species$name)
      stop("argument to species is not a species name in argument to x")
    spp_pos <- match(species, x@species$name)
  }
  if (is.numeric(species)) {
    if (!species %in% seq_along(x@species$name))
      stop(paste0("argument to species is not a valid index for species in ",
                  "argument to x"))
    spp_pos <- species
  }
  # get basemap
  if (basemap != "none")
    basemap <- basemap.RapData(x, basemap, grayscale, force.reset)
  ## main processing
  # extract planning unit colors
  values <- numeric(nrow(x@pu))
  rows <- which(x@pu.species.probabilities$species == spp_pos)
  values[x@pu.species.probabilities$pu[rows]] <-
    x@pu.species.probabilities$value[rows]
  if (length(unique(values)) > 1) {
    cols <- brewerCols(scales::rescale(values, to = c(0, 1)),
                       prob.color.palette, alpha)
  } else {
    cols <- brewerCols(rep(values[1], length(values)), prob.color.palette,
                       alpha)
    values <- c(0, values[1])
  }
  # set title
  if (is.null(main)) {
    if ("name" %in% names(x@species) & is.numeric(species)) {
      main <- paste0(x@species$name[species])
    } else if (is.numeric(species)) {
      main <- paste0("Species ", species)
    } else {
      main <- paste0(species)
    }
  }
  # get selected rows
  sel.pu.ids <- which(x@pu$status == 2)
  unsel.pu.ids <- which(x@pu$status != 2)
  # extract planning unit border colors
  border.cols <- rep(pu.color.palette[1], nrow(x@pu))
  border.cols[sel.pu.ids] <- pu.color.palette[2]
  border.cols[which(x@pu$status == 2)] <- pu.color.palette[3]
  border.cols[which(x@pu$status == 3)] <- pu.color.palette[4]
  # make plot
  prettyGeoplot(
    polygons = list(
      x@polygons[x@polygons$PID %in% unsel.pu.ids, ],
      x@polygons[x@polygons$PID %in% sel.pu.ids, ]
    ),
    col = list(cols[unsel.pu.ids], cols[sel.pu.ids]),
    basemap, main = main,
    continuousLegend(values, prob.color.palette,
                     posx = c(0.3, 0.4), posy = c(0.1, 0.9)),
    beside = TRUE,
    border = list(border.cols[unsel.pu.ids],
                  border.cols[sel.pu.ids]),
    lwd = list(1, 5)
  )
}

#' @rdname space.plot
#'
#' @method space.plot RapData
#'
#' @export
space.plot.RapData <- function(x, species, space = 1,
                               pu.color.palette = c("#4D4D4D4D", "#00FF0080",
                                                    "#FFFF0080", "#FF00004D"),
                               main = NULL, ...) {
  # data checks
  assertthat::assert_that(assertthat::is.count(species) ||
                          assertthat::is.string(species),
                          assertthat::is.count(space),
                          is.character(pu.color.palette),
                          length(pu.color.palette) == 4,
                          assertthat::is.string(main) || is.null(NULL))
  if (is.character(species)) {
    spp_pos <- match(species, x@species$name)
  } else {
    spp_pos <- species
    species <- x@species$name[spp_pos]
  }
  assertthat::assert_that(
    all(!is.na(species)), all(spp_pos %in% seq_len(nrow(x@species))),
    msg = "argument to species contains names or ids not present in object.")
  # set title
  if (is.null(main)) {
    if ("name" %in% names(x@species) & is.numeric(species)) {
      main <- paste0(x@species$name[species], " in space ", space)
    } else if (is.numeric(species)) {
      main <- paste0("Species ", species, " in space ", space)
    } else {
      main <- paste0(species, " in space ", space)
    }
  }
  # extract pu data
  pu <- as.data.frame(x@attribute.spaces[[space]]@spaces[[spp_pos]]@
                        planning.unit.points@coords)
  names(pu) <- paste0("X", seq_len(ncol(pu)))
  pu$status <- "Not Selected"
  pu$status[which(x@pu$status == 2)] <- "Locked In"
  pu$status[which(x@pu$status == 3)] <- "Locked Out"
  # extract dp data
  dp <- as.data.frame(x@attribute.spaces[[space]]@spaces[[spp_pos]]@
                        demand.points@coords)
  names(dp) <- paste0("X", seq_len(ncol(dp)))
  dp$weights <- x@attribute.spaces[[space]]@spaces[[spp_pos]]@
                  demand.points@weights
  # make plots
  do.call(
    paste0(
      "spacePlot.",
       ncol(
         x@attribute.spaces[[space]]@spaces[[spp_pos]]@
          planning.unit.points@coords
        ),
        "d"
    ),
    list(pu, dp, pu.color.palette, main)
  )
}

#' @rdname amount.target
#'
#' @method amount.target RapData
#'
#' @export
amount.target.RapData <- function(x, species = NULL) {
  assertthat::assert_that(
    is.null(species) ||
      is.character(species) ||
      is.numeric(species)
  )
  if (is.null(species))
    return(structure(x@targets$proportion[which(x@targets$target == 0)],
           .Names = x@species$name[x@targets$species[which(x@targets$target ==
                                                           0)]]))
  if (is.character(species))
    species <- match(species, x@species$name)
  return(structure(x@targets$proportion[which(x@targets$target == 0 &
                                              x@targets$species == species)],
                   .Names = x@species$name[x@targets$species[which(
                     x@targets$target == 0 & x@targets$species == species)]]))
}

#' @rdname amount.target
#'
#' @export
`amount.target<-.RapData` <- function(x, species = NULL, value) {
  assertthat::assert_that(
    is.null(species) ||
      is.character(species) ||
      is.numeric(species),
    is.numeric(value))
  if (is.null(species)) {
    x@targets$proportion[which(x@targets$target == 0)] <- value
  } else {
    if (is.character(species))
      species <- match(species, x@species$name)
    if (!all(species %in% seq_len(nrow(x@species)))) {
      stop("species not present in argument to x")
    }
    pos <- which(x@targets$target == 0 & x@targets$species %in% species)
    x@targets$proportion[pos] <- value
  }
  # check of validity
  methods::validObject(x, test = FALSE)
  return(x)
}

#' @rdname space.target
#'
#' @method space.target RapData
#'
#' @export
space.target.RapData <- function(x, species = NULL, space = NULL) {
  assertthat::assert_that(
    is.null(species) ||
      is.character(species) ||
      is.numeric(species),
    is.null(space) ||
      is.numeric(space)
  )
  rows <- seq_len(nrow(x@targets))
  if (!is.null(species)) {
    if (is.character(species))
      species <- match(species, x@species$name)
    if (!all(species %in% seq_len(nrow(x@species))))
      stop("species not present in argument to x")
    rows <- rows[which(x@targets$species %in% species)]
  }
  if (is.null(space)) {
    rows <- rows[which(x@targets$target[rows] > 0)]
  } else{
    if (!all(space %in% x@targets$target))
      stop("space not present in argument to x")
    rows <- rows[which(x@targets$target[rows] %in% space)]
  }
  return(structure(x@targets$proportion[rows],
                   .Dim = c(length(unique(x@targets$species[rows])),
                            length(unique(x@targets$target[rows]))),
                   .Dimnames = list(
                      unique(x@species$name[x@targets$species[rows]]),
                      unique(x@targets$target[rows]))))
}

#' @rdname space.target
#'
#' @export
`space.target<-.RapData` <- function(x, species = NULL, space = NULL, value) {
  assertthat::assert_that(is.null(species) || is.character(species) ||
                          is.numeric(species),
                          is.null(space) || is.numeric(space),
                          is.numeric(value))
  rows <- seq_len(nrow(x@targets))
  if (!is.null(species)) {
    if (is.character(species))
      species <- match(species, x@species$name)
    if (!all(species %in% seq_len(nrow(x@species))))
      stop("species not present in argument to x")
    rows <- rows[which(x@targets$species %in% species)]
  }
  if (is.null(space)) {
    rows <- rows[which(x@targets$target[rows] > 0)]
  } else{
    if (!all(space %in% x@targets$target))
      stop("space not present in argument to x")
    rows <- rows[which(x@targets$target[rows] %in% space)]
  }
  # assign new targets
  x@targets$proportion[rows] <- value
  # check of validity
  methods::validObject(x, test = FALSE)
  return(x)
}

#' @rdname names
#'
#' @export
`names<-.RapData` <- function(x, value) {
  assertthat::assert_that(is.character(value), all(!is.na(value)),
                          length(value) == nrow(x@species))
  # change names
  x@species$names <- value
  # check of validity
  methods::validObject(x, test = FALSE)
  return(x)
}

#' @rdname names
#' @export
names.RapData <- function(x) {
  return(x@species$names)
}
