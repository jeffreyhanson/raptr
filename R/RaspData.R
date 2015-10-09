#' @include RcppExports.R raspr-internal.R generics.R DemandPoints.R calcBoundaryData.R calcSpeciesAverageInPus.R
NULL

#' RaspData: An S4 class to represent RASP input data
#'
#' This class is used to store Marxan input data.
#'
#' @slot polygons \code{PolySet} planning unit spatial data or \code{NULL} if data not available.
#' @slot pu \code{data.frame} planning unit data. Columns are 'cost' (\code{numeric}), 'area' (\code{numeric}), and 'status' (\code{integer}).
#' @slot species \code{data.frame} with species data. Columns are 'area.target' (\code{numeric}), 'space.target' (\code{numeric}), and 'name' (\code{character}, optional).
#' @slot pu.species.probabilities \code{data.frame} with data on the probability of species in each planning unit. Columns are 'species' (\code{integer}), 'pu' (\code{integer}), and 'value' (\code{numeric}) columns.
#' @slot attribute.spaces \code{list} of \code{AttributeSpace} objects with the demand points and planning unit coordinates.
#' @slot boundary \code{data.frame} with data on the shared boundary length of planning units. Columns are with 'id1' (\code{integer}), 'id2' (\code{integer}), and 'boundary' (\code{integer}).
#' @slot skipchecks \code{logical} Skip data integrity checks? May improve speed for big data sets.
#' @slot .cache \code{environment} used to cache calculations.
#' @seealso \code{\link[PBSmapping]{PolySet}}.
#' @export
setClass("RaspData",
	representation(
		polygons="PolySetOrNULL",
		pu="data.frame",
		species="data.frame",
		pu.species.probabilities="data.frame",
		attribute.spaces="list",
		boundary="data.frameOrNULL",
		skipchecks="logical",
		.cache="environment"
	),
	validity=function(object) {
		if (!object@skipchecks) {
			assign('object', object, envir=globalenv())
		
			### check column names of inputs
			# pu
			if (any(!c("cost","area","status") %in% names(object@pu)))
				stop("argument to pu is missing one of these columns: 'cost', 'area', or 'status'")
							
			if (!inherits(object@pu$cost, 'numeric'))
				stop('argument to pu$cost is not numeric')
			if (any(!is.finite(object@pu$cost)))
				stop('argument to pu$cost contains NA or non-finite values')

			if (!inherits(object@pu$area, 'numeric'))
				stop('argument to pu$area is not numeric')
			if (any(!is.finite(object@pu$area)))
				stop('argument to pu$cost contains NA or non-finite values')

			if (!inherits(object@pu$status, 'integer'))
				stop('argument to pu$status is not integer')
			if (any(!is.finite(object@pu$status)))
				stop('argument to pu$status contains NA or non-finite values')
			if (any(!object@pu$status %in% 0L:3L))
				stop('argument to pu$status must not contain values other than 0L, 1L, 2L, 3L')

			if (!all(laply(object@pu[,grep('^coords.*$', names(object@pu)),drop=FALSE], is.numeric)))
				stop('argument to pu has columns starting with "coords" that are not numeric')

			# species
			if (any(!c('area.target', 'space.target') %in% names(object@species)))
				stop("argument to species is missing one of these columns: 'area.target', or 'space.target'")
			
			if (!inherits(object@species$area.target, c('numeric')))
				stop('argument to species$area.target is not numeric or character')
			if (any(is.na(object@species$area.target)))
				stop('argument to species$area.target contains NA or non-finite values')
			if (any(object@species$area.target<0 | object@species$area.target>1))
				stop('argument to species$area.target contains values >1 or <0')
				
			if (!inherits(object@species$space.target, c('numeric')))
				stop('argument to species$space.target is not numeric or character')
			if (any(is.na(object@species$space.target)))
				stop('argument to species$space.target contains NA or non-finite values')
			if (any(object@species$space.target<0 | object@species$space.target>1))
				stop('argument to species$space.target contains values >1 or <0')

			if (!is.null(object@species$name)) {
				object@species$name<-gsub("[[:punct:]]", "", object@species$name)
				if (is.factor(object@species$name))
					object@species$name<-as.character(object@species$name)
				if (!inherits(object@species$name, 'character'))
					stop('argument to species$name is not character')
				if (any(is.na(object@species$name)))
					stop('argument to species$name contains NA values')
			}
				
			# pu.species.probabilities
			if (any(!c('species','pu','value') %in% names(object@pu.species.probabilities)))
				stop("argument to pu.species.probabilities is missing one of these columns: 'species', 'pu', or 'value'")
			if (!inherits(object@pu.species.probabilities$pu, 'integer'))
				stop('argument to pu.species.probabilities$pu is not integer')
			if (!inherits(object@pu.species.probabilities$species, 'integer'))
				stop('argument to pu.species.probabilities$species is not integer')
			if (any(!is.finite(object@pu.species.probabilities$value)))
				stop('argument to pu.species.probabilities$value contains NA or non-finite values')
			if (any(object@pu.species.probabilities$value<0 | object@pu.species.probabilities$value>1))
				stop('argument to pu.species.probabilities$value contains values >1 or <0')
			
			# attribute.space
				# all validity checks are internal in the object
			
			# boundary
			if (!is.null(object@boundary)) {
				if (any(!c('id1','id2','boundary') %in% names(object@boundary)))
					stop("argument to boundary is missing one of these columns: 'id1', 'id2', or 'boundary'")
				if (!inherits(object@boundary$id1, 'integer'))
					stop('argument to boundary$id1 is not integer')
				if (any(!is.finite(object@boundary$id1)))
					stop('argument to boundary$id1 contains NA or non-finite values')

				if (!inherits(object@boundary$id2, 'integer'))
					stop('argument to boundary$id2 is not integer')
				if (any(!is.finite(object@boundary$id2)))
					stop('argument to boundary$id2 contains NA or non-finite values')

				if (!inherits(object@boundary$boundary, 'numeric'))
					stop('argument to boundary$boundary is not numeric')
				if (any(!is.finite(object@boundary$boundary)))
					stop('argument to boundary$boundary contains NA or non-finite values')
			}
			
			## cross table dependencies
			# check all planning units match
			if (!all(object@boundary$id1 %in% seq_len(nrow(object@pu))))
				stop('argument to boundary$id1 must have values that correspond to rows in argument to pu')
			if (!all(object@boundary$id2 %in% seq_len(nrow(object@pu))))
				stop('argument to boundary$id2 must have values that correspond to rows in argument to pu')
			if (!all(object@pu.species.probabilities$pu %in% seq_len(nrow(object@pu))))
				stop('argument to pu.species.probabilities$pu must have values that correspond to rows in argument to pu')
			if (!all(laply(object@attribute.spaces, function(x) {nrow(x@pu@coords)==nrow(object@pu)})))
				stop('arguments to attribute.space and pu must have the same number of planning units')
			
			# check all species match 
			if (!all(object@pu.species.probabilities$species %in% seq_len(nrow(object@species)))) 
				stop('argument to pu.species.probabilities$species must have values that correspond to rows in argument to species')
			if (!all(laply(object@attribute.spaces, function(x) {length(x@dp)==nrow(object@species)})))
				stop('arguments to attribute.space and species must have the same number of species')
		} 
		return(TRUE)
	}
)

#' @export
setMethod(
	"initialize", 
	"RaspData", 
	function(.Object, pu, species, pu.species.probabilities, attribute.spaces, boundary, polygons, skipchecks, .cache=new.env()) {
		# remove extra columns
		pu<-pu[,which(names(pu) %in% c('cost', 'area', 'status')),drop=FALSE]
		species<-species[,which(names(species) %in% c('area.target', 'space.target', 'name')),drop=FALSE]
		pu.species.probabilities<-pu.species.probabilities[,which(names(pu.species.probabilities) %in% c('pu', 'species', 'value')),drop=FALSE]
		boundary<-boundary[,which(names(boundary) %in% c('id1', 'id2', 'boundary')),drop=FALSE]
		# create object
		callNextMethod(.Object, polygons=polygons, pu=pu, species=species, pu.species.probabilities=pu.species.probabilities, attribute.spaces=attribute.spaces, skipchecks=skipchecks,boundary=boundary, .cache=.cache)
	}
)

#' Create new RaspData object
#'
#' This function creates a "RaspData" object using pre-processed data.
#'
#' @param polygons \code{PolySet} planning unit spatial data or \code{NULL} if data not available.
#' @param pu \code{data.frame} planning unit data. Columns are 'cost' (\code{numeric}), 'area' (\code{numeric}), and 'status' (\code{integer}).
#' @param species \code{data.frame} with species data. Columns are 'area.target' (\code{numeric}), 'space.target' (\code{numeric}), and 'name' (\code{character}, optional).
#' @param pu.species.probabilities \code{data.frame} with data on the probability of species in each planning unit. Columns are 'species' (\code{integer}), 'pu' (\code{integer}), and 'value' (\code{numeric}) columns.
#' @param attribute.spaces \code{list} of \code{AttributeSpace} objects with the demand points and planning unit coordinates.
#' @param boundary \code{data.frame} with data on the shared boundary length of planning units. Columns are with 'id1' (\code{integer}), 'id2' (\code{integer}), and 'boundary' (\code{integer}).
#' @param skipchecks \code{logical} Skip data integrity checks? May improve speed for big data sets.
#' @param .cache \code{environment} used to cache calculations.
#' @note Generally, users are not encouraged to change arguments to \code{.cache}.
#' @return RaspData object
#' @seealso \code{\link[PBSmapping]{PolySet}}, \code{\link[sp]{SpatialPoints}}, \code{\link[sp]{SpatialPointsDataFrame}}, \code{\link{format.RaspData}}, \code{\link{RaspData-class}}.
#' @export
RaspData<-function(pu, species, pu.species.probabilities, attribute.spaces, boundary, polygons=NULL, skipchecks=FALSE, .cache=new.env(), ...) {
	# convert factors to characters
	if (inherits(species$name, "factor"))
		species$name<-as.character(species$name)
	# make object
	rd<-new("RaspData", polygons=polygons, pu=pu, species=species, pu.species.probabilities=pu.species.probabilities, attribute.spaces=attribute.spaces, skipchecks=skipchecks,boundary=boundary, .cache=.cache)
	# test for validity
	validObject(rd, test=FALSE)
	return(rd)
}

#' Make data for RASP using minimal inputs
#'
#' This function prepares spatially explicit planning unit, species data, and landscape data layers for RASP processing.
#'
#' @usage make.RaspData(pus, species.points, species.rasters, space.rasters, area.targets = 0.2,
#' space.targets = 0.2,  demand.points=1000, kernel.method='guassian', include.geographic.space=TRUE,
#' pu = NULL, species = NULL, species.coords = NULL, pu.species.probabilities = NULL,
#' boundary = NULL, ..., verbose = FALSE)
#' @param pus \code{SpatialPolygons} with planning unit data.
#' @param species \code{RasterLayer}, \code{RasterStack}, \code{RasterBrick} with species probability distribution data.
#' @param spaces \code{list} of/or \code{RasterLayer}, \code{RasterStack}, \code{RasterBrick} representing projects of attribute space over geographic space. Use a \code{list} to denote seperate attribute spaces.
#' @param area.targets \code{numeric} vector for area targets (%) for each species. Defaults to 0.2 for each attribute space for each species. 
#' @param space.targets \code{numeric} vector for attribute space targets (%) for each species. Defaults to 0.2 for each attribute space for each species. Note all attribute spaces have the same targets.
#' @param n.demand.points \code{integer} number of demand points to use for each attribute space for each species.
#' @param kernel.method \code{character} name of kernel method to use to generate demand points. Use either \code{'sm.density'} or \code{'hypervolume'}.
#' @param quantile \code{numeric} quantile to generate demand points within. If 0 then demand points are generated across the full range of values the \code{species.points} intersect. Defaults to 0.2. 
#' @param include.geographic.space \code{logical} should the geographic space be considered an attribute space?
#' @param species.points \code{list} of/or \code{SpatialPointsDataFrame} or \code{SpatialPoints} with species presence records. Use a \code{list} of objects to represent different species. Must have the same number of elements as \code{species.rasters}. If not supplied then use \code{n.species.points} to sample points from the species distributions.
#  @param n.species.points \code{numeric} vector specfiying the number points to sample the species distributions to use to generate demand points. Defaults to 20% of the distribution.
#' @param ... additional arguments to \code{calcBoundaryData} and \code{calcPuVsSpeciesData}.
#' @param verbose \code{logical} print statements during processing?
#' @seealso \code{\link{RaspData-class}}, \code{\link{RaspData}}.
#' @export
#' @examples
#' data(pus, species, space)
#' x<-RaspData(pus, species, space)
make.RaspData<-function(pus, species, spaces=NULL,
	area.targets=0.2, space.targets=0.2, n.demand.points=1000L, kernel.method=c('sm.density', 'hyperbox')[1], quantile=0.2,
	species.points=NULL, n.species.points=ceiling(0.2*cellStats(species, 'sum')), include.geographic.space=TRUE, verbose=FALSE, ...) {
	
	## init
	# check inputs for validity
	stopifnot(inherits(species.points, c('SpatialPoints', 'SpatialPointsDataFrame', 'NULL')))
	stopifnot(inherits(pus, c('SpatialPolygons')))
	stopifnot(inherits(species, c('RasterStack', 'RasterLayer')))
	stopifnot(inherits(spaces, c('NULL', 'RasterStack', 'RasterLayer', 'list')))
	.cache<-new.env()
	
	# coerce non-list items to list
	if (!inherits(spaces, 'list'))
		spaces=list(spaces)

	# create species.points from species
	if (is.null(species.points)) {
		species.points=llply(
			seq_len(nlayers(species)),
			function(i) {
				SpatialPoints(coords=randomPoints(species[[i]], n=n.species.points[[i]]), proj4string=species[[i]]@crs)
			}
		)
	} else {
		if (!inherits(species.points, 'list')) {
			if (inherits(species.points, 'SpatialDataFrame')) {
				if ('id' %in% names(species.points@data)) {
					species.points=llply(
						sort(unique(species@data$id)),
						function(x) {
							return(species.points[which(species.points$id==x),])
						}
					)
				} else {
					species.points=list(SpatialPoints(species.points))
				}
			} else {
				species.points=list(species.points)
			}
		} 
	}
	
	# set polygons
	geoPolygons<-pus
	if (!identical(geoPolygons, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))) {
		if (verbose)
			cat('Projecting polygons to WGS1984 for rendering.\n')
		geoPolygons<-spTransform(geoPolygons, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
	}
	geoPolygons<-rcpp_Polygons2PolySet(geoPolygons@polygons)
	## set pu
	validNames<-c('cost','status', 'area')
	if (inherits(pus, "SpatialPolygonsDataFrame") & all(c('cost','status', 'area') %in% names(pus))) {
		pu<-pus@data[,validNames,drop=FALSE]
	} else {
		warning("argument to pus does not have 'cost', and 'status' columns, creating default with all costs=1 and status=0")
		pu<-data.frame(cost=rep(1, length(pus@polygons)), status=rep(0L, length(pus@polygons)))
	} 
	if (!'area' %in% names(pu)) {
		if (identical(pus@proj4string, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')))
			warning('Planning unit areas are being calculated in a geographic coordinate system')
		pu$area=gArea(pus,byid=TRUE)
	}
	
	## set pu.points
	# set pu.points based spaces
	pu.points=list()
	# if spaces is not NULL
	if (!is.null(spaces[[1]])) {
		pu.points=append(
			pu.points,
			llply(
				spaces,
				.fun=function(x) {
					SimplePoints(coords=extract(x, pus, fun=mean))
				}
			)
		)
	}
	# calculate positions in geographic space
	if (include.geographic.space) {
		pu.points=append(
			pu.points,
			list(SimplePoints(gCentroid(pus, byid=TRUE)@coords))
		)
	}
	if (length(pu.points)==0) {
		stop('Attribute spaces must be specified. Either include.geographic.space=TRUE or spaces must contain at least one Raster object')
	}
	## set demand.points
	# include geographic space if set
	if (!is.null(spaces[[1]]) & include.geographic.space)
		spaces=append(spaces, list(NULL))
	# generate demand points
	demand.points=list()
	for (i in seq_along(spaces)) {
		dpLST=list()
		for (j in seq_along(species.points)) {
			dpLST[[j]]=make.DemandPoints(
				species.points[[j]],
				space.rasters=spaces[[i]],
				kernel.method=kernel.method,
				n=n.demand.points,
				id=j,
				quantile=quantile
# 				...
			)
		}
		demand.points[[i]]=dpLST
	}	
	# create AttributeSpace objects
	attribute.spaces=llply(seq_along(spaces), function(i) {
		return(
			AttributeSpace(
				pu=pu.points[[i]],
				dp=demand.points[[i]]
			)
		)
	})
	## set boundary
	if (identical(pus@proj4string, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')))
		warning("creating boundary length data from pus in WGS1984; consider supplying in an object a projected CRS.")
	if (verbose)
		cat("Calculating boundary data.")
	boundary<-calcBoundaryData(rcpp_Polygons2PolySet(pus@polygons), ...)
	## set pu.species.probabilities
	projPolygons=pus 
	if (!identical(projPolygons@proj4string, species@crs)) {
		if (verbose)
			cat("Projecting polygons to rasters' CRS.")
		projPolygons<-spTransform(projPolygons, species@crs)
	}
	if (verbose)
		cat("Calculating average species probability in planning units.")
	pu.species.probabilities<-calcSpeciesAverageInPus(projPolygons, species, ...)
	## set species
	species<-data.frame(
		area.target=area.targets,
		space.target=space.targets,
		name=names(species),
		stringsAsFactors=FALSE
	)	
	return(RaspData(pu=pu, species=species, pu.species.probabilities=pu.species.probabilities, attribute.spaces=attribute.spaces, boundary=boundary, polygons=geoPolygons, .cache=.cache))
}

#' @rdname basemap
#' @export
basemap.RaspData<-function(x, basemap="hybrid", grayscale=FALSE, force_reset=FALSE) {
	callchar<-hashCall(match.call(), 1)
	match.arg(basemap, c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid"))	
	if (is.null(x@polygons))
	stop("Rasp object is not associated with spatially explicit data for the planning units.")
	# fetch data from google or cache
	if (force_reset || !is.cached(x, callchar)) {
		cache(x, callchar, GetMap.bbox(range(x@polygons[["X"]]), range(x@polygons[["Y"]]), destfile=paste0(tempfile(),'.png'), maptype=basemap, GRAYSCALE=grayscale))
	}
	return(cache(x, callchar))
}

#' @export
print.RaspData<-function(x, header=TRUE) {
	if (header)
		cat("RaspData object.\n")
	cat("  Number of planning units:",nrow(x@pu),"\n")
	cat("  Number of species:",nrow(x@species),"\n")
	cat("  Number of attribute spaces:",length(x@attribute.spaces),"\n")
}

#' @export
setMethod(
	'show',
	signature(object='RaspData'),
	function(object)
		print.RaspData(object)
)

#' @describeIn is.cached
setMethod(
	f="is.cached", 
	signature(x="RaspData", name="character"), 
	function(x,name) {
		return(!is.null(x@.cache[[name]]))
	}
)

#' @describeIn cache
setMethod(
	f="cache", 
	signature(x="RaspData", name="character", y="ANY"), 
	function(x, name, y) {
		x@.cache[[name]]=y
	}
)

#' @describeIn cache
setMethod(
	f="cache", 
	signature(x="RaspData", name="character", y="missing"), 
	function(x, name, y) {
		return(x@.cache[[name]])
	}
)

#' @rdname is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="RaspData", y="RaspData"),
	function(x,y) {
		return(
			identical(nrow(x@pu), nrow(y@pu)) &
			identical(nrow(x@species), nrow(y@species)) &
			identical(x@polygons, y@polygons) &
			identical(x@boundary$id1, y@boundary$id1) &
			identical(x@boundary$id2, y@boundary$id2)
		)
	}
)