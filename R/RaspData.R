#' @include RcppExports.R raspr-internal.R generics.R DemandPoints.R calcBoundaryData.R calcSpeciesAverageInPus.R
NULL

#' RaspData: An S4 class to represent RASP input data
#'
#' This class is used to store Marxan input data.
#'
#' @slot polygons \code{PolySet} planning unit spatial data or \code{NULL} if data not available.
#' @slot pu \code{data.frame} planning unit data. Columns are 'id' (\code{integer}), 'cost' (\code{numeric}), 'area' (\code{numeric}), and 'status' (\code{integer}).
#' @slot pu.points \code{list} of \code{matrix} objects with coordinates of the planning units in the attribute spaces.
#' @slot species \code{data.frame} with species data. Columns are 'id' (\code{integer}), 'area.target' (\code{numeric}), 'space.target' (\code{numeric}), and 'name' (\code{character}, optional).
#' @slot demand.points \code{list} of \code{DemandPoint} objects with the demand points.
#' @slot pu.species.probabilities \code{data.frame} with data on the probability of species in each planning unit. Columns are 'species' (\code{integer}), 'pu' (\code{integer}), and 'value' (\code{numeric}) columns.
#' @slot boundary \code{data.frame} with data on the shared boundary length of planning units. Columns are with 'id1' (\code{integer}), 'id2' (\code{integer}), and 'boundary' (\code{integer}).
#' @slot skipchecks \code{logical} Skip data integrity checks? May improve speed for big data sets.
#' @slot .cache \code{environment} used to cache calculations.
#' @seealso \code{\link[PBSmapping]{PolySet}}.
#' @export
setClass("RaspData",
	representation(
		polygons="PolySetOrNULL",
		pu="data.frame",
		pu.points="list",
		species="data.frame",
		demand.points="list",
		pu.species.probabilities="data.frame",
		boundary="data.frameOrNULL",
		skipchecks="logical",
		.cache="environment"
	),
	validity=function(object) {
		if (!object@skipchecks) {
			### check column names of inputs		
			# pu
			if (any(!c("id","cost","area","status") %in% names(object@pu)))
				stop("argument to pu is missing one of these columns: 'id', 'cost', 'area', or 'status'")

			if (!inherits(object@pu$id, 'integer'))
				stop('argument to pu$id is not integer')
			if (any(!is.finite(object@pu$id)))
				stop('argument to pu$id contains NA or non-finite values')
			if (anyDuplicated(object@pu$id))
				stop('argument to pu$id most not contain duplicates')
							
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

			# pu.points
			if (!all(laply(object@pu.points, inherits, 'matrix')))
				stop('All elements in argument to pu.points must be matrix objects')
			if (!all(laply(object@pu.points, is.numeric)))
				stop('All elements in argument to pu.points must be numeric')
			if (!all(laply(object@pu.points, is.finite)))
				stop('argument to pu.points contains NA or non-finite values')
				
				
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
			
			# species
			if (any(!c('id','area.target', 'space.target') %in% names(object@species)))
				stop("argument to species is missing one of these columns: 'id',  'area.target', or 'space.target'")
			
			if (!inherits(object@species$id, 'integer'))
				stop('argument to species$id is not integer')
			if (any(!is.finite(object@species$id)))
				stop('argument to species$id contains NA or non-finite values')
			if (anyDuplicated(object@species$id))
				stop('argument to species$id most not contain duplicates')
				
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
			
			# demand.point
				# all integrity checks are contained in the class
				
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
			if (!all(object@boundary$id1 %in% object@pu$id))
				stop('argument to boundary$id1 and pu$id must share the same values')
			if (!all(object@boundary$id2 %in% object@pu$id))
				stop('argument to boundary$id2 and pu$id must share the same values')			
			if (!all(object@pu.species.probabilities$pu %in% object@pu$id))
				stop('argument to pu.species.probabilities$pu and pu$id must share the same values')
			if (!all(laply(object@pu.points, function(x) all(rownames(x) %in% object@pu$id))))
				stop('argument to pu.points must have elements with rownames in pu$id')
			
			# check all species match 
			if (!all(object@pu.species.probabilities$species %in% object@species$id))
				stop('argument to pu.species.probabilities$species and species$id must share the same values')
			if (!all(laply(object@demand.points, function(x) all(x@species %in% object@species$id))))
				stop('demand.points@species must share same values as species$id')

			# check attribute spaces match in dimensions
			if (length(object@demand.points)!=length(object@pu.points))
				stop('arguments to demand.points and pu.points must  have the same number of elements')
			if (!all(laply(seq_along(object@demand.points), function(i) ncol(object@demand.points[[i]]@coords) ==  ncol(object@pu.points[[i]]))))
				stop('Each consecutive element in demand.points and pu.points must have the same number of dimensions')
		}
		return(TRUE)
	}
)

#' @export
setMethod(
	"initialize", 
	"RaspData", 
	function(.Object, pu, pu.points, species, demand.points, pu.species.probabilities, boundary, polygons, skipchecks, .cache=new.env()) {
		callNextMethod(.Object, polygons=polygons, pu=pu, pu.points=pu.points, species=species, demand.points=demand.points, pu.species.probabilities=pu.species.probabilities,skipchecks=skipchecks,boundary=boundary, .cache=.cache)
	}
)

#' Create new RaspData object
#'
#' This function creates a "RaspData" object using pre-processed data.
#'

#' @slot polygons \code{PolySet} planning unit spatial data or \code{NULL} if data not available.
#' @slot pu \code{data.frame} planning unit data. Columns are 'id' (\code{integer}), 'cost' (\code{numeric}), 'area' (\code{numeric}), and 'status' (\code{integer}).
#' @slot pu.points \code{list} of \code{matrix} objects with coordinates of the planning units in the attribute spaces.
#' @slot species \code{data.frame} with species data. Columns are 'id' (\code{integer}), 'area.target' (\code{numeric}), 'space.target' (\code{numeric}), and 'name' (\code{character}, optional).
#' @slot demand.points \code{list} of \code{DemandPoint} objects with the demand points.
#' @slot pu.species.probabilities \code{data.frame} with data on the probability of species in each planning unit. Columns are 'species' (\code{integer}), 'pu' (\code{integer}), and 'probability' (\code{numeric}) columns.
#' @slot boundary \code{data.frame} with data on the shared boundary length of planning units. Columns are with 'id1' (\code{integer}), 'id2' (\code{integer}), and 'boundary' (\code{integer}).
#' @slot skipchecks \code{logical} Skip data integrity checks? May improve speed for big data sets.
#' @slot .cache \code{environment} used to cache calculations.
#' @note Generally, users are not encouraged to change arguments to \code{.cache}.
#' @return RaspData object
#' @seealso \code{\link[PBSmapping]{PolySet}}, \code{\link[sp]{SpatialPoints}}, \code{\link[sp]{SpatialPointsDataFrame}}, \code{\link{format.RaspData}}, \code{\link{RaspData-class}}.
#' @export
RaspData<-function(pu, pu.species.probabilities, pu.points, species, demand.points, boundary, polygons=NULL, skipchecks=FALSE, .cache=new.env(), ...) {
	# convert factors to characters
	if (inherits(species$name, "factor"))
		species$name<-as.character(species$name)
	# make object
	rd<-new("RaspData", polygons=polygons, pu=pu, pu.points=pu.points, species=species, demand.points=demand.points, pu.species.probabilities=pu.species.probabilities, skipchecks=skipchecks,boundary=boundary, .cache=.cache)
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
make.RaspData<-function(pus, species, spaces,
	area.targets=0.2, space.targets=0.2, n.demand.points=1000L, kernel.method=c('sm.density', 'hyperbox')[1], quantile=0.2,
	species.points=NULL, n.species.points=ceiling(0.2*cellStats(species, 'sum')), include.geographic.space=TRUE, verbose=FALSE, ...) {
	
	## init
	# check inputs for validity
	stopifnot(inherits(species.points, c('SpatialPoints', 'SpatialPointsDataFrame', 'NULL')))
	stopifnot(inherits(pus, c('SpatialPolygons')))
	stopifnot(inherits(species, c('RasterStack')))
	stopifnot(inherits(spaces, c('RasterStack', 'RasterLayer', 'list')))
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
	validNames<-c('id','area','cost','status', 'area')
	if (inherits(pus, "SpatialPolygonsDataFrame") & all(c('id','cost','status', 'area') %in% names(pus))) {
		pu<-pus@data[,validNames,drop=FALSE]
	} else {
		warning("argument to pus does not have 'id', 'cost', and 'status' columns, creating default with all costs=1 and status=0")
		pu<-data.frame(id=seq_along(pus@polygons), cost=1, status=0L)
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
					extract(x, pus, fun=mean)
				}
			)
		)
	}
	# calculate positions in geographic space
	if (include.geographic.space) {
		pu.points=append(
			pu.points,
			list(gCentroid(pus, byid=TRUE)@coords)
		)
	}
	if (length(pu.points)==0) {
		stop('Attribute spaces must be specified. Either include.geographic.space=TRUE or spaces must contain at least one Raster object')
	}
	## set demand.points
	if (include.geographic.space)
		spaces=append(spaces, list(NULL))
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
				quantile=quantile,
				...
			)
		}
		demand.points[[i]]=merge.DemandPoints(dpLST)
	}
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
		id=seq_along(names(species)),
		area.target=area.targets,
		space.target=space.targets,
		name=names(species),
		stringsAsFactors=FALSE
	)	
	return(RaspData(pu=pu, pu.points=pu.points, species=species, demand.points=demand.points, pu.species.probabilities=pu.species.probabilities, boundary=boundary, polygons=geoPolygons, .cache=.cache))
}


#' @export
print.RaspData<-function(x, header=TRUE) {
	if (header)
		cat("RaspData object.\n")
	cat("Number of planning units:",nrow(x@pu),"\n")
	cat("Number of species:",nrow(x@species),"\n")
	cat("Number of attribute spaces:",length(x@demand.points),"\n")
}

#' @export
setMethod(
	'show',
	signature(object='RaspData'),
	function(object)
		print.RaspData(object)
)

