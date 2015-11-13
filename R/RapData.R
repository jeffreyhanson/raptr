#' @include RcppExports.R rapr-internal.R generics.R DemandPoints.R calcBoundaryData.R calcSpeciesAverageInPus.R
NULL

#' RapData: An S4 class to represent RASP input data
#'
#' This class is used to store RASP input data.
#'
#' @slot polygons \code{PolySet} planning unit spatial data or \code{NULL} if data not available.
#' @slot pu \code{data.frame} planning unit data. Columns are 'cost' (\code{numeric}), 'area' (\code{numeric}), and 'status' (\code{integer}).
#' @slot species \code{data.frame} with species data. Columns are 'name' (\code{character}.
#' @slot targets \code{data.frame} with species data. Columns are 'species' (\code{integer}), 'target' (\code{integer}), and 'proportion' (\code{character}).
#' @slot pu.species.probabilities \code{data.frame} with data on the probability of species in each planning unit. Columns are 'species' (\code{integer}), 'pu' (\code{integer}), and 'value' (\code{numeric}) columns.
#' @slot attribute.spaces \code{list} of \code{AttributeSpace} objects with the demand points and planning unit coordinates.
#' @slot boundary \code{data.frame} with data on the shared boundary length of planning units. Columns are with 'id1' (\code{integer}), 'id2' (\code{integer}), and 'boundary' (\code{numeric}).
#' @slot skipchecks \code{logical} Skip data integrity checks? May improve speed for big data sets.
#' @slot .cache \code{environment} used to cache calculations.
#' @seealso \code{\link[PBSmapping]{PolySet}}.
#' @export
setClass("RapData",
	representation(
		polygons="PolySet",
		pu="data.frame",
		species="data.frame",
		targets="data.frame",
		pu.species.probabilities="data.frame",
		attribute.spaces="list",
		boundary="data.frame",
		skipchecks="logical",
		.cache="environment"
	),
	validity=function(object) {
		if (!object@skipchecks) {
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
			if (!is.null(object@species$name)) {
				object@species$name<-gsub("[[:punct:]]", "", object@species$name)
				if (is.factor(object@species$name))
					object@species$name<-as.character(object@species$name)
				if (!inherits(object@species$name, 'character'))
					stop('argument to species$name is not character')
				if (any(is.na(object@species$name)))
					stop('argument to species$name contains NA values')
			}

			# targets
			if (any(!c('species','target','proportion') %in% names(object@targets)))
				stop("argument to targets is missing one of these columns: 'species', 'target', or 'proportion'")

			if (!inherits(object@targets$species, c('integer')))
				stop('argument to targets$species is not integer')
			if (any(is.na(object@targets$species)))
				stop('argument to targets$species contains NA or non-finite values')

			if (!inherits(object@targets$target, c('integer')))
				stop('argument to targets$target is not integer')
			if (any(is.na(object@targets$target)))
				stop('argument to targets$target contains NA or non-finite values')

			if (!inherits(object@targets$proportion, c('numeric')))
				stop('argument to targets$proportion is not numeric')
			if (any(is.na(object@targets$proportion)))
				stop('argument to targets$proportion contains NA or non-finite values')
			if (any(object@targets$proportion<0 | object@targets$proportion>1))
				stop('argument to targets$proportion contains values >1 or <0')

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
			if (!all(seq_len(nrow(object@species)) %in% object@pu.species.probabilities$species))
				stop('argument to species has species that do not occur at least once in pu.species.probabilities$species')
			if (!all(laply(object@attribute.spaces, function(x) {length(x@dp)==nrow(object@species)})))
				stop('arguments to attribute.space and species must have the same number of species')
			if (!all(object@targets$species %in% seq_len(nrow(object@species))))
				stop('arguments to targets must have species present in argument to species')
			# check that attribute spaces match
			if (!all(object@targets$target %in% 0:length(object@attribute.spaces)))
				stop('argument to targets must have values in values that are zero or correspond to elements in argument to attribute.spaces')
		}
		return(TRUE)
	}
)


#' Create new RapData object
#'
#' This function creates a "RapData" object using pre-processed data.
#'
#' @param polygons \code{PolySet} planning unit spatial data or \code{NULL} if data not available.
#' @param pu \code{data.frame} planning unit data. Columns are 'cost' (\code{numeric}), 'area' (\code{numeric}), and 'status' (\code{integer}).
#' @param species \code{data.frame} with species data. Columns are 'name' (\code{character}).
#' @param targets \code{data.frame} with species data. Columns are 'species' (\code{integer}), 'target' (\code{integer}), and 'proportion' (\code{character}).
#' @param pu.species.probabilities \code{data.frame} with data on the probability of species in each planning unit. Columns are 'species' (\code{integer}), 'pu' (\code{integer}), and 'value' (\code{numeric}) columns.
#' @param attribute.spaces \code{list} of \code{AttributeSpace} objects with the demand points and planning unit coordinates.
#' @param boundary \code{data.frame} with data on the shared boundary length of planning units. Columns are with 'id1' (\code{integer}), 'id2' (\code{integer}), and 'boundary' (\code{integer}).
#' @param skipchecks \code{logical} Skip data integrity checks? May improve speed for big data sets.
#' @param .cache \code{environment} used to cache calculations.
#' @note Generally, users are not encouraged to change arguments to \code{.cache}.
#' @return RapData object
#' @seealso \code{\link[PBSmapping]{PolySet}}, \code{\link[sp]{SpatialPoints}}, \code{\link[sp]{SpatialPointsDataFrame}}, \code{\link{make.RapData}}, \code{\link{RapData-class}}.
#' @export
#' @examples
#' \dontrun{
#' # load data
#' data(cs_pus, cs_spp, cs_space)
#' # create data for RapData object
#' attribute.spaces=list(
#' 	AttributeSpace(
#' 		pu=SimplePoints(rgeos::gCentroid(cs_pus[1:10,], byid=TRUE)@@coords),
#' 		dp=list(
#'			make.DemandPoints(
#'				SpatialPoints(
#'					coords=randomPoints(
#'						cs_spp,
#'						n=10,
#'						prob=TRUE
#'					)
#'				),
#'				NULL
#'			)
#'		)
#' 	),
#' 	AttributeSpace(
#' 		pu=SimplePoints(extract(cs_space[[1]],cs_pus[1:10,],fun=mean)),
#' 		dp=list(
#'			make.DemandPoints(
#'				SpatialPoints(
#'					coords=randomPoints(
#'						cs_spp,
#'						n=10,
#'						prob=TRUE
#'					)
#'				),
#'				cs_space[[1]]
#'			)
#'		)
#' 	)
#' )
#' pu.species.probabilities=calcSpeciesAverageInPus(cs_pus[1:10,], cs_spp)
#' polygons=SpatialPolygons2PolySet(cs_pus[1:10,])
#' boundary=calcBoundaryData(cs_pus[1:10,])
#'
# # create RapData object
#' x<-RapData(
#' 	pu=cs_pus@@data[1:10,],
#' 	species=data.frame(name='test'),
#'  target=data.frame(species=1, target=0:2, proportion=0.2),
#' 	pu.species.probabilities=pu.species.probabilities,
#' 	attribute.spaces=attribute.spaces,
#' 	polygons=polygons,
#' 	boundary=boundary
#' )
#' print(x)
#' }
RapData<-function(pu, species, targets, pu.species.probabilities, attribute.spaces, boundary, polygons=NA, skipchecks=FALSE, .cache=new.env()) {
	# convert factors to characters
	if (inherits(species$name, "factor"))
		species$name<-as.character(species$name)
	# remove extra columns
	pu<-pu[,which(names(pu) %in% c('cost', 'area', 'status')),drop=FALSE]
	species<-species[,which(names(species) %in% c('name')),drop=FALSE]
	targets<-targets[,which(names(targets) %in% c('species','target','proportion')),drop=FALSE]
	pu.species.probabilities<-pu.species.probabilities[,which(names(pu.species.probabilities) %in% c('pu', 'species', 'value')),drop=FALSE]
	boundary<-boundary[,which(names(boundary) %in% c('id1', 'id2', 'boundary')),drop=FALSE]
	# make object
	rd<-new("RapData", polygons=polygons, pu=pu, species=species, targets=targets, pu.species.probabilities=pu.species.probabilities, attribute.spaces=attribute.spaces, skipchecks=skipchecks,boundary=boundary, .cache=.cache)
	# test for validity
	validObject(rd, test=FALSE)
	return(rd)
}

#' Make data for RASP using minimal inputs
#'
#' This function prepares spatially explicit planning unit, species data, and landscape data layers for RASP processing.
#'
#' @param pus \code{SpatialPolygons} with planning unit data.
#' @param species \code{RasterLayer}, \code{RasterStack}, \code{RasterBrick} with species probability distribution data.
#' @param spaces \code{list} of/or \code{RasterLayer}, \code{RasterStack}, \code{RasterBrick} representing projects of attribute space over geographic space. Use a \code{list} to denote seperate attribute spaces.
#' @param amount.target \code{numeric} vector for area targets (\%) for each species. Defaults to 0.2 for each attribute space for each species.
#' @param space.target \code{numeric} vector for attribute space targets (\%) for each species. Defaults to 0.2 for each attribute space for each species and each space.
#' @param n.demand.points \code{integer} number of demand points to use for each attribute space for each species. Defaults to 100L.
#' @param kernel.method \code{character} name of kernel method to use to generate demand points. Use either \code{ks} or \code{hypervolume}.
#' @param quantile \code{numeric} quantile to generate demand points within. If 0 then demand points are generated across the full range of values the \code{species.points} intersect. Defaults to 0.2.
#' @param scale \code{logical} should attribute spaces be z-scored before generating demand points? Defaults to \code{TRUE}.
#' @param include.geographic.space \code{logical} should the geographic space be considered an attribute space?
#' @param species.points \code{list} of/or \code{SpatialPointsDataFrame} or \code{SpatialPoints} with species presence records. Use a \code{list} of objects to represent different species. Must have the same number of elements as \code{species}. If not supplied then use \code{n.species.points} to sample points from the species distributions.
#' @param n.species.points \code{numeric} vector specfiying the number points to sample the species distributions to use to generate demand points. Defaults to 20\% of the distribution.
#' @param ... additional arguments to \code{calcBoundaryData} and \code{calcPuVsSpeciesData}.
#' @param verbose \code{logical} print statements during processing?
#' @seealso \code{\link{RapData-class}}, \code{\link{RapData}}.
#' @export make.RapData
#' @examples
#' # load data
#' data(cs_pus, cs_spp, cs_space)
#' # make RapData object using the 1st 10 planning units
#' x <- make.RapData(cs_pus[1:10,], cs_spp, cs_space, include.geographic.space=TRUE)
#' print(x)
make.RapData<-function(pus, species, spaces=NULL,
	amount.target=0.2, space.target=0.2, n.demand.points=100L, kernel.method=c('ks', 'hyperbox')[1], quantile=0.2, scale=TRUE,
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
	# z-score spaces
	meansLST=list()
	sdsLST=list()
	if (scale) {
		for (i in seq_along(spaces)) {
			if (inherits(spaces[[i]], 'Raster')) {
				# get means and sds
				meansLST[[i]]=cellStats(spaces[[i]], 'mean')
				sdsLST[[i]]=cellStats(spaces[[i]], 'sd')
				# if sdsLST contains zeros then the SD is 1 due to bug in cellStats package
				if (any(is.na(sdsLST[[i]])))
					sdsLST[[i]][is.na(sdsLST[[i]])]=1
				tmpLST=list()
				for (j in seq_along(meansLST[[i]])) {
					tmpLST[[j]]=(spaces[[i]][[j]] - meansLST[[i]][[j]]) / sdsLST[[i]][[j]]
				}
				spaces[[i]]=stack(tmpLST)
			}
		}
	}
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
	if (inherits(pus, "SpatialPolygonsDataFrame") & any(c('cost','status', 'area') %in% names(pus))) {
		pu<-pus@data[,intersect(validNames,names(pus@data)),drop=FALSE]
	} else {
		pu<-data.frame(x=rep(1, length(pus@polygons)))[,-1,drop=FALSE]
	}
	if (!'cost' %in% names(pu)) {
		pu$cost<-rep(1, length(pus@polygons))
		warning("argument to pus does not have a 'cost' column, creating default with all costs=1")
	}
	if (!'status' %in% names(pu)) {
		pu$status<-rep(0L, length(pus@polygons))
		warning("argument to pus does not have a 'status' column, creating default with all status=0L")
	}
	if (!'area' %in% names(pu)) {
		pu$area=gArea(pus,byid=TRUE)
		warning("argument to pus does not have a 'area' column, creating default using area of polygons")
		if (identical(pus@proj4string, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')))
			warning('Planning unit areas are being calculated in a geographic coordinate system')
	}
	#### Attribute space data
	## set pu.points
	# set pu.points based spaces
	pu.points<-list()
	# if spaces is not NULL
	if (!is.null(spaces[[1]])) {
		# create initial rasterized version of the pus
		pus@data$id<-seq_len(nrow(pus@data))
		pu.rast<-rasterize(pus, spaces[[1]][[1]], field='id')
		# extract means
		pu.points<-llply(
			spaces,
			.fun=function(x) {
				# generate new raster if needed
				if (
					xmin(pu.rast) != xmin(x) ||
					xmax(pu.rast) != xmax(x) ||
					ymin(pu.rast) != ymin(x) ||
					ymax(pu.rast) != ymax(x) ||
					res(pu.rast)[1] != res(x)[1] ||
					res(pu.rast)[2] != res(x)[2]
				) {
					pu.rast<-rasterize(pus, x)
				}
				# extract points
				coordMTX<-matrix(NA, nrow=nrow(pus@data), ncol=nlayers(x))
				for (i in seq_len(ncol(coordMTX))) {
					vals<-rcpp_groupmean(getValues(pu.rast), getValues(x[[i]]))
					coordMTX[attr(vals, 'ids'),i]<-c(vals)
				}
				if (any(is.na(coordMTX[])))
					stop('Some planning units do not intersect with an attribute space layer.')
				return(SimplePoints(coords=coordMTX))
			}
		)
	}
	# calculate positions in geographic space
	if (include.geographic.space) {
		pu_coords=gCentroid(pus, byid=TRUE)@coords
		if (scale) {
			pu_meansDBL=c()
			pu_sdsDBL=c()
			for (i in seq_len(ncol(pu_coords))) {
				pu_meansDBL=c(pu_meansDBL,mean(pu_coords[,i]))
				pu_sdsDBL=c(pu_sdsDBL,sd(pu_coords[,i]))
				pu_coords[,i] = (pu_coords[,i] - pu_meansDBL[i]) / pu_sdsDBL[i]
			}
		}
		pu.points=append(
			pu.points,
			list(SimplePoints(pu_coords))
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
			# extract space points
			if (is.null(spaces[[i]])) {
				space.points=species.points[[j]]@coords
				if (scale) {
					for (k in seq_len(ncol(space.points)))
						space.points[,k]=(species.points[[j]]@coords[,k] - pu_meansDBL[k]) / pu_sdsDBL[k]
				}
			} else {
				space.points=extract(spaces[[i]], species.points[[j]])
			}
			# generate demand points
			dpLST[[j]]=make.DemandPoints(
				points=space.points,
				kernel.method=kernel.method,
				n=n.demand.points,
				quantile=quantile
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
		cat("Calculating boundary data.\n")
	boundary<-calcBoundaryData(rcpp_Polygons2PolySet(pus@polygons), ...)
	## set pu.species.probabilities
	projPolygons=pus
	if (!identical(projPolygons@proj4string, species@crs)) {
		if (verbose)
			cat("Projecting polygons to rasters' CRS.\n")
		projPolygons<-spTransform(projPolygons, species@crs)
	}
	if (verbose)
		cat("Calculating average species probability in planning units.\n")
	pu.species.probabilities<-calcSpeciesAverageInPus(projPolygons, species, ...)
	## set species
	species<-data.frame(
		name=names(species),
		stringsAsFactors=FALSE
	)
	## set targets
	targets<-rbind(
		expand.grid(
			species=seq_len(nrow(species)),
			target=0L,
			proportion=amount.target
		),
		expand.grid(
			species=seq_len(nrow(species)),
			target=seq(1L, length(attribute.spaces)),
			proportion=space.target
		)
	)
	## return object
	return(RapData(pu=pu, species=species, targets=targets, pu.species.probabilities=pu.species.probabilities, attribute.spaces=attribute.spaces, boundary=boundary, polygons=geoPolygons, .cache=.cache))
}

#' @rdname basemap
#' @export
basemap.RapData<-function(x, basemap="hybrid", grayscale=FALSE, force.reset=FALSE) {
	callchar<-hashCall(match.call(), 1)
	match.arg(basemap, c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid"))
	if (is.null(x@polygons))
	stop("Rap object is not associated with spatially explicit data for the planning units.")
	# fetch data from google or cache
	if (force.reset || !is.cached(x, callchar)) {
		cache(x, callchar, GetMap.bbox(range(x@polygons[["X"]]), range(x@polygons[["Y"]]), destfile=paste0(tempfile(),'.png'), maptype=basemap, GRAYSCALE=grayscale))
	}
	return(cache(x, callchar))
}

#' @method print RapData
#' @rdname print
#' @export
print.RapData<-function(x, ..., header=TRUE) {
	if (header)
		cat("RapData object.\n")
	cat("  Number of planning units:",nrow(x@pu),"\n")
	cat("  Number of species:",nrow(x@species),"\n")
	cat("  Number of attribute spaces:",length(x@attribute.spaces),"\n")
}

#' @describeIn show
#' @export
setMethod(
	'show',
	signature(object='RapData'),
	function(object)
		print.RapData(object)
)

#' @describeIn is.cached
setMethod(
	f="is.cached",
	signature(x="RapData", name="character"),
	function(x,name) {
		return(!is.null(x@.cache[[name]]))
	}
)

#' @describeIn cache
setMethod(
	f="cache",
	signature(x="RapData", name="character", y="ANY"),
	function(x, name, y) {
		x@.cache[[name]]=y
	}
)

#' @describeIn cache
setMethod(
	f="cache",
	signature(x="RapData", name="character", y="missing"),
	function(x, name, y) {
		return(x@.cache[[name]])
	}
)

#' @rdname is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="RapData", y="RapData"),
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

#' @rdname spp.subset
#' @method spp.subset RapData
#' @export
spp.subset.RapData<-function(x, species) {
	# convert species names to integers
	if (inherits(species, 'character')) {
		if (!'name' %in% names(x@species))
			stop('argument to x does not have names for species.')
		species <- match(species, x@species$name)
		if (any(is.na(species)))
			stop('argument to species contains names not present in object.')
	}
	# create new objects
	pu.species.probabilities<-x@pu.species.probabilities[which(
			x@pu.species.probabilities$species %in% species
	),]
	pu.species.probabilities$species<-match(pu.species.probabilities$species, species)
	targets<-x@targets[which(x@targets$species %in% species),,drop=FALSE]
	targets$species<-match(targets$species, species)
	# return new object
	return(
		RapData(
			pu=x@pu,
			species=x@species[species,,drop=FALSE],
			targets=targets,
			pu.species.probabilities=pu.species.probabilities,
			attribute.spaces=lapply(
				x@attribute.spaces,
				function(x) {
						AttributeSpace(
							x@pu,
							x@dp[species]
					  )
				}
			),
			boundary=x@boundary,
			polygons=x@polygons
	  )
	)
}

#' @rdname pu.subset
#' @method pu.subset RapData
#' @export
pu.subset.RapData<-function(x, pu) {
	## create objects
	# pu.species.probabilities
	pu.species.probabilities<-x@pu.species.probabilities[which(
			x@pu.species.probabilities$pu %in% pu
	),]
	species<-unique(pu.species.probabilities$species)
	pu.species.probabilities$species<-match(pu.species.probabilities$species, species)
	pu.species.probabilities$pu<-match(pu.species.probabilities$pu, pu)
	# boundary
	boundary<-x@boundary[which(x@boundary$id1 %in% pu & x@boundary$id2 %in% pu),]
	boundary2<-x@boundary[which(x@boundary$id1 %in% pu & !x@boundary$id2 %in% pu),]
	boundary2$id2<-boundary2$id1
	boundary3<-x@boundary[which(!x@boundary$id1 %in% pu & x@boundary$id2 %in% pu),]
	boundary3$id1<-boundary3$id2
	boundary<-do.call(rbind, list(boundary, boundary2, boundary3))
	boundary$id1<-match(boundary$id1, pu)
	boundary$id2<-match(boundary$id2, pu)
	boundary<-rcpp_sum_duplicates(boundary[[1]], boundary[[2]], boundary[[3]])
	# polygons
	polygons<-x@polygons[x@polygons$PID %in% pu,]
	polygons$PID<-match(polygons$PID, pu)
	# return new object
	return(
		RapData(
			pu=x@pu[pu,,drop=FALSE],
			species=x@species,
			targets=x@targets,
			pu.species.probabilities=pu.species.probabilities,
			attribute.spaces=lapply(
				x@attribute.spaces,
				function(x) {
						AttributeSpace(
							SimplePoints(x@pu@coords[pu,,drop=FALSE]),
							x@dp[species]
					  )
				}
			),
			boundary=boundary,
			polygons=polygons
	  )
	)
}

#' @rdname dp.subset
#' @method dp.subset RapData
#' @export
dp.subset.RapData<-function(x, space, species, points) {
	# create objects
	attr.space<-x@attribute.spaces
	for (i in seq_along(space)) {
		for (j in seq_along(species)) {
			attr.space[[space[i]]]@dp[[species[j]]]<-DemandPoints(
				SimplePoints(attr.space[[space[i]]]@dp[[species[j]]]@points@coords[points,]),
				attr.space[[space[i]]]@dp[[species[j]]]@weights[points]
			)
		}
	}
	# return new object
	return(
		RapData(
			pu=x@pu,
			species=x@species,
			targets=x@targets,
			pu.species.probabilities=x@pu.species.probabilities,
			attribute.spaces=attr.space,
			boundary=x@boundary,
			polygons=x@polygons
	  )
	)
}

#' @rdname prob.subset
#' @method prob.subset RapData
#' @export
prob.subset.RapData<-function(x, species, threshold) {
	stopifnot(length(species)==length(threshold))
	# create new object
	pu.species.probs<-x@pu.species.probabilities
	for (i in seq_along(species)) {
		rows <- which(pu.species.probs$species == species[i] & pu.species.probs[[3]] < threshold[i])
		if (length(rows)>0)
			pu.species.probs<-pu.species.probs[-rows,,drop=FALSE]
	}
	# return new object
	return(
		RapData(
			pu=x@pu,
			species=x@species,
			targets=x@targets,
			pu.species.probabilities=pu.species.probs,
			attribute.spaces=x@attribute.spaces,
			boundary=x@boundary,
			polygons=x@polygons
	  )
	)
}

#' @rdname update
#' @export
#' @method update RapData
update.RapData<-function(object, species=NULL, space=NULL, name=NULL, amount.target=NULL, space.target=NULL, pu=NULL, cost=NULL, status=NULL, ...) {
	# deparse species
	if (is.null(species)) {
		species<-seq_len(nrow(object@species))
	} else {
		if (is.character(species))
			species<-match(species, object@species$name)
		if (is.na(species) | !species %in% seq_len(nrow(object@species)))
			stop('argument to species not found')
	}
	# deparse space
	if (is.null(space))
		space<-seq_along(object@attribute.spaces)
	# update species
	if (!is.null(name))
		object@species$name[species] <-name
	# update pu
	if (!is.null(pu) & is.null(status))
	if (!is.null(pu) & is.null(cost))
		object@pu$status[pu] <- status
		object@pu$cost[pu] <- cost
	# update amount targets
	if (!is.null(amount.target))
		object@targets$proportion[which(object@targets$target==0 & object@targets$species %in% species)]<-amount.target
	# update space targets
	if (!is.null(space.target))
		object@targets$proportion[which(object@targets$target %in% space & object@targets$species %in% species)]<-space.target
	# check object for validity
	validObject(object, test=FALSE)
	# return object
	return(object)
}

#' @rdname spp.plot
#' @method spp.plot RapData
#' @export
spp.plot.RapData<-function(
	x,
	species,
	prob.color.palette='YlGnBu',
	pu.color.palette=c('#4D4D4D', '#00FF00', '#FFFF00', '#FF0000'),
	basemap='none',
	alpha=ifelse(basemap=="none", 1, 0.7),
	grayscale=FALSE,
	main=NULL,
	force.reset=FALSE,
	...
) {
	# data checks
	stopifnot(length(species)==1)
	if (nrow(x@polygons)==0)
			stop("Spatial data for planning units not present in object")
	if (is.character(species)) {
		if (!species %in% x@species$name)
			stop('argument to species is not a species name in argument to x')
		spp_pos <-match(species, x@species$name)
	}
	if (is.numeric(species)) {
		if (!species %in% seq_along(x@species$name))
			stop('argument to species is not a valid index for species in argument to x')
		spp_pos <- species
	}
	# get basemap
	if (basemap!="none")
		basemap<-basemap.RapData(x, basemap, grayscale, force.reset)
	## main processing
	# extract planning unit colors
	values<-numeric(nrow(x@pu))
	rows<-which(x@pu.species.probabilities$species == spp_pos )
	values[x@pu.species.probabilities$pu[rows]]<-x@pu.species.probabilities$value[rows]
	if (length(unique(values))>1) {
		cols<-brewerCols(rescale(values, to=c(0,1)), prob.color.palette, alpha)
	} else {
		cols<-brewerCols(rep(values[1], length(values)), prob.color.palette, alpha)
		values<-c(0,values[1])
	}
	# set title
	if (is.null(main)) {
		if ('name' %in% names(x@species) & is.numeric(species)) {
			main=paste0(x@species$name[species])
		} else if (is.numeric(species)) {
			main=paste0('Species ', species)
		} else {
			main=paste0(species)
		}
	}	
	# get selected rows
	sel.pu.ids<-which(x@pu$status==2)
	unsel.pu.ids<-which(x@pu$status!=2)
	# extract planning unit border colors
	border.cols<-rep(pu.color.palette[1], nrow(x@pu))
	border.cols[sel.pu.ids]<-pu.color.palette[2]
	border.cols[which(x@pu$status==2)]<-pu.color.palette[3]
	border.cols[which(x@pu$status==3)]<-pu.color.palette[4]
	# make plot
	prettyGeoplot(
		polygons=list(x@polygons[x@polygons$PID %in% unsel.pu.ids,], x@polygons[x@polygons$PID %in% sel.pu.ids,]),
		col=list(cols[unsel.pu.ids], cols[sel.pu.ids]),
		basemap,
		main=main,
		continuousLegend(values,prob.color.palette,posx=c(0.3, 0.4),posy=c(0.1, 0.9)),
		beside=TRUE,
		border=list(border.cols[unsel.pu.ids], border.cols[sel.pu.ids]),
		lwd=list(1, 5)
	)
}

#' @rdname space.plot
#' @method space.plot RapData
#' @export
space.plot.RapData<-function(
		x,
		species,
		space=1,
		pu.color.palette=c('#4D4D4D4D', '#00FF0080', '#FFFF0080', '#FF00004D'),
		main=NULL,
		...
	) {
	# data checks
	stopifnot(length(species)==1)
	if (is.character(species)) {
		if (!species %in% x@species$name)
			stop('argument to species is not a species name in argument to x')
		spp_pos<-match(species, x@species$name)
	}
	if (is.numeric(species)) {
		if (!species %in% seq_along(x@species$name))
			stop('argument to species is not a valid index for species in argument to x')
		spp_pos <- species
	}
	# set title
	if (is.null(main)) {
		if ('name' %in% names(x@species) & is.numeric(species)) {
			main=paste0(x@species$name[species], ' in space ', space)
		} else if (is.numeric(species)) {
			main=paste0('Species ', species, ' in space ', space)
		} else {
			main=paste0(species, ' in space ', space)
		}
	}
	# extract pu data
	pu<-as.data.frame(x@attribute.spaces[[space]]@pu@coords)
	names(pu)<-paste0('X',seq_len(ncol(pu)))
	pu$status	<-'Not Selected'
	pu$status[which(x@pu$status==2)]<-'Locked In'
	pu$status[which(x@pu$status==3)]<-'Locked Out'
	# extract dp data
	dp<-as.data.frame(x@attribute.spaces[[space]]@dp[[spp_pos]]@points@coords)
	names(dp)<-paste0('X',seq_len(ncol(dp)))
	dp$weights=x@attribute.spaces[[space]]@dp[[spp_pos]]@weights
	# make plots
	do.call(
		paste0('spacePlot.',ncol(x@attribute.spaces[[space]]@pu@coords),'d'),
		list(
			pu,
			dp,
			pu.color.palette,
			main
		)
	)
}

#' @rdname amount.target
#' @method amount.target RapData
#' @export
amount.target.RapData<-function(x, species=NULL) {
	if (is.null(species))
		return(
			structure(
				x@targets$proportion[which(x@targets$target==0)],
				.Names = x@species$name[x@targets$species[which(x@targets$target==0)]]
			)
		)
	if (is.character(species))
		species<-match(species, x@species$name)
	return(
		structure(
			x@targets$proportion[which(x@targets$target==0 & x@targets$species==species)],
			.Names = x@species$name[x@targets$species[which(x@targets$target==0 & x@targets$species==species)]]
		)
	)
}

#' @rdname amount.target
#' @export
`amount.target<-.RapData`<-function(x, species=NULL, value) {
	if (is.null(species)) {
		x@targets$proportion[which(x@targets$target==0)]<-value
	} else {
		if (is.character(species))
			species<-match(species, x@species$name)
		x@targets$proportion[which(x@targets$target==0 & x@targets$species %in% species)]<-value
	}
	# check of validity
	validObject(x, test=FALSE)
	return(x)
}

#' @rdname space.target
#' @method space.target RapData
#' @export
space.target.RapData<-function(x, species=NULL, space=NULL) {
	rows <- seq_len(nrow(x@targets))
	if (!is.null(species)) {
		if (is.character(species))
			species<-match(species, x@species$name)
		rows<-rows[which(x@targets$species %in% species)]
	}
	if (is.null(space)) {
		rows<-rows[which(x@targets$target[rows] > 0)]
	} else{
		rows<-rows[which(x@targets$target[rows] %in% space)]
	}
	return(
		structure(
			x@targets$proportion[rows],
 			.Dim = c(
				length(unique(x@targets$species[rows])),
				length(unique(x@targets$target[rows]))
			),
			.Dimnames = list(
					unique(x@species$name[x@targets$species[rows]]),
					unique(x@targets$target[rows])
			)
		)
	)
}

#' @rdname space.target
#' @export
`space.target<-.RapData`<-function(x, species=NULL, space=NULL, value) {
	rows <- seq_len(nrow(x@targets))
	if (!is.null(species)) {
		if (is.character(species))
			species<-match(species, x@species$name)
		rows<-rows[which(x@targets$species %in% species)]
	}
	if (is.null(space)) {
		rows<-rows[which(x@targets$target[rows] > 0)]
	} else{
		rows<-rows[which(x@targets$target[rows] %in% space)]
	}
	# assign new targets
	x@targets$proportion[rows]<-value
	# check of validity
	validObject(x, test=FALSE)
	return(x)
}
