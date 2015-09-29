#' @include RcppExports.R
NULL

#' @export
#' @rdname calcSpeciesAverageInPus
calcSpeciesAverageInPus.SpatialPolygons<-function(x,y,ids=seq_len(nlayers(y)), ncores=1, gdal=FALSE, ...) {
	# check for invalid inputs
	stopifnot(inherits(y, "Raster"))
	stopifnot(nlayers(y)!=length(ids))
	return(
		calcPuVsSpeciesData.SpatialPolygonsDataFrame(
			x=SpatialPolygonsDataFrame(x@polygons, data=data.frame(id=seq_len(nrow(x@data)), row.names=laply(x@polygons, slot, name="ID"))),
			y=y,
			ids=ids,
			ncores=ncores,
			gdal=gdal,
			field="id"
		)
	)
}

#' @export
#' @rdname calcSpeciesAverageInPus
calcSpeciesAverageInPus.SpatialPolygonsDataFrame<-function(x,y,ids=seq_len(nlayers(y)), ncores=1, gdal=FALSE, field=NULL, ...) {
	# check for invalid inputs
	stopifnot(inherits(y, "Raster"))
	stopifnot(nlayers(y)==length(ids))
	# prepare attribute table
	if (is.null(field)) {
		x@data<-data.frame(id=seq_len(nrow(x@data)), row.names=row.names(x@data))
	} else {
		x@data<-data.frame(id=x@data[[field]], row.names=row.names(x@data))
	}
	# set zero values in raster to NA to speed up processing
	if (inherits(y,c('RasterStack','RasterBrick'))) {
		for (i in seq_len(nlayers(y)))
			y[[i]][Which(y[[i]]==0)]<-NA
 	} else { 
		y[Which(y==0)]<-NA
	}
	# generate raster layer with polygons
	if (gdal & is.gdalInstalled()) {
		x<-rasterize.gdal(x, y[[1]], "id")
	} else {
		if (gdal & !is.gdalInstalled())
			warning('GDAL is not installed on this computer, using raster::rasterize for processing')
		x<-rasterize(x, y[[1]], method="ngb")
	}
	# main processing
	return(zonalMean(x, y, ids, ncores))
} 
