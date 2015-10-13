#' @include RcppExports.R raspr-internal.R
NULL

#' Test if Gurobi is installed on computer
#'
#' This function determines if Gurobi is installed on the computer, check that its licensing is set up, and will store the path in \code{\link[base]{options}}.
#'
#' @return \code{logical} Is it installed and ready to use?
#' @seealso \code{\link[base]{options}}.
#' @export
#' @examples
#' is.GurobiInstalled()
#' options()$GurobiInstalled
is.GurobiInstalled<-function() {
	# check if installed 
	gpth=Sys.getenv('GUROBI_HOME')
	if (nchar(gpth)==0) {
		options(GurobiInstalled=FALSE)
		stop('Gorubi is not installed on system')
	}
	# try running example problem
	gpth2=tempfile()
	ret=try(call.Gurobi(GurobiOpts(), file.path(gpth, 'examples/data/coins.lp'), paste0(gpth2, '.log'), paste0(gpth2, '.sol'), verbose=FALSE), silent=TRUE)
	if (!file.exists(paste0(gpth2, '.sol'))) {
		cat(GurobiInstalled=FALSE)
		stop('Gorubi is not setup correctly.')
	}
	options(GurobiInstalled=TRUE)
	return(invisible(TRUE))
}


#' Test if GDAL is installed on computer
#'
#' This function tests if GDAL is installed on the computer.
#' If not, download it here: \url{http://download.osgeo.org/gdal}.
#'
#' @return \code{logical} is GDAL installed?
#' @seealso \code{\link[gdalUtils]{gdal_setInstallation}}.
#' @export
#' @examples
#' is.gdalInstalled()
is.gdalInstalled<-function() {
	suppressWarnings(findGdalInstallationPaths())
	return(!is.null(getOption("gdalUtils_gdalPath")))
} 

#' Rasterize polygon data using GDAL
#'
#' This function converts a \code{SpatialPolygonsDataFrame} to a \code{RasterLayer} using GDAL.
#' It is expected to be faster than \code{\link[raster]{rasterize}} for large datasets.
#' However, it will be significantly slower for small datasets because the data will need to be written and read from disk.
#' @param x \code{SpatialPolygonsDataFrame} object.
#' @param y \code{RasterLayer} with dimensions, extent, and resolution to be used as a template for new raster.
#' @param field \code{character} column name with values to burn into the output raster. If not supplied, default behaviour is to burn polygon indices into the \code{	RasterLayer}.
#' @export
#' @return \code{RasterLayer} object.
#' @seealso \code{\link[raster]{rasterize}}, \code{\link{is.gdalInstalled}}.
#' @examples
#' data(species,planningunits)
#' x<-rasterizeGDAL(planningunits[1:5,],species[[1]])
rasterizeGDAL<-function(x,y, field=NULL) {
	if (is.null(field)) {
		x@data$id<-seq_len(nrow(x@data))
		field<-'id'
	}
	if (!field %in% names(x@data))
		stop(paste0("x@data does not have a field called ",field, "."))
	writeOGR(x, tempdir(), 'polys', driver='ESRI Shapefile', overwrite_layer=TRUE)
	writeRaster(setValues(y, NA), file.path(tempdir(), 'rast.tif'), NAflag=-9999, overwrite=TRUE)
	return(gdal_rasterize(file.path(tempdir(), 'polys.shp'), file.path(tempdir(), 'rast.tif'), l="polys", a=field, output_Raster=TRUE)[[1]])
}


#' Blank raster 
#'
#' This functions creates a blank raster based on the spatial extent of a Spatial object.
#' @param x \code{Spatial*} object. 
#' @param res \code{numeric vector} specifying resolution of the output raster in the x and y dimensions. If \code{vector} is of length one, then the pixels are assumed to be square.
#' @export
#' @rdname blank.raster
blank.raster<-function(x, res) {
	# init
	if (length(res)==1)
		res=c(res, res)
	# generate raster from sp
	xpos<-seq(xmin(x), xmax(x)+res[1]*(((xmax(x)-xmax(x)) %% res[1])!=0), res[1])
	ypos<-seq(ymin(x), ymax(x)+res[2]*(((ymax(x)-ymax(x)) %% res[2])!=0), res[2])
	rast<-raster(xmn=min(xpos), xmx=max(xpos), ymn=min(ypos), ymx=max(ypos), nrow=length(ypos)-1, ncol=length(xpos)-1)
	return(setValues(rast, 1))
}


