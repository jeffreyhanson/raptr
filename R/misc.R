#' @include RcppExports.R raspr-internal.R
NULL

#' Test if Gurobi is installed on computer
#'
#' This function determines if the Gurobi R package is installed on the computer and that it can be used \code{\link[base]{options}}.
#'
#' @param verbose \code{logical} should messages be printed?
#' @return \code{logical} Is it installed and ready to use?
#' @seealso \code{\link[base]{options}}.
#' @export
#' @examples
#' \dontrun{
#' # check if Gurobi is installed
#' is.GurobiInstalled()
#' # print cached status of installation
#' options()$GurobiInstalled
#' }
is.GurobiInstalled<-function(verbose=TRUE) {
	# check if installed 
	if (!'gurobi' %in% unlist(sapply(.libPaths(), dir), recursive=FALSE, use.names=TRUE)) {
		if (verbose)
			cat('The gorubi R package is not installed\n')
		options(GurobiInstalled=FALSE)
		return(FALSE)
	}
	# try running example problem - from the gurobi help file
	tmp<-capture.output(
		{
		result<-gurobi::gurobi(
			list(
				A=matrix(c(1, 2, 3, 1, 1, 0), nrow = 2, ncol=3, byrow=TRUE),
				obj=c(1, 1, 2),
				sense=c("<=", ">="),
				rhs=c(4, 1),
				vtype="B"
			),
			list(Presolve=2, TimeLimit=10.0)
		)
		}
	)
	if (result$status!="OPTIMAL") {
		if (verbose)
			cat('The gurobi R package is installed, but R is having issues using it\n')
		options(GurobiInstalled=FALSE)
		return(FALSE)		
	}
	return(TRUE)
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
#' # check if gdal is installed on system
#' \dontrun{
#' is.gdalInstalled()
#' }
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
#' \dontrun{
#' data(sim_pu,sim_spp)
#' x <- rasterizeGDAL(sim_pu[1:5,],sim_spp[[1]])
#' }
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
#' @examples
#' data(sim_pus)
#' x <- blank.raster(sim_pus, 1)
#' print(x)
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

#' PolySet
#'
#' Object contains PolySet data.
#'
#' @seealso \code{\link[PBSmapping]{PolySet}}.
#' @name PolySet-class
#' @aliases PolySet
#' @exportClass PolySet
setClass("PolySet", contains='data.frame')

#' PolySetOrNULL class
#'
#' Object is either \code{PolySet} or \code{NULL}.
#'
#' @name PolySetOrNULL-class
#' @aliases PolySetOrNULL
#' @exportClass PolySetOrNULL
setClassUnion("PolySetOrNULL", c("PolySet", "NULL"))

#' data.frameOrNULL class
#'
#' Object is either \code{data.frame} or \code{NULL}.
#'
#' @name data.frameOrNULL-class
#' @aliases data.frameOrNULL
#' @exportClass data.frameOrNULL
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))


#' Sample random points from a RasterLayer
#'
#' This function generates random points in a \code{RasterLayer} object.
#' 
#' @param mask \code{RasterLayer} object
#' @param n \code{integer} number of points to sample
#' @param prob \code{logical} should the raster values be used as weights? Defaults to \code{FALSE}.
#' @return code{matrix} with x-coordinates, y-coordinates, and cell values.
#' @seealso \code{\link[dismo]{randomPoints}}.
#' @export
#' @examples
#' data(sim_spp)
#' # generate points
#' pts <- randomPoints(sim_spp[[1]])
#' # plot points
#' plot(sim_spp[[1]])
#' points(pts)
randomPoints <- function(mask, n, prob=FALSE) {
	# check that data can be processed in memory
	stopifnot(canProcessInMemory(mask, n=3))
	# extract cells
	validPos<-which(is.finite(mask[]))
	if (length(validPos) < n)
		stop('argument to n is greater than the number of cells with finite values')
	if (prob) {
		randomCells <- sample(validPos, n, prob=mask[validPos], replace=FALSE)
	} else {
		randomCells <- sample(validPos, n, , replace=FALSE)
	}
	# get coordinates of the cell centres 
	return(
		xyFromCell(mask, randomCells)
	)
}


