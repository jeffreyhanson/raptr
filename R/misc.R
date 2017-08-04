#' @include RcppExports.R raptr-internal.R
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
#' \donttest{
#' # check if Gurobi is installed
#' is.GurobiInstalled()
#' # print cached status of installation
#' options()$GurobiInstalled
#' }
is.GurobiInstalled<-function(verbose=TRUE) {
	# define installation instructions
	gurobiInstallationInstructions <- paste(
		'Follow these instructions to download the Gurobi software suite:\n\t-',
		c('Linux'='http://bit.ly/1ksXUaQ','Windows'='http://bit.ly/1MrjXWc', 'Darwin'='http://bit.ly/1N0AlT0')[Sys.info()[['sysname']]]
	)
	
	rInstallationInstructions1 <- paste(
		'Follow these instructions to install the "gurobi" R package:\n\t-',
		c('Linux'='http://bit.ly/1HLCRoE','Windows'='http://bit.ly/1MMSZaH','Darwin'='http://bit.ly/1Pr2WRG')[Sys.info()[['sysname']]]
	)
		
	rInstallationInstructions2 <- 'To access multiple solutions from the solution pool,\n\tfollow these instructions to install the "rgurobi" R package from GitHub: \n\thttp://bit.ly/1RF19XO'
		
	licenseInstructions <- 'The Gurobi R package requires a Gurobi license to work:\n\t- visit this web-page for an overview: http://bit.ly/1OHEQCm\n\t- academics can obtain a license at no cost here: http://bit.ly/1iYg3LX'
	
	# check if gurobi installed
	result<-suppressWarnings(system2('gurobi_cl','-v', stdout=FALSE, stderr=FALSE))
	if (result!=0) {
		if (verbose) {
			cat('The gorubi software is not installed\n')
			cat('\n', gurobiInstallationInstructions,'\n\n',licenseInstructions,'\n\n',rInstallationInstructions1,'\n\n', rInstallationInstructions2, '\n\n')
		}
		options(GurobiInstalled=list(gurobi=FALSE, rgurobi=FALSE))
		return(FALSE)
	}
	# check if R packages installed
	pkgs.installed <- list(
		gurobi=requireNamespace('gurobi', quietly=TRUE),
		rgurobi=requireNamespace('rgurobi', quietly=TRUE)
	)
	if (!pkgs.installed[[1]]) {
		if (verbose) {
			cat('The gorubi R package is not installed\n')
			cat('\n', rInstallationInstructions1, '\n\n')
		}
	}
	if (!pkgs.installed[[2]]) {
		if (verbose) {
			cat('The rgorubi R package is not installed\n')
			cat('\n', rInstallationInstructions2, '\n\n')
		}
	}
	options(GurobiInstalled=pkgs.installed)
	if (!pkgs.installed[[1]])
		return(FALSE)
	
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
	if (file.exists('gurobi.log')) unlink('gurobi.log')
	if (result$status!="OPTIMAL") {
		if (verbose) {
			cat('The gurobi R package is installed, but R is having issues using it\n')
			cat('\nOutput from trying to run Gurobi:\n\n')
			cat(paste(tmp, collapse='\n'),'\n',sep='')
			cat('\nThis might be due to licensing issues.\n',licenseInstructions, '\n',sep='')
		}
		options(GurobiInstalled=list(gurobi=FALSE, rgurobi=FALSE))
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
#' \donttest{
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
#' @param x \code{\link[sp]{SpatialPolygonsDataFrame}} object.
#' @param y \code{\link[raster]{raster}} with dimensions, extent, and resolution to be used as a template for new raster.
#' @param field \code{character} column name with values to burn into the output raster. If not supplied, default behaviour is to burn polygon indices into the \code{\link[raster]{raster}}.
#' @export
#' @return \code{RasterLayer} object.
#' @seealso \code{\link[raster]{rasterize}}, \code{\link{is.gdalInstalled}}.
#' @examples
#' \donttest{
#' data(cs_pus,cs_spp)
#' x <- rasterizeGDAL(cs_pus[1:5,], cs_spp[[1]])
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
#' @param x \code{\link[sp]{Spatial-class}} object.
#' @param res \code{numeric vector} specifying resolution of the output raster in the x and y dimensions. If \code{vector} is of length one, then the pixels are assumed to be square.
#' @export
#' @rdname blank.raster
#' @examples
#' # make SpatialPolygons
#' polys <- sim.pus(225L)
#' # make RasterLayer from SpatialPolygons
#' blank.raster(polys, 1)
blank.raster <- function(x, res) {
	# init
	if (length(res)==1)
		res <- c(res, res)
	# extract coordinates
	if ((xmax(x)-xmin(x) <= res[1])) {
		xpos <- c(xmin(x), res[1])
	} else {
		xpos<-seq(xmin(x), xmax(x)+(res[1]*(((xmax(x)-xmin(x)) %% res[1])!=0)), res[1])
	}
	if ((ymax(x)-ymin(x) <= res[2])) {
		ypos <- c(ymin(x), res[2])
	} else {
		ypos<-seq(ymin(x), ymax(x)+(res[2]*(((ymax(x)-ymin(x)) %% res[2])!=0)), res[2])
	}
	# generate raster from sp
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
setClass("PolySet")

#' RapOpts class
#'
#' Object is either \code{\link{RapReliableOpts}} or \code{\link{RapUnreliableOpts}}.
#'
#' @name RapOpts-class
#' @aliases RapOpts
#' @exportClass RapOpts
setClass("RapOpts",
	representation(
		BLM="numeric"
	),
	prototype=list(
		BLM=0
	)
)

#' SolverOpts class
#'
#' Object stores parameters used to solve problems.
#'
#' @name SolverOpts-class
#' @seealso \code{\link{GurobiOpts}}.
#' @aliases SolverOpts
#' @exportClass SolverOpts
setClass("SolverOpts")

#' Sample random points from a RasterLayer
#'
#' This function generates random points in a \code{\link[raster]{raster}} object.
#'
#' @param mask \code{\link[raster]{raster}} object
#' @param n \code{integer} number of points to sample
#' @param prob \code{logical} should the raster values be used as weights? Defaults to \code{FALSE}.
#' @return \code{\link[base]{matrix}} with x-coordinates, y-coordinates, and cell values.
#' @seealso This function is similar to \code{dismo::randomPoints}.
#' @export
#' @examples
#' # simulate data
#' sim_pus <- sim.pus(225L)
#' sim_spp <- sim.species(sim_pus, model='normal', n=1, res=0.25)
#' # generate points
#' pts1 <- randomPoints(sim_spp, n=5)
#' pts2 <- randomPoints(sim_spp, n=5, prob=TRUE)
#' # plot points
#' plot(sim_spp)
#' points(pts1, col='red')
#' points(pts2, col='black')
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
		randomCells <- sample(validPos, n, replace=FALSE)
	}
	# get coordinates of the cell centres
	return(
		xyFromCell(mask, randomCells)
	)
}
