#' @include RcppExports.R dependencies.R
NULL

## define built-in functions
#' Affine transformation
#' @noRd
affineTrans <- function(OldValue, OldMax, OldMin, NewMax, NewMin) {
    OldRange <- (OldMax - OldMin)
    NewRange <- (NewMax - NewMin)
    NewValue <- (((OldValue - OldMin) * NewRange) / OldRange) + NewMin
    return(NewValue)
}

#' Normal niche simulation function
#' @noRd
normal_niche <- function(x, y) {
	return(dmvnorm(x=matrix(c(x,y), ncol=2), mean=c(0,0), sigma=matrix(c(8.5,0,0,8.5), ncol=2))*40)
}

#' Uniform niche simulation function
#' @noRd
uniform_niche <- function(x, y) {
	return(rep(0.5, length(x)))
}

#' Bimodal niche simulation function
#' @noRd
bimodal_niche <- function(x, y) {
	return(
		apply(
			matrix(
				c(
					dmvnorm(x=matrix(c(x,y), ncol=2), mean=c(-2,-2), sigma=matrix(c(5,0,0,5), ncol=2))*30,
					dmvnorm(x=matrix(c(x,y), ncol=2), mean=c(3,3), sigma=matrix(c(3,0,0,3), ncol=2))*12
				),
				ncol=2
			),
			1,
			max
		)
	)
}

#' Function to hash function call
#' @noRd
hashCall<-function(expr, skipargs=c(), env=parent.frame()) {
	expr<-expr[c((skipargs*-1L)-1L)]
	expr<-expr[which(names(expr)!="force.reset")]
	for (i in seq_along(names(expr)))
		if (inherits(expr[[i]], c("name")))
			expr[[i]] <- eval(expr[[i]], envir=env)
	return(paste(deparse(expr), collapse=";"))
}

#' Make pretty geoplot
#' @noRd
prettyGeoplot<-function(polygons, col, basemap, main, fun, beside=TRUE, border=NULL, lwd=1) {
	# make layout
	defpar<-par(no.readonly = TRUE)
	par(mar=c(1,1,1,1),oma=c(0,0,0,0))
	if (beside) {
		layout(matrix(c(1,1,3,2),ncol=2,byrow=TRUE), widths=c(0.8,0.2), heights=c(0.1,0.9))
	} else {
		layout(matrix(c(1,3,2),ncol=1,byrow=TRUE), widths=c(1), heights=c(0.1,0.8,0.1))
	}
	# convert to list if not
	if (!inherits(polygons, 'list')) {
		polygons <- list(polygons)
		col <-list(col)
		border <- list(border)
		lwd <- list(lwd)
	}
	
	# make title
	plot(1,1,type="n", xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, xlab="", ylab="")
	mtext(side=1, line=-0.5, main, cex=1.5)
	# make legend
	plot(1,1,type="n", xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, xlab="", ylab="")
	fun()
	# make geoplot
	if (is.list(basemap)) {
		PlotOnStaticMap(basemap)
		for (i in seq_along(polygons)) {
			suppressWarnings(PlotPolysOnStaticMap(basemap, polygons[[i]], col=col[[i]], border=border[[i]], add=TRUE, lwd=lwd[[i]]))
		}
	} else {
		allpolygons <- do.call(rbind, polygons)
		plotPolys(polygons[[1]], col=col[[1]], axes=FALSE, xlab="", ylab="", border=border[[1]], lwd=lwd[[1]], xlim=range(allpolygons$X), ylim=range(allpolygons$Y))
		for (i in seq_along(polygons)[-1]) {
			if (nrow(polygons[[i]])>0)
				suppressWarnings(addPolys(polygons[[i]], col=col[[i]], xlab="", ylab="", border=border[[i]], lwd=lwd[[i]]))
		}
	}
	par(defpar)
	return(invisible())
}

#' Extract colors from RcolorBrewer palette functions
#' @noRd
brewerCols<-function(values, pal, alpha=1, n=NULL) {
	if (is.null(n) & length(pal)==1) {
		n<-brewer.pal.info$maxcolors[which(rownames(brewer.pal.info)==pal)]
	} else {
		n<-length(values)
	}
	if (length(pal)==1) {
		suppressWarnings(r<-colorRamp(brewer.pal(n, pal))(values))
	} else{
		suppressWarnings(r<-colorRamp(pal)(values))
	}
	return(rgb(r, maxColorValue=255, alpha=rescale(alpha, from=c(0,1), to=c(0,255))))
}

#' Add continuous legend to plot
#' @noRd
continuousLegend<-function(values, pal, posx, posy, center=FALSE, endlabs=NULL) {
	return(
		function() {
			if (center) {
				vabs<-max(abs(range(values)))
				values<-seq(-vabs,vabs,length.out=100)
			}
			xdiff<-diff(par()$usr[1:2])
			ydiff<-diff(par()$usr[3:4])
			zvals<-pretty(values)
			zvals<-zvals[which(zvals>min(values) & zvals<max(values))]
			if (max(zvals)<1) {
				digit<-2
			} else {
				digit<-1
			}
			# rect(xleft=par()$usr[1]+(xdiff*posx[1]), ybottom=par()$usr[3]+(ydiff*posy[1]), xright=par()$usr[1]+(xdiff*posx[2]), ytop=par()$usr[4]+(ydiff*posy[2]), xpd=TRUE)
			colorlegend(zlim=range(values), digit=digit, col=brewerCols(seq(0,1,length.out=100), pal), zval=zvals, posx=posx, posy=posy, xpd=TRUE)
			if (!is.null(endlabs)) {
				xcoord<-par()$usr[1] + mean(par()$usr[2:1]*posx*2.2)
				ycoords<-(par()$usr[3] + diff(par()$usr[3:4])*posy) + (diff(par()$usr[3:4]) * c(-0.02,0.02))
				text(x=rep(xcoord, 2), y=ycoords, rev(endlabs), cex=1.2, ad=c(0.5,0.5))
			}
		}
	)
}

#' Add categorical legend to plot
#' @noRd
categoricalLegend<-function(col,labels, ncol=1) {
	return(
		function() {
			if (ncol==1) {
				legend("top", bg="white", legend=labels, fill=col, bty="n", horiz=TRUE, cex=1.5)
			} else {
				legend(y=par()$usr[3]+(diff(par()$usr[3:4])*0.6), x=par()$usr[1]+(diff(par()$usr[1:2])*0.5), bg="white", legend=labels, fill=col, bty="n", ncol=ncol, cex=1.5, xjust=0.5, yjust=0.5, xpd=TRUE)
			}
		}
	)
}

#' Create 1-dimensional demand points
#' @noRd
demand.points.density1d<-function(pts, n, quantile=0, ...) {
	# transform pts
	curr.mean<-mean(pts[,1])
	curr.sd<-sd(pts[,1])
	pts[,1] <- (pts[,1] - curr.mean) / curr.sd
	# generate points
	quants<-quantile(pts[,1], c((quantile/2), 1-(quantile/2)))
	dp<-runif(n, quants[[1]], quants[[2]])
	# density kernel
	est<-kde(pts[,1], eval.points=dp, ...)
	# back-transform demand point coordinates
	dp.pts <- (matrix(est$eval.points, ncol=1) * curr.sd) + curr.mean
	# return object
	return(
		list(
			coords=dp.pts,
			weights=est$estimate
		)
	)
}

#' Create 2-dimensional demand points
#' @noRd
demand.points.density2d<-function(pts, n, quantile=0, ...) {
	# transform pts
	curr.mean<-apply(pts,2,mean)
	curr.sd<-apply(pts,2,sd)
	pts<-sweep(pts,MARGIN=2,FUN='-',curr.mean)
	pts<-sweep(pts,MARGIN=2,FUN='/',curr.sd)
	# generate points
	dp<-spsample(
		mcp(SpatialPoints(coords=pts), percent=(100-quantile), unin = c("m"), unout = c("m2")),
		n*1.1,
		type='random'
	)@coords[seq_len(n),]
	# fit density kernel
	est<-kde(pts, eval.points=dp, ...)
	# back-transform dps
	dp<-sweep(dp,MARGIN=2,FUN='*',curr.sd)
	dp<-sweep(dp,MARGIN=2,FUN='+',curr.mean)
	# prepare data to return
	return(
		list(
			coords=dp,
			weights=est$estimate
		)
	)
}

#' Create n-dimensional demand points
#' @noRd
demand.points.hypervolume<-function(pts, n, quantile=0, ...) {
	# transform pts
	curr.mean<-apply(pts,2,mean)
	curr.sd<-apply(pts,2,sd)
	pts<-sweep(pts,MARGIN=2,FUN='-',curr.mean)
	pts<-sweep(pts,MARGIN=2,FUN='/',curr.sd) 
	# fit density kernel
	args<-list(...)
	if (!'repsperpoint' %in% names(args))
		args$repsperpoint<-500*ncol(pts)
	if (args$repsperpoint*nrow(pts) < n) {
		stop('argument to n.demand.points is too high. Set a higher value in the argument to repsperpoint (defaults to 500*dimensions in attribute space).')
	}
	# estimate bandwidth for kernel
	if (!'bandwidth' %in% names(args)) {
		args$bandwidth<-estimate_bandwidth(pts)
	}
	# fit kernel
	hv<-do.call(hypervolume, append(list(data=pts, quantile=quantile), args))
	# extract random points
	rndpos<-sample.int(nrow(hv@RandomUniformPointsThresholded), n)
	# extract coordinates and back-transform
	dp<-hv@RandomUniformPointsThresholded[rndpos,,drop=FALSE]
	dp<-sweep(dp,MARGIN=2,FUN='*',curr.sd)
	dp<-sweep(dp,MARGIN=2,FUN='+',curr.mean)
	# return object
	return(
		list(
			coords=dp,
			weights=hv@ProbabilityDensityAtRandomUniformPoints[rndpos]
		)
	)
}


#' Calculate zonal-means
#' @noRd
zonalMean <- function(x, y, ids=names(y), ncores=1) {
	if (canProcessInMemory(x,2)) {
		x<-rbind.fill(llply(seq_len(nlayers(y)), function(l) {
				return(zonalMean.RasterLayerInMemory(x, y[[l]], ids[l]))
		}))
	} else {
		bs<-blockSize(x)
		if (ncores>1) {
			clust<-makeCluster(ncores, type="SOCK")
			clusterEvalQ(clust, {library(raster);library(Rcpp)})
			clusterExport(clust, c("bs", "x", "rcpp_groupmean"))
			registerDoParallel(clust)
		}
		x<-rbind.fill(llply(seq_len(nlayers(y)), function(l) {
			return(zonalMean.RasterLayerNotInMemory(bs, x, y[[l]], ids[l], registered=ncores>1, clust))
		}))
		if (ncores>1)
			clust<-stopCluster(clust)
	}
	# sort data and return
	return(x)
}

#' Calculate zonal-means for rasters where all data can be stored in RAM
#' @noRd
zonalMean.RasterLayerInMemory <- function(polys, rast, speciesName) {
	tmp<-rcpp_groupmean(getValues(polys),getValues(rast))
	tmp<-data.frame(species=speciesName, pu=attr(tmp, "ids"), value=c(tmp))
	return(tmp[which(tmp$value>0),,drop=FALSE])
}

#' Calculate zonal-means by processing data in chunks
#' @noRd
zonalMean.RasterLayerNotInMemory <- function(bs, polys, rast, speciesName, ncores, registered, clust) {
	if (registered & .Platform$OS.type=="windows")
		clusterExport(clust, c("bs","polys", "rast", "rcpp_groupmean"))
	tmp<-rcpp_groupcombine(llply(seq_len(bs$n), .parallel=registered, function(i) {
		return(rcpp_groupmean(getValues(polys, bs$row[i], bs$nrows[i]), getValues(rast, bs$row[i], bs$nrows[i])))
	}))
	tmp<-data.frame(species=speciesName, pu=attr(tmp, "ids"), value=c(tmp))
	return(tmp[which(tmp$value>0),,drop=FALSE])
}

#' Find gdal installation location
#' @details This function is basically the same as gdalUtils:::set_gdalInstallation, it has addiitonal functionality to be compatible with Ubuntu 14.04 Trusty
#' @noRd
findGdalInstallationPaths<-function (search_path = NULL, rescan = FALSE, ignore.full_scan = TRUE, verbose = FALSE) {
	gdal_python_utilities <- function(path) {
		if (missing(path))
			path <- gdal_path()
		sapply(path, list.files, pattern = "\\.py")
	}
	gdal_drivers <- function(path, verbose = FALSE) {
		if (missing(path))
			path <- gdal_path(checkValidity = TRUE)
		cmd <- file.path(path, "gdalinfo")
		cmd <- paste0("\"", cmd, "\"", " --formats")
		drivers_raw <- lapply(cmd, system, intern = TRUE)
		result <- vector(mode = "list", length(path))
		names(result) <- path
		for (i in seq_along(drivers_raw)) {
			drivers_raw[[i]] <- drivers_raw[[i]][-1]
			drivers <- strsplit(drivers_raw[[i]], ":")
			driver_names <- gsub("^ ", "", sapply(drivers, function(x) {x[2]}))
			driver_codes_perm <- strsplit(sapply(drivers, function(x) {x[1]}), "\\(")
			driver_codes <- gsub(" ", "", sapply(driver_codes_perm, function(x) {x[1]}), fixed = TRUE)
			driver_perm <- gsub("\\)", "", sapply(driver_codes_perm, function(x) {x[2]}))
			r <- w <- u <- v <- s <- rep(FALSE, length(driver_perm))
			r[grep(driver_perm, pattern = "r")] <- TRUE
			w[grep(driver_perm, pattern = "w")] <- TRUE
			u[grep(driver_perm, pattern = "\\+")] <- TRUE
			v[grep(driver_perm, pattern = "v")] <- TRUE
			s[grep(driver_perm, pattern = "s")] <- TRUE
			result[[i]] <- data.frame(format_code = driver_codes,
				read = r, write = w, update = u, virtualIO = v,
				subdatasets = s, format_name = driver_names
			)
		}
		return(result[[1]])
	}
	gdal_version <- function(path, newerThan = NULL, verbose = FALSE) {
		if (missing(path))
			path <- gdal_path()
		cmd <- normalizePath(list.files(path, "^gdalinfo$|^gdalinfo\\.exe$", full.names = TRUE))
		cmd <- paste0("\"", cmd, "\"", " --version")
		result <- lapply(cmd, system, intern = TRUE)
		res <- sapply(result, length)
		if (sum(res) != length(result))
		  message("Probably broken install of gdal at '", paste0(path[which(res != 1)], collapse = "' and '"), "'")
		result <- result[res == 1]
		date <- version <- vector(mode = "list", length = length(result))
		for (i in seq_along(result)) {
			ingd <- strsplit(result[[i]], ",")[[1]]
			version[[i]] <- gsub(ingd[1], pattern = "GDAL ", replacement = "")
			ind <- grep(ingd, pattern = "releas")
			date[[i]] <- as.character(as.Date(gsub(ingd[ind], pattern = " released ", replacement = ""), format = "%Y/%m/%d"))
		}
		if (!is.null(newerThan)) {
			test <- try(as.Date(newerThan), silent = TRUE)
			if (!inherits(test, "try-error")) {
				datein <- lapply(date, as.Date)
				res <- sapply(datein, ">=", as.Date(newerThan))
			} else {
				version <- gsub(tolower(version), pattern = "[a-z]", replacement = "")
				res <- sapply(version, strsplit, "\\.")
				newerThan <- strsplit(newerThan, "\\.")[[1]]
				for (i in seq_along(res)) {
					difs <- as.numeric(res[[i]]) - as.numeric(newerThan)
					difs <- sign(difs)
					if (sum(difs == -1) == 0) {
						res[[i]] <- TRUE
					} else {
						if (difs[1] < 0) {
							res[[i]] <- FALSE
						} else if (difs[1] > 0) {
							res[[i]] <- TRUE
						} else if (difs[1] == 0) {
							if (difs[2] < 0) {
								res[[i]] <- FALSE
							} else if (difs[2] > 0) {
								res[[i]] <- FALSE
							}
							else {
								if (difs[3] >= 0) {
									res[[i]] <- TRUE
								}
								else if (difs[3] < 0) {
									res[[i]] <- FALSE
								}
							}
						}
					}
				}
			}
			names(res) <- path
			return(res)
		}
		result <- as.data.frame(cbind(path = path[res == 1], version = version, date = date), stringsAsFactors = FALSE)
		return(result)
	}
	correctPath <- function(x) {
		if (!is.null(x)) {
			if (.Platform$OS.type == "windows") {
				x <- utils::shortPathName(x)
			} else {
				x <- path.expand(x)
			}
			x <- gsub(x, pattern = "\\\\", replacement = "/")
			ind <- substr(x, nchar(x), nchar(x)) != "/"
			x[ind] <- paste0(x[ind], "/")
		}
		return(x)
	}
	gdal_check_validity <- function(path) {
		checkValidity <- sapply(path, function(x) {
			cmd <- normalizePath(list.files(path = x, pattern = "^gdalinfo$|^gdalinfo\\.exe$", full.names = TRUE))
			if (length(cmd) == 0) {
				return(FALSE)
			} else {
			cmd <- paste0("\"", cmd, "\"", " --version")
			validity <- length(try(gdal <- system(cmd, intern = TRUE), silent = TRUE))
			return(as.logical(validity))
			}
			}
		)
	}
  gdal_path <- function(search_path = NULL, ignore.options = FALSE,
						ignore.which = FALSE, ignore.common = FALSE, ignore.full_scan = FALSE,
						force_full_scan = FALSE, checkValidity, search_path_recursive = FALSE,
						verbose = FALSE) {
		owarn <- getOption("warn")
		options(warn = -2)
		on.exit(options(warn = owarn))
		if (missing(checkValidity)) {
			if (is.null(getOption("gdalUtils_gdalPath"))) {
				checkValidity <- TRUE
			} else {
				checkValidity <- FALSE
			}
		}
		path <- NULL
		if (!force_full_scan) {
			if (!ignore.options) {
				if (verbose)
					message("Checking the gdalUtils_gdalPath option...")
				option_paths <- unlist(sapply(getOption("gdalUtils_gdalPath"), function(x) return(x$path)))
				if (!is.null(option_paths) && checkValidity) {
					option_paths_check <- gdal_check_validity(option_paths)
					option_paths <- option_paths[option_paths_check]
				}
				path <- c(path, option_paths)
			}
			if (!is.null(search_path) && length(path) == 0) {
				if (verbose)
					message("Checking the search path...")
				if (.Platform$OS == "unix") {
					search_paths <- list.files(path = search_path, pattern = "^gdalinfo$|^gdalinfo\\.exe$", recursive = search_path_recursive, full.names = TRUE)
				} else {
					search_paths <- list.files(path = search_path,
								 pattern = "^gdalinfo$|^gdalinfo\\.exe$",
								 recursive = search_path_recursive, full.names = TRUE)
				}
				if (length(search_paths) == 0) {
					search_paths <- NULL
				} else {
					search_paths <- normalizePath(dirname(search_paths))
				}
				if (!is.null(search_paths) && checkValidity) {
					search_paths_check <- gdal_check_validity(search_paths)
					search_paths <- search_paths[search_paths_check]
				}
				path <- c(path, search_paths)
			}
			if (!ignore.which && length(path) == 0) {
				if (verbose)
					message("Checking Sys.which...")
				Sys.which_path <- dirname(Sys.which("gdalinfo"))
				if (Sys.which_path == "")
					Sys.which_path <- NULL
				if (!is.null(Sys.which_path) && checkValidity) {
					Sys.which_path_check <- gdal_check_validity(Sys.which_path)
					Sys.which_path <- Sys.which_path[Sys.which_path_check]
				}
				path <- c(path, Sys.which_path)
			}
			if (!ignore.common && length(path) == 0) {
				if (verbose)
					message("Checking common locations...")
				if (.Platform$OS == "unix") {
					common_locations <- c("/usr/bin", "/usr/local/bin", "/Library/Frameworks/GDAL.framework", "/opt/local/bin")
				} else if (.Platform$OS == "windows") {
				common_locations <- c("C:\\Program Files", "C:\\Program Files (x86)", "C:\\OSGeo4W")
				}
				if (length(common_locations != 0)) {
					common_paths <- unlist(sapply(common_locations,
						function(x) {
							if (.Platform$OS == "unix") {
								search_common_paths <- list.files(path = x, pattern = "^gdalinfo$|^gdalinfo\\.exe$", recursive = TRUE, full.names = TRUE)
							} else {
								search_common_paths <- list.files(path = x, pattern = "^gdalinfo$|^gdalinfo\\.exe$", recursive = TRUE, full.names = TRUE)
							}
							if (length(search_common_paths) == 0) {
								return(search_common_paths)
							} else {
								return(normalizePath(dirname(search_common_paths)))
							}
						}))
					if (length(common_paths) == 0)
						common_paths <- NULL
					if (!is.null(common_paths) && checkValidity) {
						common_paths_check <- gdal_check_validity(common_paths)
						common_paths <- common_paths[common_paths_check]
					}
					path <- c(path, common_paths)
				}
			}
			if (!ignore.full_scan && length(path) == 0) {
				force_full_scan <- TRUE
			}
		}
		if (force_full_scan) {
			if (verbose)
				message("Scanning your root-dir for available GDAL installations,... This could take some time...")
			if (.Platform$OS == "unix") {
				root_dir <- "/"
			}
			if (.Platform$OS == "windows") {
				root_dir <- "C:\\"
			}
			if (.Platform$OS == "unix") {
				search_full_path <- list.files(path = root_dir, pattern = "^gdalinfo$|^gdalinfo\\.exe$", recursive = TRUE, full.names = TRUE)
			} else {
				search_full_path <- list.files(path = root_dir, pattern = "^gdalinfo$|^gdalinfo\\.exe$", recursive = TRUE, full.names = TRUE)
			}
			if (length(search_full_path) == 0)  {
				search_full_path <- NULL
			} else {
				search_full_path <- normalizePath(dirname(search_full_path))
			}
			if (!is.null(search_full_path) && checkValidity) {
				search_full_path_check <- gdal_check_validity(search_full_path)
				search_full_path <- search_full_path[search_full_path_check]
			}
			path <- c(path, search_full_path)
		}
		if (length(path) == 0) {
			return(NULL)
		} else {
			return(correctPath(unique(path)))
		}
	}
	gdal_installation <- function(return_versions = TRUE, return_drivers = TRUE,
									return_python_utilities = TRUE, sort_most_current = TRUE,
									rescan = FALSE, search_path = NULL, ignore.full_scan = FALSE,
									verbose = FALSE)
	{
		if (verbose)
			message("Scanning for GDAL installations...")
		path <- gdal_path(ignore.options = rescan, search_path = search_path, ignore.full_scan = ignore.full_scan, verbose = verbose)
		if (is.null(path))
			return(NULL)
		gdal_installation_results <- lapply(path, function(x, return_drivers, return_python_utilities, return_versions) {
			result <- list(path = x)
			if (return_versions) {
				version <- gdal_version(x)
				result$version <- unlist(version$version)
				result$date <- unlist(version$date)
			}
			if (return_drivers) {
				result$drivers <- gdal_drivers(x)
			}
			if (return_python_utilities) {
				result$python_utilities <- gdal_python_utilities(x)
			}
			return(result)
		}, return_drivers = return_drivers, return_python_utilities = return_python_utilities, return_versions = return_versions)
		if (sort_most_current) {
			versions <- unlist(sapply(gdal_installation_results, function(x) return(x$date)))
			gdal_installation_results <- gdal_installation_results[order(as.Date(unlist(versions)), decreasing = TRUE)]
		}
		return(gdal_installation_results)
	}
	if (is.null(getOption("gdalUtils_gdalPath")))
		rescan <- TRUE
	gdal_installation_out <- gdal_installation(search_path = search_path,
												rescan = rescan,
												ignore.full_scan = ignore.full_scan,
												verbose = verbose
	)
	options(gdalUtils_gdalPath = gdal_installation_out)
	if (is.null(getOption("gdalUtils_gdalPath"))) {
		warning("No GDAL installation found. Please install 'gdal' before continuing:\n\t- www.gdal.org (no HDF4 support!)\n\t- www.trac.osgeo.org/osgeo4w/ (with HDF4 support RECOMMENDED)\n\t- www.fwtools.maptools.org (with HDF4 support)\n")
		if (ignore.full_scan)
			warning("If you think GDAL is installed, please run:\ngdal_setInstallation(ignore.full_scan=FALSE)")
	} else {
		if (verbose)
		message("GDAL version ", unlist(getOption("gdalUtils_gdalPath")[[1]]$version))
	}
}

# merge list of RapResults into a single object
mergeRapResults<-function(x) {
	x <-RapResults(
		summary=ldply(x, slot, name="summary"),
		selections=do.call(rbind, llply(x, slot, name="selections")),
		amount.held=do.call(rbind, llply(x, slot, name="amount.held")),
		space.held=do.call(rbind, llply(x, slot, name="space.held")),
		logging.file=sapply(x, slot, name="logging.file")
	)
	x@summary$Run_Number<-seq_len(nrow(x@summary))
	return(x)
}

#' Read RAP results
#'
#' This function reads files output from Gurobi and returns a \code{RapResults} object.
#'
#' @param opts \code{RapReliableOpts} or \code{RapUnreliableOpts} object
#' @param data \code{RapData} object
#' @param model.list \code{list} object containing Gurobi model data
#' @param logging.file \code{character} Gurobi log files.
#' @param solution.list \code{list} object containing Gurobi solution data.
#' @param verbose \code{logical} print progress messages? Defaults to \code{FALSE}.
#' @keywords internal
#' @return \code{RapResults} object
#' @seealso \code{\link{RapReliableOpts}}, \code{\link{RapUnreliableOpts}}, \code{\link{RapData}}, \code{\link{RapResults}}.
read.RapResults <- function(opts, data, model.list, logging.file, solution.list, verbose=FALSE) {
  x <- rcpp_extract_model_object(
    opts, inherits(opts, 'RapUnreliableOpts'),
    data, model.list, logging.file, solution.list, verbose
  )
	x@.cache <- new.env()
	return(x)
}

#' Compare Rap objects
#'
#' This function checks objects to see if they share the same input data.
#'
#' @param x \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#' @param y \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#' @return \code{logical} are the objects based on the same data?
#' @keywords internal
#' @seealso \code{\link{RapData-class}}, \code{\link{RapUnsolved-class}}, \code{\link{RapSolved-class}}.
setGeneric("is.comparable", function(x, y) standardGeneric("is.comparable"))

#' Basemap
#'
#' This function retrieves google map data for planning units. The google map data is cached to provide fast plotting capabilities.
#'
#' @param x \code{RapData}, \code{RapUnsolved}, \code{RapSolved} object.
#' @param basemap \code{character} type of base map to display. Valid names are "roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid".
#' @param grayscale \code{logical} should base map be gray scale?
#' @param force.reset \code{logical} ignore data in cache? Setting this as ignore will make function slower but may avoid bugs in cache system.
#' @return \code{list} with google map data.
#' @keywords internal
#' @seealso \code{\link[RgoogleMaps]{GetMap.bbox}}, \code{\link{plot}}.
basemap<-function(x, basemap="hybrid", grayscale=FALSE, force.reset=FALSE) {UseMethod("basemap")}

#' Test if hash is cached in a Rap object
#'
#' Tests if hash is cached in Rap object.
#'
#' @param x \code{RapData} or \code{RapResults} object
#' @param name \code{character} hash.
#' @note caches are implemented using environments, the hash is used as the name of the object in the environment.
#' @return \code{logical} Is it cached?
#' @keywords internal
setGeneric("is.cached", function(x,name) standardGeneric("is.cached"))

#' Get and set cache Methods
#'
#' Getter and setter methods for caches in RapData and RapResults object.
#'
#' @param x \code{RapData} or \code{RapResults} object
#' @param name \code{character} hash.
#' @param y if \code{ANY} this object gets cached with name, else if \code{missing} the object hashed at name gets returned.
#' @note caches are implemented using environments, the hash is used as the name of the object in the environment.
#' @return \code{ANY} or \code{NULL} depends on \code{y} argument.
#' @keywords internal
setGeneric("cache", function(x, name, y) standardGeneric("cache"))


#' Plot a 1-dimensional attribute space
#' @noRd
spacePlot.1d<-function(pu, dp, pu.color.palette, main) {
  # create X2 vars
  pu$X2<-0
  dp$X2<-0
  # create colors
  if (length(pu.color.palette)==1)
		pu.color.palette<-brewerCols(seq(0,1,0.25), pu.color.palette, 4)
  # make plot
  ggplot() +
  geom_point(
		aes_string(x="X1", y="X2", alpha="weights"),
    data=dp,
    color='darkblue',
    size=5,
    position=position_jitter(width=0, height=5)
  ) +
  scale_alpha_continuous(
    name='Demand point weight'
  ) +
  geom_point(
		aes_string(x="X1", y="X2", color="status", size="status"),
    data=pu,
    position=position_jitter(width=0, height=5)
  ) +
  scale_color_manual(
    name='Planning unit status',
    values=c(
      "Locked Out"=pu.color.palette[4],
      "Not Selected"=pu.color.palette[1],
      "Selected"=pu.color.palette[2],
      "Locked In"=pu.color.palette[3]
    )
  ) +
  scale_size_manual(
    values=c(
      "Locked Out"=2,
      "Not Selected"=2,
      "Selected"=4.5,
      "Locked In"=4.5
    ),
    guide=FALSE
  ) +
  theme_classic() +
  coord_equal() +
  theme(
    legend.position='right',
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank(),
    axis.line.y=element_line(),
    axis.line.x=element_line()
  ) +
  ggtitle(main) +
  xlab('Dimension 1') +
  ylab('')
}

#' Plot a 2-d attribute space
#' @noRd
spacePlot.2d<-function(pu, dp, pu.color.palette, main) {
  # create colors
  if (length(pu.color.palette)==1)
		pu.color.palette<-brewerCols(seq(0,1,0.25), pu.color.palette, 4)
  # make plot
  ggplot() +
  geom_point(
		aes_string(x="X1", y="X2", alpha="weights"),
    data=dp,
    color='darkblue',
    size=5
  ) +
  scale_alpha_continuous(
    name='Demand point weight'
  ) +
  geom_point(
		aes_string(x="X1", y="X2", color="status", size="status"),
    data=pu
  ) +
  scale_color_manual(
    name='Planning unit status',
    values=c(
      "Locked Out"=pu.color.palette[4],
      "Not Selected"=pu.color.palette[1],
      "Selected"=pu.color.palette[2],
      "Locked In"=pu.color.palette[3]
    )
  ) +
  scale_size_manual(
    values=c(
      "Locked Out"=2,
      "Not Selected"=2,
      "Selected"=4.5,
      "Locked In"=4.5
    ),
    guide=FALSE
  ) +
  theme_classic() +
  coord_equal() +
  theme(
    legend.position='right',
    axis.line.y=element_line(),
    axis.line.x=element_line()
    
  ) +
  ggtitle(main) +
  xlab('Dimension 1') +
  ylab('Dimension 2')
}

#' Plot a 3-d attribute space
#' @noRd
spacePlot.3d<-function(pu, dp, pu.color.palette, main) {
	# check if rgl is installed
  if (!'rgl' %in% unlist(lapply(.libPaths(), dir), recursive=FALSE, use.names=FALSE))
		stop('The rgl R package must be installed to visualise 3d attribute spaces')
  # create frame
  rgl::open3d()
  # add pu points
  if (length(pu.color.palette)==1)
		pu.color.palette<-brewerCols(seq(0,1,0.25), pu.color.palette, 4)
  pu.cols<-character(nrow(pu))
  pu.cols[which(pu$status=='Not Selected')]<-pu.color.palette[1]
  pu.cols[which(pu$status=='Selected')]<-pu.color.palette[2]
  pu.cols[which(pu$status=='Locked In')]<-pu.color.palette[3]
  pu.cols[which(pu$status=='Locked Out')]<-pu.color.palette[4]
  rgl::points3d(as.matrix(pu[,1:3]), col=pu.cols)
  # add dp points
  dp.cols<-alpha(rep('darkblue', nrow(dp)), affineTrans(dp$weights, min(dp$weights), max(dp$weights), 0.1, 1))
  rgl::points3d(as.matrix(dp[,1:3]), col=dp.cols)
  # add title
  rgl::title3d(main)
}

#' General argument parsing function
#' @noRd
parseArgs<-function(fn, object=NULL, skip=-1, ...) {
  if (!is.null(object))
    fn<-paste0(fn, '.', class(object))
  ellipses.args<-list(...)
  fn.args<-names(formals(fn))
  if (!is.null(skip))
		fn.args<-fn.args[skip]
  return(
    ellipses.args[intersect(
      names(ellipses.args),
      fn.args
    )]
  )
}

#' Alternative argument parsing function
#' @noRd
parseArgs2<-function(args, ...) {
  ellipses.args<-list(...)
  return(
    ellipses.args[intersect(
      names(ellipses.args),
      args
    )]
  )
}

#' Create an empty PolySet object
#' @noRd
emptyPolySet<-function() {
  return(
    structure(
      list(PID = integer(0), SID = integer(0), POS = integer(0),
        X = numeric(0), Y = numeric(0)), .Names = c("PID", "SID",
        "POS", "X", "Y"),
      row.names = integer(0),
      class = c("PolySet", "data.frame")
    )
  )
}

#' Calculate distances between points using URAP
#' @noRd
urap.squared.distance <- function(x, y, y.weights=rep(1, nrow(y))) {
	expect_is(x, 'matrix')
	expect_is(y, 'matrix')
	expect_is(y.weights, 'numeric')
	expect_equal(nrow(y), length(y.weights))
	expect_true(all(is.finite(c(x))))
	expect_true(all(is.finite(c(y))))
	expect_true(all(is.finite(c(y.weights))))
	expect_true(all(y.weights >=  0))
	rcpp_squared_distance(x, y, y.weights)
}

#' Calculate distances between points using RRAP
#' @noRd
rrap.squared.distance <- function(pu.coordinates, pu.probabilities, dp.coordinates, dp.weights, failure.distance, maximum.r.level=as.integer(length(pu.probabilities))) {
	# data integreity checks
	expect_is(pu.coordinates, 'matrix')
	expect_is(dp.coordinates, 'matrix')
	expect_is(pu.probabilities, 'numeric')
	expect_is(dp.weights, 'numeric')
	expect_is(failure.distance, 'numeric')
	expect_is(maximum.r.level, 'integer')
	expect_length(failure.distance, 1)
	expect_length(maximum.r.level, 1)
	expect_equal(nrow(pu.coordinates), length(pu.probabilities))
	expect_equal(nrow(dp.coordinates), length(dp.weights))
	expect_equal(ncol(dp.coordinates), ncol(pu.coordinates))
	expect_true(all(is.finite(c(dp.weights))))
	expect_true(all(is.finite(c(pu.probabilities))))
	expect_true(all(is.finite(c(pu.coordinates))))
	expect_true(all(is.finite(c(dp.coordinates))))
	expect_true(all(is.finite(c(failure.distance))))
	expect_true(all(is.finite(c(maximum.r.level))))
	expect_true(maximum.r.level <= nrow(pu.coordinates))
	expect_true(maximum.r.level >=  1)
	expect_true(failure.distance >=  0)
	expect_true(nrow(pu.coordinates)>=1)
	expect_true(nrow(dp.coordinates)>=1)
	# main processing
	rcpp_rrap_squared_distance(pu.coordinates, pu.probabilities, dp.coordinates, dp.weights, failure.distance, maximum.r.level)
}

#' Dump object from model cache 
#' @noRd
dump_object <- function(x, mode = c('numeric', 'integer', 'character')) {
	expect_is(x, 'externalptr')
	mode <- match.arg(mode)
	if (mode == 'numeric') {
		return(rcpp_dump_numeric_object(x))
	}
	if (mode == 'integer') {
		return(rcpp_dump_integer_object(x))
	}
	if (mode == 'character') {
		return(rcpp_dump_character_object(x))
	}
}