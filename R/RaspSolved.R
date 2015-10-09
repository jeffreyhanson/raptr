#' @include RcppExports.R raspr-internal.R misc.R generics.R RaspOpts.R RaspData.R RaspUnsolved.R RaspResults.R
NULL


#' RaspSolved: An S4 class to represent RASP inputs and outputs
#'
#' This class is used to store RASP input and output data in addition to input parameters.
#'
#' @slot opts \code{RaspOpts} object used to store input parameters.
#' @slot data \code{RasprData} object used to store input data.
#' @slot results \code{RaspResults} object used to store results.
#' @export
#' @seealso \code{\link{RaspOpts-class}}, \code{\link{RaspData-class}}, \code{\link{RaspResults-class}}.
setClass("RaspSolved",
	representation(
		opts="RaspOpts",
		gurobi="GurobiOpts",
		data="RaspData",
		results="RaspResults"
	)
)

setClassUnion("RaspUnsolvedOrSolved", c("RaspSolved", "RaspUnsolved"))


#' Create new RaspSolved object
#'
#' This function creates a \code{RaspSolved} object using a \code{RaspUnsolved} object and a \code{RaspResults} object.
#'
#' @param unsolved \code{RaspUnsolved} object.
#' @param results \code{RaspResults} object.
#' @return \code{RaspSolved} object.
#' @export
#' @seealso \code{\link{RaspSolved-class}}, \code{\link{RaspResults-class}}.
RaspSolved<-function(unsolved, results) {
	return(new("RaspSolved", opts=unsolved@opts, gurobi=unsolved@gurobi, data=unsolved@data, results=results))
}

#' @rdname solve
#' @inheritParams solve
#' @export
solve.RaspSolved<-function(x, wd=tempdir(), clean=TRUE, force_reset=FALSE) {
	if (!force_reset)
		stop("This object already has solutions. Use force_reset=TRUE to force recalculation of solutions.")
	return(solve(RaspUnsolved(opts=x@opts,gurobi=x@gurobi, data=x@data), wd, clean))
}

#' @rdname selections
#' @inheritParams selections
#' @export
selections.RaspSolved<-function(x, y=0) {
	return(selections.RaspResults(x@results, y))
}

#' @rdname score
#' @inheritParams score
#' @export
score.RaspSolved<-function(x, y=0) {
	return(score.RaspResults(x@results, y))
}

#' @export
summary.RaspSolved<-function(x) {
	return(summary.RaspResults(x@results))
}

#' @rdname log.file
#' @inheritParams log.file
#' @export
log.file.RaspSolved<-function(x) {
	log.file.RaspResults(x@results)
}

#' @rdname model.file
#' @inheritParams model.file
#' @export
model.file.RaspSolved<-function(x) {
	model.file.RaspResults(x@results)
}

#' @rdname solution.file
#' @inheritParams model.file
#' @export
solution.file.RaspSolved<-function(x) {
	solution.file.RaspResults(x@results)
}

#' @export
#' @inheritParams amount.held
#' @rdname amount.held
amount.held.RaspSolved<-function(x, y=0) {
	return(amount.held.RaspResults(x@results, y))
}

#' @rdname space.held
#' @inheritParams space.held
#' @export
space.held.RaspSolved<-function(x, y=0) {
	return(space.held.RaspResults(x@results, y))
}

#' @export
print.RaspSolved<-function(x) {
	cat("Parameters\n")
	print.RaspOpts(x@opts, FALSE)
	cat("Solver settings\n")
	print.GurobiOpts(x@gurobi, FALSE)
	cat("Data\n")
	print.RaspData(x@data, FALSE)
	cat("Results\n")
	print.RaspResults(x@results, FALSE)
}

# ' @export
setMethod(
	'show',
	'RaspSolved',
	function(object)
		print.RaspSolved(object)
)

#' @rdname is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="RaspUnsolvedOrSolved", y="RaspUnsolvedOrSolved"),
	function(x,y) {
		return(is.comparable(x@data, y@data))
	}
)

#' @rdname is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="RaspData", y="RaspUnsolvedOrSolved"),
	function(x,y) {
		return(is.comparable(x, y@data))
	}
)

#' @rdname is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="RaspUnsolvedOrSolved", y="RaspData"),
	function(x,y) {
		return(is.comparable(x@data, y))
	}
)

#' @rdname basemap
#' @export
basemap.RaspSolved<-function(x, basemap="none", grayscale=FALSE, force_reset=FALSE) {
	return(basemap.RaspData(x@data, basemap, grayscale, force_reset))
}


#' @export
plot.RaspSolved<-function(x, ...) {
	plotRasp(x, ...)
}

#' @rdname plotRasp
#' @inheritParams plotRasp
#' @export
setMethod(
	"plotRasp",
	signature(x="RaspSolved",y="numeric"),
	function(x, y, basemap="none", colramp="Greens", lockedincol="#000000FF", lockedoutcol="#D7D7D7FF", alpha=ifelse(basemap=="none",1,0.7), grayscale=FALSE, force_reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.RaspData(x@data, basemap, grayscale, force_reset)
		# main processing
		if (y==0)
			y<-x@results@best
		if (is.numeric(y))
			stopifnot(y<=nrow(x@results@selections))
		values<-x@results@selections[y,]
		cols<-character(length(values))
		cols[which(x@data@pu$status==2)]<-lockedincol
		cols[which(x@data@pu$status==3)]<-lockedoutcol
		cols[which(x@data@pu$status<2)]<-brewerCols(values[which(x@data@pu$status<2)], colramp, alpha, n=2)
		prettyGeoplot(
			x@data@polygons,
			cols,
			basemap,
			ifelse(y==x@results@best, paste0("Best Solution (",y,")"), paste0("Solution (",y,")")),
			categoricalLegend(c(lockedoutcol,brewerCols(c(0,1),colramp,alpha,n=2),lockedincol),c("Locked Out", "Not Selected", "Selected", "Locked In")),
			beside=FALSE
		)
	}
)

#' @rdname plotRasp
#' @inheritParams plotRasp
#' @export
setMethod(
	"plotRasp",
	signature(x="RaspSolved",y="missing"),
	function(x, y, basemap="none", colramp="PuBu", lockedincol="#000000FF", lockedoutcol="#D7D7D7FF", alpha=ifelse(basemap=="none",1,0.7), grayscale=FALSE, force_reset=FALSE) {
		# check for issues
		match.arg(basemap, c("none", "roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid"))
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.RaspData(x@data, basemap, grayscale, force_reset)	
		# main processing
		if (force_reset || !is.cached(x@results, "selectionfreqs")) {
			cache(x@results, "selectionfreqs", colMeans(x@results@selections))
		}
		values<-cache(x@results,"selectionfreqs")[which(x@data@pu$status<2)]
		cols<-brewerCols(rescale(cache(x@results,"selectionfreqs"),from=range(values),to=c(0,1)), pal=colramp, alpha=alpha)
		cols[which(x@data@pu$status==2)]<-lockedincol
		cols[which(x@data@pu$status==3)]<-lockedoutcol
		prettyGeoplot(
			x@data@polygons,
			cols,
			basemap,
			"Planning unit selection frequency",
			continuousLegend(values,colramp,posx=c(0.3, 0.4),posy=c(0.1, 0.9)),
			beside=TRUE
		)
	}
)

#' @rdname plotRasp
#' @inheritParams plotRasp
#' @export
setMethod(
	"plotRasp",
	signature(x="RaspSolved",y="RaspSolved"),
	function(x, y, i=NULL, j=i, basemap="none", colramp=ifelse(is.null(i), "RdYlBu", "Accent"), xlockedincol="#000000FF", xlockedoutcol="#D7D7D7FF", ylockedincol="#FFFFFFFF", ylockedoutcol="#D7D7D7FF", alpha=ifelse(basemap=="none",1,0.7), grayscale=FALSE, force_reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))
		stopifnot(is.comparable(x,y))
		if (is.numeric(i))
			stopifnot(i <= nrow(x@results@selections))
		if (is.numeric(j))
			stopifnot(j <= nrow(y@results@selections))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.RaspData(x@data, basemap, grayscale, force_reset)	
		# main processing
		cols<-character(nrow(x@data@pu))
		if (is.null(i) || is.null(j)) {
			cols[which(x@data@pu$status==2)]<-xlockedincol
			cols[which(y@data@pu$status==2)]<-ylockedincol
			cols[which(x@data@pu$status==3)]<-xlockedoutcol
			cols[which(y@data@pu$status==3)]<-ylockedoutcol
		
			if (force_reset || !is.cached(x@results, "selectionfreqs"))
				cache(x@results, "selectionfreqs", colMeans(x@results@selections))
			xsc<-cache(x@results, "selectionfreqs")[which(nchar(cols)==0)]
			if (force_reset || !is.cached(y@results, "selectionfreqs"))
				cache(y@results, "selectionfreqs", colMeans(y@results@selections))
			ysc<-cache(y@results, "selectionfreqs")[which(nchar(cols)==0)]
			values<-xsc-ysc
			cols[which(nchar(cols)==0)]<-brewerCols(rescale(values,to=c(0,1)), colramp, alpha)
			prettyGeoplot(
				x@data@polygons,
				cols,
				basemap,
				"Difference in selection frequencies",
				fun<-continuousLegend(
					values,
					colramp,
					posx=c(0.3, 0.4),posy=c(0.1, 0.9),
					center=TRUE,
					endlabs=c('+X','+Y')
				),
				beside=TRUE
			)
		} else {
			if (i==0)
				i<-x@results@best
			if (j==0)
				j<-y@results@best
			cols2<-brewerCols(seq(0,1,length.out=4),colramp,alpha,n=4)
			
			cols[which(x@results@selections[i,]==1 & y@results@selections[j,]==0)]<-cols2[1]
			cols[which(x@results@selections[i,]==0 & y@results@selections[j,]==1)]<-cols2[2]
			cols[which(x@results@selections[i,]==1 & y@results@selections[j,]==1)]<-cols2[3]
			cols[which(x@results@selections[i,]==0 & y@results@selections[j,]==0)]<-cols2[4]

			cols[which(x@data@pu$status==2)]<-xlockedincol
			cols[which(y@data@pu$status==2)]<-ylockedincol
			cols[which(x@data@pu$status==3)]<-xlockedoutcol
			cols[which(y@data@pu$status==3)]<-ylockedoutcol
			
			xrepr<-ifelse(i==x@results@best, '(best)', paste0('(',i,')'))
			yrepr<-ifelse(i==y@results@best, '(best)', paste0('(',j,')'))

			prettyGeoplot(
				x@data@polygons,
				cols,
				basemap,
				paste0("Difference in X solution ",i,ifelse(i==x@results@best, " (best)", ""), " and Y solution ",j, ifelse(j==y@results@best, " (best)", "")),
				categoricalLegend(
					c(cols2,xlockedincol,ylockedincol,xlockedoutcol,ylockedoutcol),
					c("Selected in X",  "Selected in Y", "Both", "Neither", "Locked in X", "Locked in Y", paste("Locked out X"), paste("Locked out Y")),
					ncol=4
				),
				beside=FALSE
			)
		}
	}
)

