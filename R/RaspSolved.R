#' @include RcppExports.R raspr-internal.R misc.R generics.R RaspReliableOpts.R RaspUnreliableOpts.R RaspData.R RaspUnsolved.R RaspResults.R
NULL

#' RaspSolved: An S4 class to represent RASP inputs and outputs
#'
#' This class is used to store RASP input and output data in addition to input parameters.
#'
#' @slot opts \code{RaspReliableOpts} or \code{RaspUnreliableOpts} object used to store input parameters.
#' @slot sovler \code{GurobiOpts} object used to store Gurobi parameters.
#' @slot data \code{RaspData} object used to store input data.
#' @slot results \code{RaspResults} object used to store results.
#' @export
#' @seealso \code{\link{RaspReliableOpts-class}}, \code{\link{RaspUnreliableOpts-class}}, \code{\link{RaspData-class}}, \code{\link{RaspResults-class}}.
setClass("RaspSolved",
	representation(
		opts="RaspOpts",
		solver="SolverOpts",
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
#' @examples
#' \dontrun{
#' # load data
#' data(sim_ru)
#' # make RaspSolved object
#' sim_rs <- raspr::solve(sim_ru)
#' }
RaspSolved<-function(unsolved, results) {
	return(new("RaspSolved", opts=unsolved@opts, solver=unsolved@solver, data=unsolved@data, results=results))
}

#' @describeIn solve
#' @export
setMethod(
	'solve',
	'RaspSolved',
	function(x, verbose=FALSE, force.reset=FALSE) {
		if (!force.reset)
			stop("This object already has solutions. Use force.reset=TRUE to force recalculation of solutions.")
		return(solve(RaspUnsolved(opts=x@opts,solver=x@sovler, data=x@data), wd, clean))
	}
)

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

#' @method summary RaspSolved
#' @export
summary.RaspSolved<-function(object, ...) {
	return(summary.RaspResults(object@results))
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

#' @rdname logging.file
#' @inheritParams logging.file
#' @export
logging.file.RaspSolved<-function(x, y=0) {
	return(logging.file.RaspResults(x@results, y))
}


#' @method print RaspSolved
#' @rdname print
#' @export
print.RaspSolved<-function(x, ...) {
	cat("Parameters\n")
	print(x@opts, header=FALSE)
	cat("Solver settings\n")
	print(x@solver, header=FALSE)
	cat("Data\n")
	print.RaspData(x@data, header=FALSE)
	cat("Results\n")
	print.RaspResults(x@results, header=FALSE)
}

#' @rdname spp.subset
#' @method spp.subset RaspSolved
#' @export
spp.subset.RaspSolved<-function(x, species) {
	return(
		spp.subset(
			RaspUnsolved(
				pu=x@pu,
				species=x@species,
				attriubte.spaces=x@attriubte.spaces,
				boundary=x@boundary,
				polygons=x@polygons
			),
			species
		)
	)
}

#' @rdname pu.subset
#' @method pu.subset RaspSolved
#' @export
pu.subset.RaspSolved<-function(x, pu) {
	return(
		pu.subset(
			RaspUnsolved(
				pu=x@pu,
				species=x@species,
				attriubte.spaces=x@attriubte.spaces,
				boundary=x@boundary,
				polygons=x@polygons
			),
			pu
		)
	)
}

#' @describeIn show
#' @export
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
basemap.RaspSolved<-function(x, basemap="none", grayscale=FALSE, force.reset=FALSE) {
	return(basemap.RaspData(x@data, basemap, grayscale, force.reset))
}

#' @describeIn plot
#' @export
setMethod(
	"plot",
	signature(x="RaspSolved",y="numeric"),
	function(x, y, basemap="none", color.palette="Greens", locked.in.color="#000000FF", locked.out.color="#D7D7D7FF", alpha=ifelse(basemap=="none",1,0.7), grayscale=FALSE, force.reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(color.palette, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.RaspData(x@data, basemap, grayscale, force.reset)
		# main processing
		if (y==0)
			y<-x@results@best
		if (is.numeric(y))
			stopifnot(y<=nrow(x@results@selections))
		values<-x@results@selections[y,]
		cols<-character(length(values))
		cols[which(x@data@pu$status==2)]<-locked.in.color
		cols[which(x@data@pu$status==3)]<-locked.out.color
		cols[which(x@data@pu$status<2)]<-brewerCols(values[which(x@data@pu$status<2)], color.palette, alpha, n=2)
		prettyGeoplot(
			x@data@polygons,
			cols,
			basemap,
			ifelse(y==x@results@best, paste0("Best Solution (",y,")"), paste0("Solution (",y,")")),
			categoricalLegend(c(locked.out.color,brewerCols(c(0,1),color.palette,alpha,n=2),locked.in.color),c("Locked Out", "Not Selected", "Selected", "Locked In")),
			beside=FALSE
		)
	}
)

#' @describeIn plot
#' @export
setMethod(
	"plot",
	signature(x="RaspSolved",y="missing"),
	function(x, y, basemap="none", color.palette="PuBu", locked.in.color="#000000FF", locked.out.color="#D7D7D7FF", alpha=ifelse(basemap=="none",1,0.7), grayscale=FALSE, force.reset=FALSE) {
		# check for issues
		match.arg(basemap, c("none", "roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid"))
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(color.palette, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.RaspData(x@data, basemap, grayscale, force.reset)
		# main processing
		if (force.reset || !is.cached(x@results, "selectionfreqs")) {
			cache(x@results, "selectionfreqs", colMeans(x@results@selections))
		}
		values<-cache(x@results,"selectionfreqs")[which(x@data@pu$status<2)]
		cols<-character(length(cache(x@results,"selectionfreqs")))
		cols[which(x@data@pu$status<2)]<-brewerCols(rescale(values,from=range(values),to=c(0,1)), pal=color.palette, alpha=alpha)
		cols[which(x@data@pu$status==2)]<-locked.in.color
		cols[which(x@data@pu$status==3)]<-locked.out.color
		prettyGeoplot(
			x@data@polygons,
			cols,
			basemap,
			"Planning unit selection frequency",
			continuousLegend(values,color.palette,posx=c(0.3, 0.4),posy=c(0.1, 0.9)),
			beside=TRUE
		)
	}
)

#' @describeIn plot
#' @export
setMethod(
	"plot",
	signature(x="RaspSolved",y="RaspSolved"),
	function(x, y, i=NULL, j=i, basemap="none", color.palette=ifelse(is.null(i), "RdYlBu", "Accent"), x.locked.in.color="#000000FF", x.locked.out.color="#D7D7D7FF", y.locked.in.color="#FFFFFFFF", y.locked.out.color="#D7D7D7FF", alpha=ifelse(basemap=="none",1,0.7), grayscale=FALSE, force.reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(color.palette, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))
		stopifnot(is.comparable(x,y))
		if (is.numeric(i))
			stopifnot(i <= nrow(x@results@selections))
		if (is.numeric(j))
			stopifnot(j <= nrow(y@results@selections))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.RaspData(x@data, basemap, grayscale, force.reset)
		# main processing
		cols<-character(nrow(x@data@pu))
		if (is.null(i) || is.null(j)) {
			cols[which(x@data@pu$status==2)]<-x.locked.in.color
			cols[which(y@data@pu$status==2)]<-y.locked.in.color
			cols[which(x@data@pu$status==3)]<-x.locked.out.color
			cols[which(y@data@pu$status==3)]<-y.locked.out.color

			if (force.reset || !is.cached(x@results, "selectionfreqs"))
				cache(x@results, "selectionfreqs", colMeans(x@results@selections))
			xsc<-cache(x@results, "selectionfreqs")[which(nchar(cols)==0)]
			if (force.reset || !is.cached(y@results, "selectionfreqs"))
				cache(y@results, "selectionfreqs", colMeans(y@results@selections))
			ysc<-cache(y@results, "selectionfreqs")[which(nchar(cols)==0)]
			values<-xsc-ysc
			col.pos=which(nchar(cols)==0)
			cols[col.pos]<-brewerCols(rescale(values,to=c(0,1)), color.palette, alpha)
			# determine legend function
			if (length(unique(round(values, 5)))>1) {
				legend.fun=continuousLegend(
					values,
					color.palette,
					posx=c(0.3, 0.4),posy=c(0.1, 0.9),
					center=TRUE,
					endlabs=c('+X','+Y')
				)
				beside=TRUE
			} else {
				# create legend entries
				leg.cols=c(cols[col.pos[1]])
				leg.labs=c(values[1])
				if (any(x@data@pu$status==2)) {
					leg.cols=c(leg.cols, x.locked.in.color)
					leg.labs=c(leg.labs, "Locked in X")
				}
				if (any(x@data@pu$status==3)) {
					leg.cols=c(leg.cols, x.locked.out.color)
					leg.labs=c(leg.labs, "Locked out X")
				}
				if (any(y@data@pu$status==2)) {
					leg.cols=c(leg.cols, y.locked.in.color)
					leg.labs=c(leg.labs, "Locked in Y")
				}
				if (any(y@data@pu$status==3)) {
					leg.cols=c(leg.cols, y.locked.out.color)
					leg.labs=c(leg.labs, "Locked out Y")
				}
				# create legend function
				legend.fun=categoricalLegend(
					leg.cols,
					leg.labs,
					ncol=1
				)
				beside=FALSE
			}
			prettyGeoplot(
				x@data@polygons,
				cols,
				basemap,
				"Difference in selection frequencies",
				fun=legend.fun,
				beside=beside
			)
		} else {
			if (i==0)
				i<-x@results@best
			if (j==0)
				j<-y@results@best
			cols2<-brewerCols(seq(0,1,length.out=4),color.palette,alpha,n=4)

			cols[which(x@results@selections[i,]==1 & y@results@selections[j,]==0)]<-cols2[1]
			cols[which(x@results@selections[i,]==0 & y@results@selections[j,]==1)]<-cols2[2]
			cols[which(x@results@selections[i,]==1 & y@results@selections[j,]==1)]<-cols2[3]
			cols[which(x@results@selections[i,]==0 & y@results@selections[j,]==0)]<-cols2[4]

			cols[which(x@data@pu$status==2)]<-x.locked.in.color
			cols[which(y@data@pu$status==2)]<-y.locked.in.color
			cols[which(x@data@pu$status==3)]<-x.locked.out.color
			cols[which(y@data@pu$status==3)]<-y.locked.out.color

			xrepr<-ifelse(i==x@results@best, '(best)', paste0('(',i,')'))
			yrepr<-ifelse(i==y@results@best, '(best)', paste0('(',j,')'))

			prettyGeoplot(
				x@data@polygons,
				cols,
				basemap,
				paste0("Difference in X solution ",i,ifelse(i==x@results@best, " (best)", ""), " and Y solution ",j, ifelse(j==y@results@best, " (best)", "")),
				categoricalLegend(
					c(cols2,x.locked.in.color,y.locked.in.color,x.locked.out.color,y.locked.out.color),
					c("Selected in X",  "Selected in Y", "Both", "Neither", "Locked in X", "Locked in Y", paste("Locked out X"), paste("Locked out Y")),
					ncol=4
				),
				beside=FALSE
			)
		}
	}
)

#' @rdname update
#' @method update RaspSolved
#' @export
update.RaspSolved<-function(object, ..., solve=TRUE) {
	object<-RaspUnsolved(
		opts=update(object@opts, ..., ignore.extra=TRUE),
		solver=update(object@solver, ..., ignore.extra=TRUE),
		data=update(object@data, ..., ignore.extra=TRUE)
	)
	if (solve)
		object<-solve(object)
	return(object)
}

#' @rdname spp.plot
#' @method spp.plot RaspSolved
#' @export
spp.plot.RaspSolved<-function(x, y, basemap="none", color.palette="YlGnBu", alpha=ifelse(basemap=="none", 1, 0.7), grayscale=FALSE, force.reset=FALSE) {
	spp.plot(x@data, y, basemap, color.palette, alpha, grayscale, force.reset)
}

#' @rdname space.plot
#' @method space.plot RaspSolved
#' @export
space.plot.RaspSolved<-function(
	x,
	y,
	space=1,
	pu.color.palette='RdYlGn',
	locked.in.color="#000000FF",
	locked.out.color="#D7D7D7FF"
) {
	stop('Function not finished')
}

#' @rdname amount.target
#' @method amount.target RaspSolved
#' @export
amount.target.RaspSolved<-function(x) {
	amount.target.RaspData(x@data)
}

#' @rdname space.target
#' @method space.target RaspSolved
#' @export
space.target.RaspSolved<-function(x) {
	space.target.RaspData(x@data)
}
