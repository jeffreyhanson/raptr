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
selections.RaspSolved<-function(x, y=NULL) {
	return(selection.RaspResults(x@results, y))
}

#' @rdname score
#' @inheritParams score
#' @export
score.RaspSolved<-function(x, y=NULL) {
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

#' @export
#' @inheritParams amount.held
#' @rdname amount.held
amount.held.RaspSolved<-function(x, y=NULL) {
	return(amount.held.RaspResults(x@results, y))
}

#' @rdname occ.held
#' @inheritParams occ.held
#' @export
occ.held.RaspSolved<-function(x, y=NULL) {
	return(occ.held.RaspResults(x@results, y))
}

#' @rdname space.held
#' @inheritParams space.held
#' @export
space.held.RaspSolved<-function(x, y=NULL) {
	return(space.held.RaspResults(x@results, y))
}

#' @rdname amount.targets.met
#' @inheritParams amount.targets.met
#' @export
amount.targets.met.RaspSolved<-function(x, y=NULL) {
	return(amount.targets.met.RaspResults(x@results, y))
}

#' @rdname space.targets.met
#' @inheritParams space.targets.met
#' @export
space.targets.met.RaspSolved<-function(x, y=NULL) {
	return(space.targets.met.RaspResults(x@results, y))
}


#' @export
print.RaspSolved<-function(x) {
	cat("RaspSolved object.\n")
	print.RaspOpts(x@opts, FALSE)
	print.RaspData(x@data, FALSE)
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

