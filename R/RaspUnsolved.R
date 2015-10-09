#' @include RcppExports.R raspr-internal.R misc.R generics.R RaspOpts.R RaspData.R
NULL

#' RaspUnsolved: An S4 class to represent RASP inputs
#'
#' This class is used to store RASP input data and input parameters.
#'
#' @slot opts \code{RaspOpts} object used to store input parameters.
#' @slot gurobi \code{GurobiOpts} object used to store input data.
#' @slot data \code{RaspData} object used to store input data.
#' @export
#' @seealso  \code{\link{RaspOpts-class}},  \code{\link{GurobiOpts-class}}, \code{\link{RaspData-class}}.
setClass("RaspUnsolved",
	representation(
		opts="RaspOpts",
		gurobi="GurobiOpts",
		data="RaspData"
	)
)

#' Create a new RaspUnsolved object
#'
#' This function creates a \code{RaspUnsolved} object using \code{RaspOpts} and \code{RaspData} objects.
#'
#' @param opts \code{RaspOpts} object.
#' @param data \code{RaspData} object.
#' @return \code{RaspUnsolved} object.
#' @export
#' @seealso  \code{\link{RaspOpts-class}},  \code{\link{GurobiOpts-class}}, \code{\link{RaspData-class}}.
RaspUnsolved<-function(opts, gurobi, data) {
	return(new("RaspUnsolved", opts=opts, gurobi=gurobi, data=data))
}

#' @rdname solve
#' @inheritParams solve
#' @export
solve.RaspUnsolved=function(x, wd=tempdir(), clean=TRUE) {
	## init
	# check that gurobi is installed
	if (!is.null(options()$GurobiInstalled)) {
		if (!options()$GurobiInstalled) {
			stop('Gurobi is either not installed, or the licensing information has not been configured.')
		}
	} else {
		is.GurobiInstalled()
	}
	
	# generate model file
	pth=tempfile(tmpdir=wd)
	model=rcpp_generate_modelfile(x@opts, x@data)
	writeLines(model, paste0(pth,'.lp'))
	
	## first run
	# run model
	call.Gurobi(x@gurobi, paste0(pth, '.lp'), paste0(pth, '.log'), paste0(pth, '.sol'), verbose=TRUE)
	
	# store results
	results=list(read.RaspResults(x@opts, x@data, readLines(paste0(pth, '.lp')), readLines(paste0(pth, '.log')), readLines(paste0(pth, '.sol'))))
	existing.solutions=list(selections(results[[1]]))
	
	## subsequent runs
	for (i in seq_len(x@opts@NUMREPS-1)) {
		# create new model file, excluding existing solutions as valid solutions to ensure a different solution is obtained
		pth=tempfile(tmpdir=wd)
		o1<<-existing.solutions
		o2<<-results[[i]]@model.file
		model=rcpp_append_modelfile(results[[i]]@model.file, existing.solutions)
		writeLines(model, paste0(pth,'.lp'))
		
		# run model
		call.Gurobi(x@gurobi, paste0(pth, '.lp'), paste0(pth, '.log'), paste0(pth, '.sol'), verbose=TRUE)
	
		# load results
		currResultFile=readLines(paste0(pth, '.sol'))
		if (length(currResultFile)==0) {
			warning(paste0('only ',i+1,' solutions found\n'))
			break
		}
		# store results
		currResult=read.RaspResults(x@opts,x@data, readLines(paste0(pth, '.lp')), readLines(paste0(pth, '.log')), currResultFile)
		results=append(results,currResult)
		existing.solutions=append(existing.solutions, list(selections(currResult)))
	}
	
	# return RaspSolved object
	return(RaspSolved(unsolved=x, results=merge.RaspResults(results)))
}

#' @export
print.RaspUnsolved=function(x) {
	cat("Parameters\n")
	print.RaspOpts(x@opts, FALSE)
	cat("Solver settings\n")
	print.GurobiOpts(x@gurobi, FALSE)
	cat("Data\n")
	print.RaspData(x@data, FALSE)
}

# ' @export
setMethod(
	'show',
	'RaspUnsolved',
	function(object)
		print.RaspUnsolved(object)
)



