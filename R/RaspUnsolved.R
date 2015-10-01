#' @include RcppExports.R raspr-internal.R misc.R generics.R RaspOpts.R RaspData.R
NULL

#' RaspUnsolved: An S4 class to represent RASP inputs
#'
#' This class is used to store RASP input data and input parameters.
#'
#' @slot opts \code{RaspOpts} object used to store input parameters.
#' @slot data \code{RaspData} object used to store input data.
#' @export
#' @seealso  \code{\link{RaspOpts-class}}, \code{\link{RaspData-class}}.
setClass("RaspUnsolved",
	representation(
		data="RaspData",
		opts="RaspOpts"
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
#' @seealso \code{\link{RaspOpts-class}}, \code{\link{RaspData-class}}.
RaspUnsolved<-function(opts, data) {
	return(new("RaspUnsolved", opts=opts, data=data))
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
	pth=tempfile(tempdir=wd)
	model=rcpp_generate_modelfile(x@opts, x@data)
	writeLines(model, paste0(pth,'.lp'))
	
	## first run
	# run model
	log.file=system(
		gurobiSystemCall(x, paste0(pth, '.lp'), paste0(pth, '.sol')),
		capture.output=TRUE
	)
	
	# store results
	results=list(rcpp_collate_model_results(paste0(pth, '.out'), x@opts, x@data, log.file))
	existing.solutions=list(selections(results))
	
	## subsequent runs
	for (i in seq_len(x@opts@NUMREPS-1)) {
		# create new model file, excluding existing solutions as valid solutions to ensure a different solution is obtained
		pth=tempfile(tempdir=wd)
		model=rcpp_append_modelfile(x, existing,solutions)
		writeLines(model, paste0(pth,'.lp'))
		
		# run model
		log.file=system(
			gurobiSystemCall(x, paste0(pth, '.lp'), paste0(pth, '.sol')),
			capture.output=TRUE
		)
		
		# store results
		results=append(results, rcpp_collate_model_results(paste0(pth, '.out'), x@opts, x@data, log.file))
		existing.solutions=list(existing.solutions, list(selections(results)))
	}
	
	# return RaspSolved object
	return(RaspSolved(data=x@data, opts=x@opts, results=merge.RaspResults(results)))
}

#' @export
print.RaspUnsolved=function(x) {
	cat("RaspUnsolved object.\n")
	print.RaspOpts(x@opts, FALSE)
	print.RaspData(x@data, FALSE)
}

