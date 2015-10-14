#' @include RcppExports.R raspr-internal.R misc.R generics.R RaspOpts.R RaspData.R
NULL

#' RaspUnsolved: An S4 class to represent RASP inputs
#'
#' This class is used to store RASP input data and input parameters.
#'
#' @slot opts \code{RaspOpts} object used to store input parameters.
#' @slot gurobi \code{GurobiOpts} object used to store Gurobi parameters.
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
#' @param gurobi \code{GurobiOpts} object used to store Gurobi parameters.
#' @param data \code{RaspData} object.
#' @return \code{RaspUnsolved} object.
#' @export
#' @seealso  \code{\link{RaspOpts-class}},  \code{\link{GurobiOpts-class}}, \code{\link{RaspData-class}}.
#' @examples
#' data(sim_pus, sim_spp)
#' # create inputs for RaspUnsolved
#' go <- GurobiOpts(MIPGap=0.9)
#' ro <- RaspOpts(NUMREPS=1L, FAILUREMULTIPLIER=1.1)
#' rd <- make.RaspData(sim_pus[1:10,], sim_spp, NULL, include.geographic.space=TRUE,n.demand.points=5L)
#' # create RaspUnsolved object
#' ru <- RaspUnsolved(ro, go, rd)
#' print(ru)

RaspUnsolved<-function(opts, gurobi, data) {
	return(new("RaspUnsolved", opts=opts, gurobi=gurobi, data=data))
}

#' @describeIn solve
#' @export
setMethod(
	'solve',
	'RaspUnsolved',
	function(x) {
		## init
		# check that gurobi is installed
		if (!is.null(options()$GurobiInstalled)) {
			if (!options()$GurobiInstalled) {
				stop('The gurobi R package has not been installed, or Girobi has not been installation has not been completed')
			}
		} else {
			is.GurobiInstalled()
		}
		
		# generate model object
		model=rcpp_generate_model_object(x@opts, x@data)
		model$A=sparseMatrix(i=model$Ar$row+1, j=model$Ar$col+1, x=model$Ar$value)
		
		## first run
		# run model
		log.pth=tempfile(fileext='.log')
		gparams=append(as.list(x@gurobi), list("LogFile"=log.pth))
		solution<-gurobi::gurobi(model, gparams)
		
		# store results
		results=list(read.RaspResults(x@opts, x@data, model, paste(readLines(log.pth), collapse="\n"), solution))
		existing.solutions=list(selections(results[[1]]))
		
		## subsequent runs
		for (i in seq_len(x@opts@NUMREPS-1)) {
			# create new model object, excluding existing solutions as valid solutions to ensure a different solution is obtained
			model=rcpp_append_model_object(model, existing.solutions[length(existing.solutions)])
			model$A=sparseMatrix(i=model$Ar$row+1, j=model$Ar$col+1, x=model$Ar$value)
			
			# run model
			solution=gurobi::gurobi(model, gparams)
		
			# load results
			if (solution$status=="INFEASIBLE") {
				warning(paste0('only ',i,' solutions found\n'))
				break
			}
			# store results
			currResult=read.RaspResults(x@opts,x@data, model, paste(readLines(log.pth), collapse="\n"), solution)
			results=append(results,currResult)
			existing.solutions=append(existing.solutions, list(selections(currResult)))
		}
		
		# return RaspSolved object
		return(RaspSolved(unsolved=x, results=mergeRaspResults(results)))
	}
)

#' @method print RaspUnsolved
#' @rdname print
#' @export
print.RaspUnsolved=function(x, ...) {
	cat("Parameters\n")
	print.RaspOpts(x@opts, FALSE)
	cat("Solver settings\n")
	print.GurobiOpts(x@gurobi, FALSE)
	cat("Data\n")
	print.RaspData(x@data, FALSE)
}

#' @describeIn show
#' @export
setMethod(
	'show',
	'RaspUnsolved',
	function(object)
		print.RaspUnsolved(object)
)



