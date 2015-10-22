#' @include RcppExports.R raspr-internal.R misc.R generics.R RaspReliableOpts.R RaspUnreliableOpts.R RaspData.R
NULL

#' RaspUnsolved: An S4 class to represent RASP inputs
#'
#' This class is used to store RASP input data and input parameters.
#'
#' @slot opts \code{RaspReliableOpts} or \code{RaspUnreliableOpts} object used to store input parameters.
#' @slot solver \code{GurobiOpts} object used to store Gurobi parameters.
#' @slot data \code{RaspData} object used to store input data.
#' @export
#' @seealso  \code{\link{RaspReliableOpts-class}}, \code{\link{RaspUnreliableOpts-class}},  \code{\link{GurobiOpts-class}}, \code{\link{RaspData-class}}.
setClass("RaspUnsolved",
	representation(
		opts="RaspOpts",
		solver="SolverOpts",
		data="RaspData"
	)
)

#' Create a new RaspUnsolved object
#'
#' This function creates a \code{RaspUnsolved} object using a \code{GurobiOpts}, a \code{RaspReliableOpts} or \code{RaspUnreliableOpts} object, and a \code{RaspData} object.
#'
#' @param opts \code{RaspReliableOpts} or \code{RaspUnreliableOpts} object.
#' @param solver \code{GurobiOpts} object used to store Gurobi parameters.
#' @param data \code{RaspData} object.
#' @return \code{RaspUnsolved} object.
#' @export
#' @seealso \code{\link{RaspReliableOpts-class}}, \code{\link{RaspUnreliableOpts-class}}, \code{\link{GurobiOpts-class}}, \code{\link{RaspData-class}}.
#' @examples
#' # load data
#' data(sim_pus, sim_spp)
#' # create inputs for RaspUnsolved
#' go <- GurobiOpts(MIPGap=0.9)
#' ro <- RaspUnreliableOpts()
#' rd <- make.RaspData(sim_pus[1:10,], sim_spp, NULL, include.geographic.space=TRUE,n.demand.points=5L)
#' # create RaspUnsolved object
#' ru <- RaspUnsolved(ro, go, rd)
#' print(ru)
RaspUnsolved<-function(opts, solver, data) {
	return(new("RaspUnsolved", opts=opts, solver=solver, data=data))
}

#' @describeIn solve
#' @export
setMethod(
	'solve',
	'RaspUnsolved',
	function(x, verbose=FALSE) {
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
		rcpp_function_name<-ifelse(
			inherits(x@opts, 'RaspReliableOpts'),
			'rcpp_generate_reliable_model_object',
			'rcpp_generate_unreliable_model_object'
		)
		model=do.call(rcpp_function_name, list(x@opts, x@data, verbose))
		model$A=sparseMatrix(i=model$Ar$row+1, j=model$Ar$col+1, x=model$Ar$value)
		## first run
		# run model
		log.pth=tempfile(fileext='.log')
		gparams=append(as.list(x@solver), list("LogFile"=log.pth))
		solution<-gurobi::gurobi(model, gparams)

		# check solution object
		if (!is.null(solution$status))
			if (solution$status=="INFEASIBLE") {
				stop('No solution found because model is not feasible.')
			}
		if (is.null(solution$x)) {
			stop('No solution found because Gurobi parameters do not allow sufficient time.')
		}

		# store results
		results=list(read.RaspResults(x@opts, x@data, model, paste(readLines(log.pth), collapse="\n"), solution))
		existing.solutions=list(selections(results[[1]]))

		## subsequent runs
		for (i in seq_len(x@solver@NumberSolutions-1)) {
			# create new model object, excluding existing solutions as valid solutions to ensure a different solution is obtained
			model=rcpp_append_model_object(model, existing.solutions[length(existing.solutions)])
			model$A=sparseMatrix(i=model$Ar$row+1, j=model$Ar$col+1, x=model$Ar$value)

			# run model
			solution=gurobi::gurobi(model, gparams)

			# load results
			if (!is.null(solution$status))
				if (solution$status=="INFEASIBLE") {
					warning(paste0('only ',i,' solutions found\n'))
					break
				}
			if (is.null(solution$x)) {
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
	print(x@opts, header=FALSE)
	cat("Solver settings\n")
	print(x@solver, header=FALSE)
	cat("Data\n")
	print.RaspData(x@data, header=FALSE)
}

#' @describeIn show
#' @export
setMethod(
	'show',
	'RaspUnsolved',
	function(object)
		print.RaspUnsolved(object)
)

#' @rdname spp.subset
#' @method spp.subset RaspUnsolved
#' @export
spp.subset.RaspUnsolved<-function(x, species) {
	return(
		RaspUnsolved(
			opts=x@opts,
			solver=x@solver,
			data=spp.subset(x@data, species)
			)
	 )
}

#' @rdname pu.subset
#' @method pu.subset RaspUnsolved
#' @export
pu.subset.RaspUnsolved<-function(x, pu) {
	return(
		RaspUnsolved(
			opts=x@opts,
			solver=x@solver,
			data=pu.subset(x@data, pu)
			)
	 )
}

#' @rdname spp.plot
#' @method spp.plot RaspUnsolved
#' @export
spp.plot.RaspUnsolved<-function(x, y, basemap='none', color.palette='YlGnBu', alpha=ifelse(basemap=="none", 1, 0.7), grayscale=FALSE, force.reset=FALSE) {
	spp.plot(x@data, y, basemap, color.palette, alpha, grayscale, force.reset)
}

#' @rdname space.plot
#' @method space.plot RaspUnsolved
#' @export
space.plot.RaspUnsolved<-function(
	x,
	y,
	space=1,
	pu.color.palette='RdYlGn',
	locked.in.color="#000000FF",
	locked.out.color="#D7D7D7FF"
) {
	space.plot.RaspData(x@data, y, space, pu.color.palette, locked.in.color, locked.out.color)
}


#' @rdname update
#' @method update RaspUnsolved
#' @export
update.RaspUnsolved<-function(object, ..., solve=TRUE) {
	object<-RaspUnsolved(
		opts=update(object@opts, ..., ignore.extra=TRUE),
		solver=update(object@solver, ..., ignore.extra=TRUE),
		data=update(object@data, ..., ignore.extra=TRUE)
	)
	if (solve)
		object<-solve(object)
	return(object)
}

#' @rdname amount.target
#' @method amount.target RaspUnsolved
#' @export
amount.target.RaspUnsolved<-function(x) {
	amount.target.RaspData(x@data)
}

#' @rdname space.target
#' @method space.target RaspUnsolved
#' @export
space.target.RaspUnsolved<-function(x) {
	space.target.RaspData(x@data)
}
