#' @include misc.R raspr-internal.R generics.R
NULL

#' RaspResults: An S4 class to represent RASP results
#'
#' This class is used to store RASP results.
#'
#' @slot summary \code{data.frame} with summary information on solutions.
#' @slot selections \code{matrix} with binary selections.
#' @slot amount.held \code{matrix} with the amount held for each species in each solution.
#' @slot occ.held \code{matrix} with the number of occurrences for each species in each solution.
#' @slot space.held \code{matrix} with the poportion of attribute space sampled for each species in each solution.
#' @slot amount.targets.met \code{matrix} whether the amount targets have been met for each species in each solution.
#' @slot space.targets.met \code{matrix} whether the attribute space targets have been met for each species in each solution.
#' @slot best \code{integer} with index of best solution.
#' @slot log.file \code{character} with Gurobi log file. 
#' @slot model.file \code{character} with Gurobi model file. 
#' @export
#' @seealso \code{\link{RaspResults}}, \code{\link{read.RaspResults}}.
setClass("RaspResults",
	representation(
		summary="data.frame",
		selections="matrix",
		amount.held="matrix",
		occ.held="matrix",
		space.held="matrix",
		amount.targets.met="matrix",
		space.targets.met="matrix",
		best="integer",
		log.file='character',
		model.file="character",
		.cache='environment'
	)
)
setMethod(
	"initialize", 
	"RaspResults", 
	function(.Object, summary, selections, amount.held, occ.held, space.held, amount.targets.met, space.targets.met, best, model.file, log.file, .cache=new.env()) {
		callNextMethod(.Object, summary=summary, selections=selections, amount.held=amount.held, occ.held=occ.held, spaceheld=space.held, amount.targets.met=amount.targets.met, space.targets.met=space.targets.met, best=best, mode=model.file, log=log.file, .cache=.cache)
	}
)

#' Create RaspResults object
#'
#' This function creates a new \code{RaspResults} object.
#'
#' @param summary \code{data.frame} with summary information on solutions.
#' @param selections \code{matrix} with binary selections.
#' @param amount.held \code{matrix} with the amount held for each species in each solution.
#' @param occ.held \code{matrix} with the number of occurrences for each species in each solution.
#' @param space.held \code{matrix} with the poportion of attribute space sampled for each species in each solution.
#' @param amount.targets.met \code{matrix} whether the amount targets have been met for each species in each solution.
#' @param space.targets.met \code{matrix} whether the attribute space targets have been met for each species in each solution.
#' @param log \code{character} with Gurobi log file. 
#' @export
#' @note slot \code{best} is automatically determined based on data in \code{summary}.
#' @return \code{RaspResults} object
#' @seealso \code{\link{RaspResults-class}} \code{\link{read.RaspResults}}
RaspResults=function(summary, selections, amount.held, occ.held, space.held, amount.targets.met, space.targets.met, model.file, log.file) {
	return(new("RaspResults", summary=summary, selections=selections, amount.held=amount.held, occ.held=occ.held, space.held=space.held, amount.targets.met=amount.targets.met, space.targets.met=space.targets.met, best=which.max(summary$Score), model=model.file, log=log.file))
}

#' Read RASP results
#'
#' This function reads files output from Gurobi and returns a \code{RaspResults} object.
#'
#' @param data \code{RaspData} object containing RASP data.
#' @param path \code{character} with file path to the output from Gurobi.
#' @export
#' @return \code{RaspResults} object
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspResults}}.
read.RaspResults=function(data, path) {
	# check for valid inputs
	stopifnot(inherits(data, 'RaspData'))
	if (!file.exists(path))
		stop('File path does not exist.')
	# load results

	# calculate result data
	
	# create object
	stop()
}

#' Merge RASP results
#'
#' This function merges a list of \code{RaspResults} objects into a single \code{RaspResults} object. 
#' It is used for collating results from multiple runs.
#'
#' @param x \code{list} of \code{RaspResults} objects.
#' @export
#' @return \code{RaspResults} object
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspResults}}.
merge.RaspResults<-function(x) {
	x=RaspResults(
		summary=ldply(x, slot, name="summary"),
		selections=do.call(rbind, llply(x, slot, name="selections")),
		amount.held=do.call(rbind, llply(x, slot, name="amount.held")),
		occ.held=do.call(rbind, llply(x, slot, name="occ.held")),
		space.held=do.call(rbind, llply(x, slot, name="occ.held")),
		amount.targets.met=do.call(rbind, llply(x, slot, name="amount.targets.met")),
		space.targets.met=do.call(rbind, llply(x, slot, name="space.targets.met")),
		log.file=paste(laply(x, slot, name="log.file"), collapse="\n"),
		model.file=paste(laply(x, slot, name="model.file"), collapse="\n")
	)
	x@summary$Run_Number<-seq_len(nrow(x@summary))
	return(x)
}

#' @rdname selections
#' @inheritParams selections
#' @export
selections.RaspResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@selections)
	if (y==0)
		return(x@selections[x@best,])
	return(x@selections[y,])
}


#' @rdname score
#' @inheritParams score
#' @export
score.RaspResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@summary$Score)
	if (y==0)
		return(x@summary$Score[x@best])
	return(x@summary$Score[y])
}

#' @export
summary.RaspResults<-function(x) {
	return(x@summary)
}

#' @rdname log.file
#' @inheritParams log.ile
#' @export
log.file.RaspResults<-function(x) {
	cat(x@log.file)
}

#' @rdname model.file
#' @inheritParams model.file
#' @export
model.file.RaspResults<-function(x) {
	cat(x@model.file)
}

#' @export
#' @inheritParams amount.held
#' @rdname amount.held
amount.held.RaspResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@amount.held)
	if (y==0)
		return(x@amount.held[x@best,])
	return(x@amount.held[y,])
}

#' @rdname occ.held
#' @inheritParams occ.held
#' @export
occ.held.RaspResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@occ.held)
	if (y==0)
		return(x@occ.held[x@best,])
	return(x@occ.held[y,])
}

#' @rdname space.held
#' @inheritParams space.held
#' @export
space.held.RaspResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@space.held)
	if (y==0)
		return(x@space.held[x@best,])
	return(x@space.held[y,])
}

#' @rdname amount.targets.met
#' @inheritParams amount.targets.met
#' @export
amount.targets.met.RaspResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@amount.targets.met)
	if (y==0)
		return(x@amount.targets.met[x@best,])
	return(x@amount.targets.met[y,])
}

#' @rdname space.targets.met
#' @inheritParams space.targets.met
#' @export
space.targets.met.RaspResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@space.targets.met)
	if (y==0)
		return(x@space.targets.met[x@best,])
	return(x@space.targets.met[y,])
}

#' @export
print.RaspResults<-function(x, header=TRUE) {
	if (header)
		cat("RaspResults object.\n")
	cat("Number of solutions:",nrow(x@summary),"\n")
}

#' @export
setMethod(
	'show',
	'RaspResults',
	function(object)
		print.RasprResults(object)
)

