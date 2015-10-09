#' @include misc.R raspr-internal.R generics.R
NULL

#' RaspResults: An S4 class to represent RASP results
#'
#' This class is used to store RASP results.
#'
#' @slot summary \code{data.frame} with summary information on solutions.
#' @slot selections \code{matrix} with binary selections.
#' @slot amount.held \code{matrix} with the amount held for each species in each solution.
#' @slot space.held \code{matrix} with the poportion of attribute space sampled for each species in each solution.
#' @slot best \code{integer} with index of best solution.
#' @slot model.file \code{character} with Gurobi model file. 
#' @slot log.file \code{character} with Gurobi log file. 
#' @slot solution.file \code{character} with Gurobi solution file. 
#' @export
#' @seealso \code{\link{RaspResults}}, \code{\link{read.RaspResults}}.
setClass("RaspResults",
	representation(
		summary="data.frame",
		selections="matrix",
		amount.held="matrix",
		space.held="matrix",
		best="integer",
		model.file="character",
		log.file='character',
		solution.file="character",
		.cache='environment'
	)
)
setMethod(
	"initialize", 
	"RaspResults", 
	function(.Object, summary, selections, amount.held, space.held, best, model.file, log.file, solution.file, .cache=new.env()) {
		callNextMethod(.Object, summary=summary, selections=selections, amount.held=amount.held,  space.held=space.held, best=best, model.file=model.file, log.file=log.file, solution.file=solution.file, .cache=.cache)
	}
)

#' Create RaspResults object
#'
#' This function creates a new \code{RaspResults} object.
#'
#' @param summary \code{data.frame} with summary information on solutions.
#' @param selections \code{matrix} with binary selections.
#' @param amount.held \code{matrix} with the amount held for each species in each solution.
#' @param space.held \code{matrix} with the poportion of attribute space sampled for each species in each solution.
#' @param model.file \code{character} with Gurobi model file. 
#' @param log.file \code{character} with Gurobi log file. 
#' @param solution.file \code{character} with Gurobi solution file. 
#' @export
#' @note slot \code{best} is automatically determined based on data in \code{summary}.
#' @return \code{RaspResults} object
#' @seealso \code{\link{RaspResults-class}} \code{\link{read.RaspResults}}
RaspResults=function(summary, selections, amount.held, space.held, model.file, log.file, solution.file) {
	return(new("RaspResults", summary=summary, selections=selections, amount.held=amount.held, space.held=space.held, best=which.min(summary$Score), model.file=model.file, log.file=log.file, solution.file=solution.file))
}

#' Read RASP results
#'
#' This function reads files output from Gurobi and returns a \code{RaspResults} object.
#'
#' @param opts \code{RaspOpts} object
#' @param data \code{RaspData} object
#' @param model.file \code{character} object containing Gurobi model file.
#' @param log.file \code{character} object containing Gurobi log file.
#' @param solution.file \code{character} object containing Gurobi solution file.
#' @export
#' @return \code{RaspResults} object
#' @seealso \code{\link{RaspData}}, \code{\link{RaspData-class}}, \code{\link{RaspResults-class}}, \code{\link{RaspResults}}.
read.RaspResults=function(opts, data, model.file, log.file, solution.file) {
	x<-rcpp_extract_model_results(
		opts,
		data,
		model.file,
		log.file,
		solution.file
	)
	x@.cache=new.env()
	return(x)
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
		space.held=do.call(rbind, llply(x, slot, name="space.held")),
		model.file=laply(x, slot, name="model.file"),
		log.file=laply(x, slot, name="log.file"),
		solution.file=laply(x, slot, name="solution.file")
	)
	x@summary$Run_Number<-seq_len(nrow(x@summary))
	return(x)
}

#' @rdname selections
#' @inheritParams selections
#' @export
selections.RaspResults<-function(x, y=0) {
	if (is.null(y))
		return(x@selections)
	if (y==0)
		return(x@selections[x@best,])
	return(x@selections[y,])
}


#' @rdname score
#' @inheritParams score
#' @export
score.RaspResults<-function(x, y=0) {
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
	return(x@log.file)
}

#' @rdname model.file
#' @inheritParams model.file
#' @export
model.file.RaspResults<-function(x) {
	return(x@model.file)
}

#' @rdname solution.file
#' @inheritParams solution.file
#' @export
solution.file.RaspResults<-function(x) {
	return(x@solution.file)
}

#' @export
#' @inheritParams amount.held
#' @rdname amount.held
amount.held.RaspResults<-function(x, y=0) {
	if (is.null(y))
		return(x@amount.held)
	if (y==0)
		return(x@amount.held[x@best,])
	return(x@amount.held[y,])
}

#' @rdname space.held
#' @inheritParams space.held
#' @export
space.held.RaspResults<-function(x, y=0) {
	if (is.null(y))
		return(x@space.held)
	if (y==0)
		return(x@space.held[x@best,])
	return(x@space.held[y,])
}

#' @export
print.RaspResults<-function(x, header=TRUE) {
	if (header)
		cat("RaspResults object.\n")
	cat("  Number of solutions:",nrow(x@summary),"\n")
	cat(paste0("  Best solution score: ", score(x,0), " (",sum(selections(x,0))," planning units)\n"))
}

#' @export
setMethod(
	'show',
	'RaspResults',
	function(object)
		print.RaspResults(object)
)


#' @describeIn is.cached
setMethod(
	f="is.cached", 
	signature(x="RaspResults", name="character"), 
	function(x,name) {
		return(!is.null(x@.cache[[name]]))
	}
)

#' @describeIn cache
setMethod(
	f="cache", 
	signature(x="RaspResults", name="character", y="ANY"), 
	function(x, name, y) {
		x@.cache[[name]]=y
	}
)

#' @describeIn cache
setMethod(
	f="cache", 
	signature(x="RaspResults", name="character", y="missing"), 
	function(x, name, y) {
		return(x@.cache[[name]])
	}
)