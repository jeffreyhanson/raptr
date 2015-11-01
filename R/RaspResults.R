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
#' @slot logging.file \code{character} Gurobi log files.
#' @slot .cache \code{environment} used to store extra data.
#' @export
#' @seealso \code{\link{RaspResults}}, \code{\link{read.RaspResults}}.
setClass("RaspResults",
	representation(
		summary="data.frame",
		selections="matrix",
		amount.held="matrix",
		space.held="matrix",
		logging.file="character",
		best="integer",
		.cache='environment'
	),
	validity=function(object) {
		# summary
		if (!all(unlist(sapply(object@summary, is.finite))))
			stop('object@summary contains NA or non-finite values')
		
		# selections
		if (any(!object@selections %in% c(0,1)))
			stop('object@selections contains values that are not 0 or 1')
		
		# amount.held
		if (!all(c(is.finite(object@amount.held))))
			stop('object@amount.held contains NA or non-finite values')
		if (any(object@amount.held < 0 | object@amount.held > 1))
			stop('object@amount.held contains values less than 0 or greater than 1')
		
		# space.held
		if (!all(c(is.finite(object@space.held))))
			stop('object@space.held contains NA or non-finite values')
		if (any(object@space.held < 0))
			stop('object@space.held contains values less than 0')
		if (any(object@space.held > 1))
			warning('object@space.held contains values greater than 1, consider increasing the FAILUREMULTIPLIER')
		
		# logging.file
		if (any(is.na(object@logging.file)))
			stop('object@logging.file contains NA values')
		
		# best
		if (length(object@best)>1)
			stop('object@best contains more than one value')
		if (!is.finite(object@best))
			stop('object@best contains NA or non-finite values')
		if (!object@best %in% seq_len(nrow(object@space.held)))
			stop('object@best is not an index of a solution in object')
		
		# cross-slot dependencies
		if (nrow(object@summary) != length(object@logging.file))
			stop('object@summary has different number of solutions to object@logging.file')
		if (nrow(object@summary) != nrow(object@selections))
			stop('object@summary has different number of solutions to object@selections')
		if (nrow(object@summary) != nrow(object@amount.held))
			stop('object@summary has different number of solutions to object@amount.held')
		if (nrow(object@summary) != nrow(object@space.held))
			stop('object@summary has different number of solutions to object@space.held')

		
		return(TRUE)
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
#' @param logging.file \code{character} Gurobi log files.
#' @export
#' @note slot \code{best} is automatically determined based on data in \code{summary}.
#' @return \code{RaspResults} object
#' @seealso \code{\link{RaspResults-class}} \code{\link{read.RaspResults}}
RaspResults=function(summary, selections, amount.held, space.held, logging.file) {
	return(new("RaspResults", summary=summary, selections=selections, amount.held=amount.held, space.held=space.held, logging.file=logging.file, best=which.min(summary$Score)))
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

#' @method summary RaspResults
#' @export summary
summary.RaspResults<-function(object) {
	return(object@summary)
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

#' @rdname logging.file
#' @inheritParams logging.file
#' @export
logging.file.RaspResults<-function(x,y=0) {
	if (is.null(y))
		return(x@logging.file)
	if (y==0)
		return(x@logging.file[x@best])
	return(x@logging.file[y])
}

#' @method print RaspResults
#' @rdname print
#' @export
print.RaspResults<-function(x, ..., header=TRUE) {
	if (header)
		cat("RaspResults object.\n")
	cat("  Number of solutions:",nrow(x@summary),"\n")
	cat(paste0("  Best solution score: ", score(x,0), " (",sum(selections(x,0))," planning units)\n"))
}

#' @describeIn show
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
