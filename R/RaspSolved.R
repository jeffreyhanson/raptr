#' @include RcppExports.R raspr-internal.R misc.R generics.R RaspReliableOpts.R RaspUnreliableOpts.R RaspData.R RaspUnsolved.R RaspResults.R
NULL

#' RaspSolved: An S4 class to represent RASP inputs and outputs
#'
#' This class is used to store RASP input and output data in addition to input parameters.
#'
#' @slot opts \code{RaspReliableOpts} or \code{RaspUnreliableOpts} object used to store input parameters.
#' @slot solver \code{GurobiOpts} or \code{ManualOpts} object used to store solver information/parameters.
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

setClassUnion("RaspUnsolOrSol", c("RaspSolved", "RaspUnsolved"))

#' Create new RaspSolved object
#'
#' This function creates a \code{RaspSolved} object.
#'
#' @param unsolved \code{RaspUnsolved} object.
#' @param solver \code{GurobiOpts} or \code{ManualOpts} object.
#' @param results \code{RaspResults} object.
#' @return \code{RaspSolved} object.
#' @export
#' @seealso \code{\link{RaspSolved-class}}, \code{\link{RaspResults-class}}, \code{link{solve}}.
RaspSolved<-function(unsolved, solver, results) {
	return(new("RaspSolved", opts=unsolved@opts, solver=solver, data=unsolved@data, results=results))
}

#' @describeIn solve
#' @export
setMethod(
	'solve',
	representation(a='RaspUnsolOrSol', b='missing'),
	function(a, b, ..., verbose=FALSE) {
		# solve using GurobiOpts
		return(
			solve(a, b=GurobiOpts(...), verbose)
		)
	}
)

#' @describeIn solve
#' @export
setMethod(
	'solve',
	representation(a='RaspUnsolOrSol', b='GurobiOpts'),
	function(a, b, verbose=FALSE) {
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
		model<-rcpp_generate_model_object(a@opts, inherits(a@opts, 'RaspUnreliableOpts'), a@data, verbose)
		model$A<-Matrix::sparseMatrix(i=model$Ar$row+1, j=model$Ar$col+1, x=model$Ar$value)
		
		## first run
		# run model
		log.pth<-tempfile(fileext='.log')
		gparams<-append(as.list(b), list("LogFile"=log.pth))
		solution<-gurobi::gurobi(model, gparams)
		if (file.exists('gurobi.log')) unlink('gurobi.log')

		# check solution object
		if (!is.null(solution$status))
			if (solution$status=="INFEASIBLE") {
				stop('No solution found because model is not feasible.')
			}
		if (is.null(solution$x)) {
			stop('No solution found because Gurobi parameters do not allow sufficient time.')
		}

		# store results
		results<-list(read.RaspResults(a@opts, a@data, model, paste(readLines(log.pth), collapse="\n"), solution, verbose))
		existing.solutions<-list(selections(results[[1]]))

		## subsequent runs
		for (i in seq_len(b@NumberSolutions-1)) {
			# create new model object, eacluding existing solutions as valid solutions to ensure a different solution is obtained
			model<-rcpp_append_model_object(model, existing.solutions[length(existing.solutions)])
			model$A<-Matrix::sparseMatrix(i=model$Ar$row+1, j=model$Ar$col+1, x=model$Ar$value)

			# run model
			solution<-gurobi::gurobi(model, gparams)
			if (file.exists('gurobi.log')) unlink('gurobi.log')

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
			currResult<-read.RaspResults(a@opts,a@data, model, paste(readLines(log.pth), collapse="\n"), solution, verbose)
			results<-append(results,currResult)
			existing.solutions<-append(existing.solutions, list(selections(currResult)))
		}
		# return RaspSolved object
		return(RaspSolved(unsolved=a, solver=b, results=mergeRaspResults(results)))
	}
)

#' @describeIn solve
#' @export
setMethod(
	'solve',
	representation(a='RaspUnsolOrSol', b='matrix'),
	function(a, b, verbose=FALSE) {
		# check arguments for validity
		if (ncol(b)!=nrow(a@data@pu))
			stop('argument to b has different number of planning units to a')
		if (any(is.na(b)))
			stop('argument to b must not contain any NA values')
		if (any(b!= 0 & b!= 1))
			stop('argument to b must be binary selections when b is a matrix')
		# generate result objects
		model=rcpp_generate_model_object(a@opts, inherits(a@opts, 'RaspUnreliableOpts'), a@data, verbose)
		results=list()
		for (i in seq_len(nrow(b))) {
			# generate result object
			currResult=read.RaspResults(
				a@opts,a@data,model,"User specified solution",
				list(
					x=b[i,],
					objval=NA
				),
				verbose
			)
			results=append(results,currResult)
		}
		# return RaspSolved object
		return(RaspSolved(unsolved=a, solver=ManualOpts(NumberSolutions=nrow(b)), results=mergeRaspResults(results)))
	}
)

#' @describeIn solve
#' @export
setMethod(
	'solve',
	representation(a='RaspUnsolOrSol', b='numeric'),
	function(a, b, verbose=FALSE) {
		# check arguments for validity
		if (any(!b %in% seq_len(nrow(a@data@pu))))
			stop('argument to b refers to planning unit indices not in a')
		# return RaspSolved object
		return(
			solve(a, b=matrix(replace(rep(0, nrow(a@data@pu)), b, rep(1, length(b))), nrow=1), verbose=verbose)
		)
	}
)

#' @describeIn solve
#' @export
setMethod(
	'solve',
	representation(a='RaspUnsolOrSol', b='logical'),
	function(a, b, verbose=FALSE) {
		# check arguments for validity
		if (length(b)!=nrow(a@data@pu))
			stop('argument to b has different number of planning units to a')
		# generate RaspSolved object with user-specified solution
		return(
			solve(a, b=matrix(as.numeric(b), nrow=1), verbose=verbose)
		)
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
	cat("RaspSolved object\n\n")
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
		RaspUnsolved(
			opts=x@opts,
			data=spp.subset(x@data, species)
			)
	 )
}

#' @rdname pu.subset
#' @method pu.subset RaspSolved
#' @export
pu.subset.RaspSolved<-function(x, pu) {
	return(
		RaspUnsolved(
			opts=x@opts,
			data=pu.subset(x@data, pu)
			)
	 )
}

#' @rdname dp.subset
#' @method dp.subset RaspSolved
#' @export
dp.subset.RaspSolved<-function(x, space, species, points) {
	return(
		RaspUnsolved(
			opts=x@opts,
			data=dp.subset(x@data, space, species, points)
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
	signature(x="RaspUnsolOrSol", y="RaspUnsolOrSol"),
	function(x,y) {
		return(is.comparable(x@data, y@data))
	}
)

#' @rdname is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="RaspData", y="RaspUnsolOrSol"),
	function(x,y) {
		return(is.comparable(x, y@data))
	}
)

#' @rdname is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="RaspUnsolOrSol", y="RaspData"),
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
		if (nrow(x@data@polygons)==0) stop("Spatial data for planning units not present in object")
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
		if (nrow(x@data@polygons)==0) stop("Spatial data for planning units not present in object")
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
		if (nrow(x@data@polygons)==0) stop("Spatial data for planning units not present in object")
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


#' @rdname spp.plot
#' @method spp.plot RaspSolved
#' @export
spp.plot.RaspSolved<-function(x, species, y=0, prob.color.palette="YlGnBu", pu.color.palette='RdYlGn', basemap="none", locked.in.color="#000000FF", locked.out.color="#D7D7D7FF", alpha=ifelse(basemap=="none", 1, 0.7), grayscale=FALSE, force.reset=FALSE, ...) {
	# data checks
	stopifnot(length(species)==1)
	stopifnot(y %in% c(0:nrow(x@results@selections)))
	if (nrow(x@data@polygons)==0)
			stop("Spatial data for planning units not present in object")
	if (is.character(species)) {
		if (!species %in% x@data@species$name)
			stop('argument to species is not a species name in argument to x')
		spp_pos <-match(species, x@data@species$name)
	} else{
		if (is.numeric(species)) {
			if (!species %in% seq_along(x@data@species$name))
				stop('argument to species is not a valid index for species in argument to x')
			spp_pos <- species
		}
}
	# get basemap
	if (basemap!="none")
		basemap<-basemap.RaspData(x, basemap, grayscale, force.reset)
	## main processing
	# extract planning fill unit colors
	values<-numeric(nrow(x@data@pu))
	rows<-which(x@data@pu.species.probabilities$species == spp_pos)
	values[x@data@pu.species.probabilities$pu[rows]]<-x@data@pu.species.probabilities$value[rows]
	if (length(unique(values))>1) {
		cols<-brewerCols(rescale(values, to=c(0,1)), prob.color.palette, alpha)
	} else {
		cols<-brewerCols(rep(values[1], length(values)), prob.color.palette, alpha)
		values<-c(0,values[1])
	}
	# extract planning unit border colors
	all.border.cols<-brewerCols(c(0,1), pu.color.palette, alpha)
	border.cols<-rep(all.border.cols[1], nrow(x@data@pu))
	border.cols[which(as.logical(selections(x, y)))]<-all.border.cols[length(all.border.cols)]
	border.cols[which(x@data@pu$status==2)]<-locked.in.color
	border.cols[which(x@data@pu$status==3)]<-locked.out.color
	# set title
	if (!is.null(x@data@species$name)) {
		main=paste0(x@data@species$name[spp_pos ], " in planning units (%)")
	} else {
		main=paste0("Species ",species, " in planning units (%)")
	}
	# make plot
	plot(1,1)
	prettyGeoplot(
		x@data@polygons,
		cols,
		basemap,
		main,
		continuousLegend(values,prob.color.palette,posx=c(0.3, 0.4),posy=c(0.1, 0.9)),
		beside=TRUE,
		border=border.cols
	)
}

#' @rdname space.plot
#' @method space.plot RaspSolved
#' @export
space.plot.RaspSolved<-function(
	x,
	species,
	space=1,
	y=0,
	pu.color.palette='RdYlGn',
	locked.in.color="#000000FF",
	locked.out.color="#D7D7D7FF",
	...
) {
	# data checks
	stopifnot(length(y)==1)
	stopifnot(y %in% c(0:nrow(x@results@selections)))
	stopifnot(length(species)==1)
	if (is.character(species)) {
		if (!species %in% x@data@species$name)
			stop('argument to species is not a species name in argument to x')
		spp_pos<-match(species, x@data@species$name)
	} else {
		if (is.numeric(species)) {
			if (!species %in% seq_along(x@data@species$name))
				stop('argument to species is not a valid index for species in argument to x')
			spp_pos <- species
		}
	}
	# set title
	if (!is.null(x@data@species$name)) {
		main=x@data@species$name[spp_pos]
	} else {
		main=paste0("Species ",species)
	}
	# extract pu data
	pu<-as.data.frame(x@data@attribute.spaces[[space]]@pu@coords)
	names(pu)<-paste0('X',seq_len(ncol(pu)))
	pu$status<-'Not Selected'
	pu$status[which(as.logical(selections(x, y)))]<-'Selected'
	pu$status[which(x@data@pu$status==2)]<-'Locked In'
	pu$status[which(x@data@pu$status==3)]<-'Locked Out'
	# extract dp data
	dp<-as.data.frame(x@data@attribute.spaces[[space]]@dp[[spp_pos]]@points@coords)
	names(dp)<-paste0('X',seq_len(ncol(dp)))
	dp$weights=x@data@attribute.spaces[[space]]@dp[[spp_pos]]@weights
	# make plots
	do.call(
		paste0('spacePlot.',ncol(x@data@attribute.spaces[[space]]@pu@coords),'d'),
		list(
			pu,
			dp,
			pu.color.palette,
			locked.in.color,
			locked.out.color,
			main
		)
	)
}

#' @rdname update
#' @method update RaspUnsolOrSol
#' @export
update.RaspUnsolOrSol<-function(object, ..., solve=TRUE) {
	object<-RaspUnsolved(
		opts=do.call(
			'update',
			append(
					list(object=object@opts),
					parseArgs('update', object@opts, ...)
			)
		),
		data=do.call(
			'update',
			append(
					list(object=object@data),
					parseArgs('update', object@data, ...)
			)
		)
	)
	if (solve) {
		object<-do.call(
				raspr::solve,
				append(
					list(a=object),
					parseArgs2(c('b', 'Threads',
						'MIPGap', 'NumberSolutions', 'TimeLimit', 'Presolve', 'verbose'
					), ...)
			)
		)
	}
	return(object)
}

#' @rdname amount.target
#' @method amount.target RaspUnsolOrSol
#' @export
amount.target.RaspUnsolOrSol<-function(x,species=NULL) {
	amount.target.RaspData(x@data, species)
}

#' @rdname space.target
#' @method space.target RaspUnsolOrSol
#' @export
space.target.RaspUnsolOrSol<-function(x, species=NULL, space=NULL) {
	space.target.RaspData(x@data, species, space)
}

#' @rdname amount.target
#' @method amount.target<- RaspUnsolOrSol
#' @export
`amount.target<-.RaspUnsolOrSol`<-function(x,species=NULL, value) {
	x@data<-`amount.target<-.RaspData`(x@data, species, value)
	return(x)
}

#' @rdname space.target
#' @export
`space.target<-.RaspUnsolOrSol`<-function(x, species=NULL, space=NULL, value) {
	x@data<-`space.target<-.RaspData`(x@data, species, space, value)
	return(x)
}
