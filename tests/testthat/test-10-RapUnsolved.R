## define functions
# define function to calculate space.held
calcReliableSpaceHeld<-function(w, p, maxr, pus) {
	### init
	# set maxr
	maxr<-min(length(pus),maxr)
	value<-0
	### main proc
	for (k in seq_len(nrow(w))) {
		## init
		# get sorted ids
		curr_pus=pus[order(w[k,pus])]
		# set initial probability
		curr_prob=1
		## main
		# real pus
		for (r in seq_len(maxr)) {
			value=value+(p[curr_pus[r]] * curr_prob * w[k,curr_pus[r]]);
			curr_prob=curr_prob*(1-p[curr_pus[r]])
		}
		# failure pu
		value=value+(curr_prob * w[k,ncol(w)])
	}
	# return value
	return(value)
}

test_that('Model compiler (unreliable)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- pu.subset(spp.subset(sim_ru, 1), 1:10)
	# generate model code
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	model$A<-Matrix::sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]], dims=c(max(model$Ar[[1]])+1, length(model$obj)))
})

test_that('Model compiler (reliable)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- pu.subset(spp.subset(sim_ru, 1), 1:10)
	# generate model code
	model<-rcpp_generate_model_object(RapReliableOpts(), FALSE, sim_ru@data, FALSE)
	model$A<-Matrix::sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]], dims=c(max(model$Ar[[1]])+1, length(model$obj)))
})

test_that('Model compiler (euclidean distances)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1), c(1:5,80)), 1, 1, 1:5)
	sim_ru@data@attribute.spaces[[1]]@distance.metric='euclidean'
	# generate model code
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	# calculate weighted distances
	pts<-rbind(sim_ru@data@attribute.spaces[[1]]@pu@coords,sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)
	dists<-unname(as.matrix(vegan::vegdist(pts, method='euclidean')))
	dists<-dists[
		nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords)+seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)),
		seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords))
	]
	dists<-(dists * matrix(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@weights, byrow=TRUE, ncol=ncol(dists), nrow=nrow(dists)))+1e-5
	# run tests
	expect_equal(model$cache$wdist[[1]][[1]], dists)
})

test_that('Model compiler (bray distances)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1), c(1:5,80)), 1, 1, 1:5)
	sim_ru@data@attribute.spaces[[1]]@distance.metric='bray'
	# generate model code
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	# calculate weighted distances
	pts<-rbind(sim_ru@data@attribute.spaces[[1]]@pu@coords,sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)
	dists<-suppressWarnings(unname(as.matrix(vegan::vegdist(pts, method='bray'))))
	dists<-dists[
		nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords)+seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)),
		seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords))
	]
	dists<-(dists * matrix(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@weights, byrow=TRUE, ncol=ncol(dists), nrow=nrow(dists)))+1e-5
	# run tests
	expect_equal(round(model$cache$wdist[[1]][[1]][-1],4), round(dists[-1],4))
})

test_that('Model compiler (manhattan distances)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1), c(1:5,80)), 1, 1, 1:5)
	sim_ru@data@attribute.spaces[[1]]@distance.metric='manhattan'
	# generate model code
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)	
	# calculate weighted distances
	pts<-rbind(sim_ru@data@attribute.spaces[[1]]@pu@coords,sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)
	dists<-unname(as.matrix(vegan::vegdist(pts, method='manhattan')))
	dists<-dists[
		nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords)+seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)),
		seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords))
	]
	dists<-(dists * matrix(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@weights, byrow=TRUE, ncol=ncol(dists), nrow=nrow(dists)))+1e-5
	# run tests
	expect_equal(round(model$cache$wdist[[1]][[1]],5), round(dists,5))
})

test_that('Model compiler (gower distances)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1), c(1:5,80)), 1, 1, 1:5)
	sim_ru@data@attribute.spaces[[1]]@pu@coords=sim_ru@data@attribute.spaces[[1]]@pu@coords+5
	sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords=sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords+5
	sim_ru@data@attribute.spaces[[1]]@distance.metric='gower'
	# generate model code
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	# calculate weighted distances
	pts<-rbind(sim_ru@data@attribute.spaces[[1]]@pu@coords,sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)
	dists<-unname(as.matrix(vegan::vegdist(pts, method='gower')))
	dists<-dists[
		nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords)+seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)),
		seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords))
	]
	dists<-(dists * matrix(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@weights, byrow=TRUE, ncol=ncol(dists), nrow=nrow(dists)))+1e-5
	# run tests
	expect_equal(round(model$cache$wdist[[1]][[1]],5), round(dists,5))
})

test_that('Model compiler (canberra distances)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1), c(1:5,80)), 1, 1, 1:5)
	sim_ru@data@attribute.spaces[[1]]@distance.metric='canberra'
	# generate model code
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	# calculate weighted distances
	pts<-rbind(sim_ru@data@attribute.spaces[[1]]@pu@coords,sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)
	dists<-matrix(NA, nrow=nrow(pts), ncol=nrow(pts))
	for (i in seq_len(nrow(pts)))
		for (j in seq_len(nrow(pts)))
		dists[i,j]<-sum((abs(pts[i,]-pts[j,])+1e-5)/(abs(pts[i,])+abs(pts[j,])+1e-5))
	dists<-dists[
		nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords)+seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)),
		seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords))
	]
	dists<-(dists * matrix(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@weights, byrow=TRUE, ncol=ncol(dists), nrow=nrow(dists)))+1e-5
	# run tests
	expect_equal(round(model$cache$wdist[[1]][[1]],5), round(dists,5))
})

test_that('Model compiler (jaccard distances)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1), c(1:5,80)), 1, 1, 1:5)
	sim_ru@data@attribute.spaces[[1]]@distance.metric='jaccard'
	# generate model code
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	# calculate weighted distances
	pts<-rbind(sim_ru@data@attribute.spaces[[1]]@pu@coords,sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)
	dists<-suppressWarnings(unname(as.matrix(vegan::vegdist(pts, method='jaccard'))))
	dists<-dists[
		nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords)+seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)),
		seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords))
	]
	dists<-(dists * matrix(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@weights, byrow=TRUE, ncol=ncol(dists), nrow=nrow(dists)))+1e-5
	# run tests
	expect_equal(round(model$cache$wdist[[1]][[1]][-1],4), round(dists[-1],4))	
})

test_that('Model compiler (kulczynski distances)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1), c(1:5,80)), 1, 1, 1:5)
	sim_ru@data@attribute.spaces[[1]]@distance.metric='kulczynski'
	# generate model code
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	# calculate weighted distances
	pts<-rbind(sim_ru@data@attribute.spaces[[1]]@pu@coords,sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)
	dists<-suppressWarnings(unname(as.matrix(vegan::vegdist(pts, method='kulczynski'))))
	dists<-dists[
		nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords)+seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)),
		seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords))
	]
	dists<-(dists * matrix(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@weights, byrow=TRUE, ncol=ncol(dists), nrow=nrow(dists)))+1e-5
	# run tests
	expect_equal(round(model$cache$wdist[[1]][[1]][is.finite(dists)],4), round(dists[is.finite(dists)],4))
})

test_that('Model compiler (mahalanobis distances)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1), c(1:5,80)), 1, 1, 1:5)
	sim_ru@data@attribute.spaces[[1]]@distance.metric='mahalanobis'
	# generate model code
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	# calculate weighted distances
	pts<-rbind(sim_ru@data@attribute.spaces[[1]]@pu@coords,sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)
	pts<-apply(pts, 2, function(x){x-mean(x)})
	cov_mat<-var(pts)
	dists<-matrix(NA, nrow=nrow(pts), ncol=nrow(pts))
	for (i in seq_len(nrow(pts)))
		for (j in seq_len(nrow(pts)))
		dists[i,j]<-sqrt(mahalanobis(pts[i,], pts[j,], cov_mat))
	dists<-dists[
		nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords)+seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords)),
		seq_len(nrow(sim_ru@data@attribute.spaces[[1]]@pu@coords))
	]
	dists<-(dists * matrix(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@weights, byrow=TRUE, ncol=ncol(dists), nrow=nrow(dists)))+1e-5
	# run tests
	expect_equal(round(model$cache$wdist[[1]][[1]],5), round(dists,5))
})

test_that('Gurobi solver (unreliable)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)	
	data(sim_ru)
	sim_ru <- pu.subset(spp.subset(sim_ru, 1:2), 1:10)
	sim_ru@data@attribute.spaces[[1]] = AttributeSpace(
		pu=sim_ru@data@attribute.spaces[[1]]@pu,
		demand.points=list(
			DemandPoints(
				SimplePoints(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords[1:10,]),
				sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@weights[1:10]
			),
			DemandPoints(
				SimplePoints(sim_ru@data@attribute.spaces[[1]]@demand.points[[2]]@points@coords[1:10,]),
				sim_ru@data@attribute.spaces[[1]]@demand.points[[2]]@weights[1:10]
			)
		),
		distance.metric='euclidean'
	)
	# generate model matrix
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	model$A<-Matrix::sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]])
	# solve the model
	result<-gurobi::gurobi(model, append(as.list(GurobiOpts(MIPGap=0.99, Presolve=1L)), list('LogFile'=tempfile(fileext='.log'))))
	if (file.exists('gurobi.log')) unlink('gurobi.log')
	# check solution variables
	expect_true(all(result$x[grep('X_',model$cache$variables,fixed=TRUE)] %in% c(0,1)))
	expect_true(all(result$x[grep('Y_',model$cache$variables,fixed=TRUE)] %in% c(0,1)))
})

test_that('Gurobi solver (reliable)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)	
	data(sim_ru)
	sim_ru <- spp.subset(pu.subset(sim_ru, 1:10), 1:2)
	sim_ru@data@attribute.spaces[[1]] = AttributeSpace(
		pu=sim_ru@data@attribute.spaces[[1]]@pu,
		demand.points=list(
			DemandPoints(
				SimplePoints(sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@points@coords[1:10,]),
				sim_ru@data@attribute.spaces[[1]]@demand.points[[1]]@weights[1:10]
			),
			DemandPoints(
				SimplePoints(sim_ru@data@attribute.spaces[[1]]@demand.points[[2]]@points@coords[1:10,]),
				sim_ru@data@attribute.spaces[[1]]@demand.points[[2]]@weights[1:10]
			)
		),
		distance.metric='euclidean'
	)
	# generate model code
	model<-rcpp_generate_model_object(RapReliableOpts(), FALSE, sim_ru@data, FALSE)
	model$A<-Matrix::sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]])
	# solve the model
	result<-gurobi::gurobi(model, append(as.list(GurobiOpts(MIPGap=0.99, Presolve=1L)), list('LogFile'=tempfile(fileext='.log'))))
	if (file.exists('gurobi.log')) unlink('gurobi.log')
	# checks
	expect_true(all(result$x[grep('X_',model$cache$variables,fixed=TRUE)] %in% c(0,1)))
	expect_true(all(result$x[grep('Y_',model$cache$variables,fixed=TRUE)] %in% c(0,1)))
	expect_true(all(round(result$x[grep('P_',model$cache$variables,fixed=TRUE)],5) >= 0))
	expect_true(all(round(result$x[grep('P_',model$cache$variables,fixed=TRUE)],5) <= 1))
	expect_true(all(
		round(result$x[grep('W_',model$cache$variables,fixed=TRUE)],5) ==  round(
			result$x[grep('Y_',model$cache$variables,fixed=TRUE)] * result$x[grep('P_',model$cache$variables,fixed=TRUE)],
			5
		)
	))
})

test_that('solve.RapUnsolved (unreliable - NUMREPS=1)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru@data@targets[[3]]=c(0.1,0.1,0.1,0.5,0.5,0.5)
	sim_ru@opts=RapUnreliableOpts()
	# solve it
	sim_rs<-rapr::solve(sim_ru, GurobiOpts(MIPGap=0.05, Presolve=2L))
	# check number of selections is 1
	expect_equal(nrow(summary(sim_rs)), 1L)
	expect_true(all(c(space.held(sim_rs))>=c(space.target(sim_rs))))
	expect_true(all(c(amount.held(sim_rs))>=c(amount.target(sim_rs))))
	# check that amount.held is correct
	pu_ids<-which(as.logical(sim_rs@results@selections[1,]))
	for (i in seq_along(sim_rs@data@species$name)) {
			# init
			curr_df=sim_rs@data@pu.species.probabilities[which(
				sim_rs@data@pu.species.probabilities[[1]] == i
			),]
			curr_df$area=sim_rs@data@pu$area[curr_df$pu]
			curr_df$benefit=curr_df[[3]] * curr_df[[4]]
			# calculate metrics
			curr_max<-sum(curr_df$benefit)
			curr_amount=sum(curr_df$benefit[curr_df$pu %in% pu_ids])
			# tests
			expect_equal(
				round(curr_amount/curr_max,5),
				round(amount.held(sim_rs)[i],5)
			)
	}
	# check that space.held is correct
	pu_ids<-which(as.logical(sim_rs@results@selections[1,]))
	for (i in seq_along(sim_rs@data@species$name))
		for (j in seq_along(sim_rs@data@attribute.spaces)) {
			# init
			curr_df=sim_rs@data@pu.species.probabilities[which(
				sim_rs@data@pu.species.probabilities[[1]] == i
			),]
			pu_pts=sim_rs@data@attribute.spaces[[j]]@pu@coords[which(seq_len(nrow(sim_rs@data@pu)) %in% curr_df[[2]]),]
			dp_pts=sim_rs@data@attribute.spaces[[j]]@demand.points[[i]]@points@coords
			dp_wts=sim_rs@data@attribute.spaces[[j]]@demand.points[[i]]@weights
			dist_mtx=fields::rdist(dp_pts, pu_pts)
			dp_wts_mtx=matrix(rep(dp_wts, nrow(pu_pts)), ncol=nrow(pu_pts),nrow=length(dp_wts))
			wdist_mtx=(dist_mtx*dp_wts_mtx) + 1e-05
			# calculate space.held in solution
			curr_solution=sum(apply(wdist_mtx[,pu_ids,drop=FALSE], 1, min))
			# calculate space.held in worst solution
			curr_worst=max(apply(wdist_mtx, 2, sum))
			# calculate space.held in best solution
			curr_best=sum(apply(wdist_mtx, 1, min))
			# tests	
			expect_equal(
				round(((curr_worst-curr_solution ) / (curr_worst-curr_best)),5),
				round(space.held(sim_rs, species=i, space=j)[1],5)
			)
		}
})

test_that('solve.RapUnsolved (reliable - NUMREPS=1)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)	
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:3)
	sim_ru@opts=RapReliableOpts(failure.multiplier=10)
	sim_ru@data@targets[[3]]=c(0.5,0.5,0.5,0.99,0.99,0.99)
	# solve it
	sim_rs<-rapr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L))
	# check number of selections is 1
	expect_equal(nrow(summary(sim_rs)), 1L)
	expect_true(all(c(amount.held(sim_rs))>=c(amount.target(sim_rs))))
	expect_true(all(c(space.held(sim_rs))>=c(space.target(sim_rs))))
	# check that amount.held is correct
	pu_ids<-which(as.logical(sim_rs@results@selections[1,]))
	for (i in seq_along(sim_rs@data@species$name)) {
			# init
			curr_df=sim_rs@data@pu.species.probabilities[which(
				sim_rs@data@pu.species.probabilities[[1]] == i
			),]
			curr_df$area=sim_rs@data@pu$area[curr_df$pu]
			curr_df$benefit=curr_df[[3]] * curr_df[[4]]
			# calculate metrics
			curr_max<-sum(curr_df$benefit)
			curr_amount<-sum(curr_df$benefit[curr_df$pu %in% pu_ids])
			# tests
			expect_equal(
				round(curr_amount/curr_max,5),
				round(amount.held(sim_rs)[i],5)
			)
	}
	## check that space.held is correct
	# run tests
	pu_ids<-which(as.logical(sim_rs@results@selections[1,]))	
	for (i in seq_along(sim_rs@data@species$name))
		for (j in seq_along(sim_rs@data@attribute.spaces)) {
			# init
			curr_df=sim_rs@data@pu.species.probabilities[which(
				sim_rs@data@pu.species.probabilities[[1]] == i
			),]
			pu_pts=sim_rs@data@attribute.spaces[[j]]@pu@coords[which(seq_len(nrow(sim_rs@data@pu)) %in% curr_df[[2]]),]
			dp_pts=sim_rs@data@attribute.spaces[[j]]@demand.points[[i]]@points@coords
			dp_wts=sim_rs@data@attribute.spaces[[j]]@demand.points[[i]]@weights
			dist_mtx=fields::rdist(dp_pts, pu_pts)
			dp_wts_mtx=matrix(rep(dp_wts, nrow(pu_pts)), ncol=nrow(pu_pts),nrow=length(dp_wts))
			wdist_mtx=(dist_mtx*dp_wts_mtx) + 1e-05
			wdist_mtx=cbind(wdist_mtx, max(wdist_mtx)*sim_rs@opts@failure.multiplier)
			# calculate space.held in solution
			curr_solution=calcReliableSpaceHeld(wdist_mtx, curr_df[[3]], sim_rs@opts@max.r.level, pu_ids)
			# calculate space.held in worst solution
			curr_worst=max(sapply(curr_df$pu, calcReliableSpaceHeld, w=wdist_mtx, p=curr_df[[3]], maxr=sim_rs@opts@max.r.level))
			# calculate space.held in best solution
			curr_best=calcReliableSpaceHeld(wdist_mtx, curr_df[[3]], sim_rs@opts@max.r.level, curr_df$pu)
			# tests
			expect_equal(
				round(((curr_worst-curr_solution ) / (curr_worst-curr_best)),5),
				round(space.held(sim_rs, species=i, space=j)[1],5)
			)
		}
})

test_that('solve.RapUnsolved (unreliable - NUMREPS=1 - sparse occupancy)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)	
	data(sim_ru)
	sim_ru<-pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:3)
	sim_ru@data@targets[[3]]=c(0.1,0.1,0.1,0.99,0.99,0.99)
	sim_ru@opts=RapUnreliableOpts()
	sim_ru@data@pu.species.probabilities=sim_ru@data@pu.species.probabilities[sample(
		seq_len(nrow(sim_ru@data@pu.species.probabilities)),
		size=ceiling(nrow(sim_ru@data@pu.species.probabilities)*0.7)
	),]
	# solve it
	sim_rs<-rapr::solve(sim_ru, GurobiOpts(MIPGap=0.05, Presolve=2L))
	# check number of selections is 1
	expect_equal(nrow(summary(sim_rs)), 1L)
	expect_true(all(c(amount.held(sim_rs))>=c(amount.target(sim_rs))))
	expect_true(all(c(space.held(sim_rs))>=c(space.target(sim_rs))))
	# check that amount.held is correct
	pu_ids<-which(as.logical(sim_rs@results@selections[1,]))
	for (i in seq_along(sim_rs@data@species$name)) {
			# init
			curr_df=sim_rs@data@pu.species.probabilities[which(
				sim_rs@data@pu.species.probabilities[[1]] == i
			),]
			curr_df$area=sim_rs@data@pu$area[curr_df$pu]
			curr_df$benefit=curr_df[[3]] * curr_df[[4]]
			# calculate metrics
			curr_max<-sum(curr_df$benefit)
			curr_amount=sum(curr_df$benefit[curr_df$pu %in% pu_ids])
			# tests
			expect_equal(
				round(curr_amount/curr_max,5),
				round(amount.held(sim_rs)[i],5)
			)
	}
	# check that space.held is correct
	pu_ids<-which(as.logical(sim_rs@results@selections[1,]))
	for (i in seq_along(sim_rs@data@species$name))
		for (j in seq_along(sim_rs@data@attribute.spaces)) {
			# init
			curr_df=sim_rs@data@pu.species.probabilities[which(
				sim_rs@data@pu.species.probabilities[[1]] == i
			),]
			pu_pts=sim_rs@data@attribute.spaces[[j]]@pu@coords
			rownames(pu_pts)=as.character(seq_len(nrow(sim_rs@data@pu)))
			pu_pts=pu_pts[which(seq_len(nrow(pu_pts)) %in% curr_df[[2]]),]
			dp_pts=sim_rs@data@attribute.spaces[[j]]@demand.points[[i]]@points@coords
			dp_wts=sim_rs@data@attribute.spaces[[j]]@demand.points[[i]]@weights
			dist_mtx=fields::rdist(dp_pts, pu_pts)
			dp_wts_mtx=matrix(rep(dp_wts, nrow(pu_pts)), ncol=nrow(pu_pts),nrow=length(dp_wts))
			wdist_mtx=(dist_mtx*dp_wts_mtx) + 1e-05
			# calculate space.held in solution
			curr_solution=sum(apply(wdist_mtx[,rownames(pu_pts) %in% as.character(pu_ids), drop=FALSE], 1, min))
			# calculate space.held in worst solution
			curr_worst=max(apply(wdist_mtx, 2, sum))
			# calculate space.held in best solution
			curr_best=sum(apply(wdist_mtx, 1, min))
			# tests
			expect_equal(
				round(((curr_worst-curr_solution ) / (curr_worst-curr_best)),5),
				round(space.held(sim_rs, species=i, space=j)[1],5)
			)
		}
})

test_that('solve.RapUnsolved (reliable - NUMREPS=1 - sparse occupancy)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:3)
	sim_ru@data@pu.species.probabilities=sim_ru@data@pu.species.probabilities[sample(
		seq_len(nrow(sim_ru@data@pu.species.probabilities)),
		ceiling(nrow(sim_ru@data@pu.species.probabilities)*0.7)
	),]
	sim_ru@opts=RapReliableOpts()
	sim_ru@data@targets[[3]]=c(0.5,0.5,0.5,0.9,0.9,0.9)
	# solve it
	sim_rs<-rapr::solve(sim_ru, GurobiOpts(MIPGap=0.15, Presolve=2L), verbose=TRUE)
	# check number of selections is 1
	expect_equal(nrow(summary(sim_rs)), 1L)
	expect_true(all(c(space.held(sim_rs))>=c(space.target(sim_rs))))
	expect_true(all(c(amount.held(sim_rs))>=c(amount.target(sim_rs))))
	# check that amount.held is correct
	pu_ids<-which(as.logical(sim_rs@results@selections[1,]))
	for (i in seq_along(sim_rs@data@species$name)) {
			# init
			curr_df=sim_rs@data@pu.species.probabilities[which(
				sim_rs@data@pu.species.probabilities[[1]] == i
			),]
			curr_df$area=sim_rs@data@pu$area[curr_df$pu]
			curr_df$benefit=curr_df[[3]] * curr_df[[4]]
			# calculate metrics
			curr_max<-sum(curr_df$benefit)
			curr_amount<-sum(curr_df$benefit[curr_df$pu %in% pu_ids])
			# tests
			expect_equal(
				round(curr_amount/curr_max,5),
				round(amount.held(sim_rs)[i],5)
			)
	}
	## check that space.held is correct
	# run tests
	pu_ids<-which(as.logical(sim_rs@results@selections[1,]))
	for (i in seq_along(sim_rs@data@species$name))
		for (j in seq_along(sim_rs@data@attribute.spaces)) {
			# init
			curr_df=sim_rs@data@pu.species.probabilities[which(
				sim_rs@data@pu.species.probabilities[[1]] == i
			),]
			pu_pts=sim_rs@data@attribute.spaces[[j]]@pu@coords[curr_df[[2]],]
			rownames(pu_pts)=as.character(curr_df$pu)
			pu_pos=which(rownames(pu_pts) %in% as.character(pu_ids))			
			dp_pts=sim_rs@data@attribute.spaces[[j]]@demand.points[[i]]@points@coords
			dp_wts=sim_rs@data@attribute.spaces[[j]]@demand.points[[i]]@weights
			dist_mtx=fields::rdist(dp_pts, pu_pts)
			dp_wts_mtx=matrix(rep(dp_wts, nrow(pu_pts)), ncol=nrow(pu_pts),nrow=length(dp_wts))
			wdist_mtx=(dist_mtx*dp_wts_mtx) + 1e-05
			wdist_mtx=cbind(wdist_mtx, max(wdist_mtx)*sim_rs@opts@failure.multiplier)
			# calculate space.held in solution
			curr_solution=calcReliableSpaceHeld(wdist_mtx, curr_df[[3]], sim_rs@opts@max.r.level, pu_pos)
			# calculate space.held in worst solution
			curr_worst=max(sapply(seq_len(nrow(pu_pts)), calcReliableSpaceHeld, w=wdist_mtx, p=curr_df[[3]], maxr=sim_rs@opts@max.r.level))
			# calculate space.held in best solution
			curr_best=calcReliableSpaceHeld(wdist_mtx, curr_df[[3]], sim_rs@opts@max.r.level, seq_len(nrow(pu_pts)))
			# tests
			expect_equal(
				round(((curr_worst-curr_solution ) / (curr_worst-curr_best)),3),
				round(space.held(sim_rs, species=i, space=j)[1],3)
			)
		}
})

test_that('solve.RapUnsolved (unreliable - NumberSolutions=2)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru@opts=RapUnreliableOpts()
	# solve it
	sim_rs<-rapr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L, NumberSolutions=2L))
	# check number of selections is 1
	expect_equal(nrow(summary(sim_rs)), 2L)
})

test_that('solve.RapUnsolved (reliable - NumberSolutions=2)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru@opts=RapReliableOpts()
	# solve it
	sim_rs<-rapr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L, NumberSolutions=2L))
	# check number of selections is 1
	expect_equal(nrow(summary(sim_rs)), 2L)
})

test_that('solve.RapUnsolved (unreliable - STATUS test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru@opts=RapUnreliableOpts()
	# lock in and lock out planning units
	sim_ru@data@pu$status[1]=0
	sim_ru@data@pu$status[2]=2
	sim_ru@data@pu$status[3]=3
	# solve it
	sim_rs<-rapr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L))
	# check number of selections is 1
	expect_identical(selections(sim_rs)[2], 1L)
	expect_identical(selections(sim_rs)[3], 0L)
})

test_that('solve.RapUnsolved (reliable - STATUS test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru@opts=RapReliableOpts()
	# lock in and lock out planning units
	sim_ru@data@pu$status[1]=0
	sim_ru@data@pu$status[2]=2
	sim_ru@data@pu$status[3]=3
	# solve it
	sim_rs<-rapr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L))
	# check number of selections is 1
	expect_identical(selections(sim_rs)[2], 1L)
	expect_identical(selections(sim_rs)[3], 0L)
})

test_that('solve.RapUnsolved (unreliable - BLM test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru@opts=RapUnreliableOpts(BLM=100)
	# solve it
	sim_rs<-rapr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L))
})

test_that('solve.RapUnsolved (reliable - BLM test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru@opts=RapReliableOpts(BLM=100)
	# solve it
	sim_rs<-rapr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L))
})

test_that('update.RapUnsolved', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	# update object
	sim_ru2 <- update(sim_ru, BLM=100, name=letters[1:3], solve=FALSE)
	# checks
	expect_equal(sim_ru2@opts@BLM, 100)
	expect_equal(sim_ru2@data@species$name, letters[1:3])
})

test_that('update.RapUnsolved (formulation argument)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	# update object
	sim_ru@opts@BLM=100
	sim_ru2 <- update(sim_ru, formulation='reliable', solve=FALSE)
	# checks
	expect_equal(sim_ru2@opts@BLM, 100)
	expect_is(sim_ru2@opts,'RapReliableOpts')
})

test_that('amount.target.RapData', {
	data(sim_ru)
	expect_equal(
		unname(amount.target(sim_ru)),
		rep(0.2, 3)
	)
	expect_equal(
		unname(amount.target(sim_ru, 1)),
		0.2
	)
})

test_that('amount.target<-.RapUnsolved', {
	data(sim_ru)
	amount.target(sim_ru)<-0.3
	expect_equal(unname(amount.target(sim_ru)), rep(0.3, 3))
	amount.target(sim_ru, 1)<-0.5
	expect_equal(unname(amount.target(sim_ru)), c(0.5, 0.3, 0.3))
})

test_that('space.target.RapData', {
	data(sim_ru)
	expect_equal(
		unname(space.target(sim_ru@data)[,1]),
		rep(0.2, 3)
	)
	expect_equal(
		unname(space.target(sim_ru@data, species=1)[,1]),
		rep(0.2)
	)
	expect_equal(
		unname(space.target(sim_ru@data, space=1)[,1]),
		rep(0.2, 3)
	)
	expect_equal(
		unname(space.target(sim_ru@data, species=1, space=1)[,1]),
		0.2
	)
})

test_that('space.target<-.RapUnsolved', {
	data(sim_ru)
	space.target(sim_ru)<-0.3
	expect_equal(unname(space.target(sim_ru)[,1]), rep(0.3, 3))
	space.target(sim_ru, 1)<-0.5
	expect_equal(unname(space.target(sim_ru)[,1]), c(0.5, 0.3, 0.3))
})
