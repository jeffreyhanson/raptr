context('10-Distances')
source('functions.R')

# test that model compiler functions don't crash
test_that('Model compiler (unreliable)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1), 1:10), species=1, space=1, points=1:20)
	# generate model code
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	model$A<-Matrix::sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]], dims=c(max(model$Ar[[1]])+1, length(model$obj)))
})

test_that('Model compiler (reliable)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1), 1:10), species=1, space=1, points=1:20)
	# generate model code
	model<-rcpp_generate_model_object(RapReliableOpts(), FALSE, sim_ru@data, FALSE)
	model$A<-Matrix::sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]], dims=c(max(model$Ar[[1]])+1, length(model$obj)))
})

# check distance calculations
dist.names <-  c('euclidean', 'bray', 'manhattan', 'gower', 'canberra', 'jaccard', 'kulczynski', 'mahalanobis', 'minkowski')
for (d in dist.names) {
	test_that(paste0('unreliable distance calculations (',d,')'), {
		set.seed(500)
		data(sim_ru)
		sim_ru@data@attribute.spaces[[1]]@distance.metric=d
		sim_ru <- dp.subset(pu.subset(sim_ru, 1:10), species=1:3, space=1, points=1:20)
		# make sure all coordinates are > 0 
		for (j in seq_along(sim_ru@data@attribute.spaces)) {
			sim_ru@data@attribute.spaces[[j]]@pu@coords <- sim_ru@data@attribute.spaces[[j]]@pu@coords+100
			for (i in seq_along(sim_ru@data@attribute.spaces[[j]]@demand.points)) {
				sim_ru@data@attribute.spaces[[j]]@demand.points[[i]]@points@coords <- sim_ru@data@attribute.spaces[[j]]@demand.points[[i]]@points@coords+100
			}
		}
		model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
		# run tests
		for (j in seq_along(sim_ru@data@attribute.spaces)) {
			for (i in seq_along(sim_ru@data@attribute.spaces[[j]]@demand.points)) {
				# calculate weighted distances
				dists<-calcUnreliableMetrics(sim_ru@data,space=j,species=i)
				# tests
				expect_equal(round(model$cache$wdist[[i]][[j]],5), round(dists$wdist,5))
				expect_equal(round(model$cache$tss[[i]][[j]],5), round(dists$tss,5))
			}
		}
	})
	test_that(paste0('reliable distance calculations (',d,')'), {
		set.seed(500)
		data(sim_ru)
		sim_ru <- dp.subset(pu.subset(sim_ru, 1:10), species=1:3, space=1, points=1:20)
		sim_ru@data@attribute.spaces[[1]]@distance.metric=d
		sim_ru@opts <- RapReliableOpts(failure.multiplier=10)
		# make sure all coordinates are > 0 
		for (j in seq_along(sim_ru@data@attribute.spaces)) {
			sim_ru@data@attribute.spaces[[j]]@pu@coords <- sim_ru@data@attribute.spaces[[j]]@pu@coords+100
			for (i in seq_along(sim_ru@data@attribute.spaces[[j]]@demand.points)) {
			sim_ru@data@attribute.spaces[[j]]@demand.points[[i]]@points@coords <- sim_ru@data@attribute.spaces[[j]]@demand.points[[i]]@points@coords+100
			}
		}
		# generate model code
		model<-rcpp_generate_model_object(sim_ru@opts, FALSE, sim_ru@data, FALSE)
		# run tests
		for (j in seq_along(sim_ru@data@attribute.spaces)) { 
			for (i in seq_along(sim_ru@data@attribute.spaces[[j]]@demand.points)) {
				# calculate weighted distances
				dists<-calcReliableMetrics(sim_ru@data,space=j,species=i,sim_ru@opts)
				# tests
				expect_equal(round(model$cache$wdist[[i]][[j]],5), round(dists$wdist,5))
				expect_equal(round(model$cache$tss[[i]][[j]],5), round(dists$tss,5))
			}
		}
	})
}
