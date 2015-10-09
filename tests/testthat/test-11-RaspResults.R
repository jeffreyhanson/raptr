test_that('RaspResults', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled()) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	pth<-file.path(tempdir(), 'gmodel')
	cat('path = ', pth, '.lp\n')
	set.seed(500)
	data(small.pus, small.species)
	ro<-RaspOpts(MAXRLEVEL=5L, FAILUREMULTIPLIER=1000)
	rd<-make.RaspData(small.pus, small.species, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	# generate model code
	model<-rcpp_generate_modelfile(ro, rd)
	# solve the model
	writeLines(model, paste0(pth, '.lp'))
	result<-call.Gurobi(GurobiOpts(MIPGap=0.9), paste0(pth, '.lp'), paste0(pth, '.log'), paste0(pth, '.sol'))
	# load results
	results<-read.RaspResults(ro,rd,readLines(paste0(pth, '.lp')),readLines(paste0(pth, '.log')),readLines(paste0(pth, '.sol')))
})

 
