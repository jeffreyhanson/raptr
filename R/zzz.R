.onLoad <- function(libname, pkgname) {
	# check gurobi package installed
	if (!is.GurobiInstalled(verbose=TRUE)) {
		cat('\n\nType the code \'is.GurobiInstalled()\' to check if Gurobi is succesfully installed.')
	}
}
