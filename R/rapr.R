#' rapr: Representative and Adequate Prioritisations in R
#'
#' Biodiversity is in crisis. The overarching aim of conservation is to preserve biodiversity patterns and processes. To this end, protected areas are established to buffer species and preserve biodiversity processes. But resources are limited and so protected areas must be cost-effective. This package contains functions to generate plans for protected areas. Conservation planning data are used to construct an optimisation problem, which in turn is then solved to yield prioritisations. Amount-based targets can be used to identify prioritisations that contain an adequate amount of the target species. Additionally, space-based targets can be used to ensure that a representative sample of the target species is preserved. To solve the optimisation problems in a feasible amount of time, this package uses the commerical 'Gurobi' software package (obtained from <http://www.gurobi.com/>).

#' The main classes used in this package are used to store input data and prioritisations:
#' \itemize{
#'   \item \code{\link{GurobiOpts-class}}: parameters for solving optimisation problems using Gurobi.
#'   \item \code{\link{RapReliableOpts-class}}: parameters for the reliable formulation of RAP.
#'   \item \code{\link{RapUnreliableOpts-class}}: parameters for the unreliable formulation of RAP.
#'   \item \code{\link{RapData-class}}: planning unit, species data, and demand points for RAP.
#'   \item \code{\link{RapUnsolved-class}}: contains all the data and input parameters required to generate prioritisations using RAP. This class contains a \code{\link{GurobiOpts-class}} object, a \code{\link{RapReliableOpts-class}} or \code{\link{RapUnreliableOpts-class}} object, and a \code{\link{RapData-class}} object.
#'   \item \code{\link{RapResults-class}}: prioritisations and summary statistics on their performance.
#'   \item \code{\link{RapSolved-class}}: contains all the input data, parameters and output data. This class contains all the objects in a \code{\link{RapUnsolved}} object and also a \code{\link{RapResults-class}} object.
#' }
#'
#' Type \code{vignette('rapr')} for a tutorial on how to use this package.
#'
#' @docType package
#' @name rapr
NULL
