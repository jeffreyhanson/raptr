#' raptr: Representative and Adequate Prioritization Toolkit in R
#'
#' Biodiversity is in crisis. The overarching aim of conservation is to preserve
#' biodiversity patterns and processes. To this end, protected areas are
#' established to buffer species and preserve biodiversity processes. But
#' resources are limited and so protected areas must be cost-effective. This
#' package contains tools to generate plans for protected areas
#' (prioritizations). Conservation planning data are used to construct an
#' optimization problem, which is then solved to yield prioritizations. To
#' solve the optimization problems in a feasible amount of time, this package
#' uses the commercial 'Gurobi' software package (obtained from
#' <https://www.gurobi.com/>). For more information on using
#' this package, see Hanson et al. (2018).
#'
#' The main classes used in this package are used to store input data and
#' prioritizations:
#' \describe{
#' \item{[GurobiOpts-class]}{parameters for solving optimization
#' problems using Gurobi.}
#' \item{[RapReliableOpts-class]}{parameters for the reliable
#' formulation of RAP.}
#' \item{[RapUnreliableOpts-class]}{parameters for the unreliable
#' formulation of RAP.}
#' \item{[RapData-class]}{planning unit, species data, and demand
#' points for RAP.}
#' \item{[RapUnsolved-class]}{contains all the data and input
#' parameters required to generate prioritizations using RAP. This class
#' contains a [GurobiOpts-class] object, a
#' [RapReliableOpts-class] or [RapUnreliableOpts-class]
#' object, and a [RapData-class] object.}
#' \item{[RapResults-class]}{prioritizations and summary
#' statistics on their performance.}
#' \item{[RapSolved-class]}{contains all the input data,
#' parameters and output data. This class contains all the objects in a
#' [RapUnsolved()] object and also a [RapResults-class]
#' object.}
#' }
#'
#' Type `vignette("raptr")` for a tutorial on how to use this package.
#'
#' @references
#' Hanson JO, Rhodes JR, Possingham HP & Fuller RA (2018)
#' raptr: Representative and Adequate Prioritization",
#' Toolkit in R. _Methods in Ecology & Evolution_,",
#' **9**: 320--330. DOI: 10.1111/2041-210X.12862.
#'
#' @docType package
#' @name raptr
NULL

# define function to avoid CRAN check issue
#' @importFrom rgdal readOGR
#' @importFrom withr with_locale
NULL
