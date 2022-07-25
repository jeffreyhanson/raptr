#' @include raptr-internal.R
NULL

#' Simulated dataset for a conservation planning exercise
#'
#' This dataset contains all the data needed to generate prioritizations for
#' three simulated species. This dataset contains planning units, species
#' distribution maps, and demand points for each species. For the purposes of
#' exploring the behaviour of the problem, demand points were generated using
#' the centroids of planning units and the probability that they are occupied
#' by the species. Note that methodology is not encouraged for real-world
#' conservation planning.
#'
#' The species were simulated to represent various simplified species
#' distributions.
#'
#' \describe{
#' \item{uniform}{This species has an equal probability (0.5) of occurring in
#' all planning units.}
#' \item{normal}{This species has a single range-core where it is most likely
#' to be found. It is less likely to be found in areas further
#' away from the center of its range.}
#' \item{bimodal}{This species has two distinct ecotypes. Each ecotype has its
#' own core and marginal area.}
#' }
#'
#' @docType data
#'
#' @format
#' \describe{
#' \item{sim_ru}{[RapUnsolved()] object with all the simulated data.}
#' \item{sim_rs}{[RapSolved()] object with 5 near-optimal solutions.}
#' }
#'
#' @aliases sim_ru sim_rs
#'
#' @keywords datasets
#'
#' @name simulated_data
#'
#' @examples
#' # load data
#' data(sim_ru, sim_rs)
#'
#' # plot species distributions
#' spp.plot(sim_ru, 1)
#' spp.plot(sim_ru, 2)
#' spp.plot(sim_ru, 3)
#'
#' # plot selection frequencies
#' plot(sim_rs)
#'
#' # plot best solution
#' plot(sim_rs, 0)
NULL

#' Case-study dataset for a conservation planning exercise
#'
#' This dataset contains data to generate example prioritizations for the
#' pale-headed Rosella (*Platycercus adscitus*) in Queensland, Australia.
#'
#' The objects in the dataset are listed below.
#' \describe{
#' \item{cs_pus}{[sp::SpatialPolygonsDataFrame()] planning units.
#' The units were generated as \eqn{30km^2} squares across the
#' species' range, and then clipped to the Queensland, Australia
#' (using data obtained from the Australia Bureau of Statistics; <https://www.abs.gov.au/ausstats/abs@@.nsf/mf/1259.0.30.001?OpenDocument>).
#' They were then overlaid with Australia's protected area
#' network (obtained from the World Database on Protected Areas
#' (WDPA) at <https://www.protectedplanet.net/en>). This
#' attribute table has 3 fields. The `area` field denotes
#' the amount of land encompassed by each unit, the `cost`
#' field is set to 1 for all units, and the `status` field
#' indicates if 50% or more of the units' extent is covered by
#' protected areas.}
#' \item{cs_spp}{[raster::raster()] probability
#' distribution map for the *P. adscitus* clipped to
#' Queensland, Australia. This map was derived from records
#' obtained from The Atlas of Living Australia (<https://ala.org.au/>)}.
#' \item{cs_space}{[raster::stack()] describing broad-scale climate
#' variation across Queensland
#' (obtained from <https://worldclim.org/>,
#' and resampled to \eqn{10km^2} resolution).}
#' }
#'
#' @docType data
#'
#' @aliases cs_pus cs_spp cs_space
#'
#' @format
#' \describe{
#' \item{cs_pus}{[sp::SpatialPolygonsDataFrame()] object.}
#' \item{cs_spp}{[raster::raster()] object.}
#' \item{cs_space}{[raster::stack()] object.}
#' }
#'
#' @keywords datasets
#'
#' @name casestudy_data
#'
#' @examples
#' # load data
#' data(cs_pus, cs_spp, cs_space)
#'
#' # plot data
#' \dontrun{
#' plot(cs_pus)
#' plot(cs_spp)
#' plot(cs_space)
#' }
NULL
