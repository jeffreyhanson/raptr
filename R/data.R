#' Simulated dataset for a conservation planning exercise
#' 
#' This dataset contains all the data needed to generate prioritisations for three simulated species. This dataset contains planning units, species distribution
#' maps, and demand points for each species. For the purposes of exploring the behaviour of the problem, demand points were generated using the centroids of planning units
#' and the probability that they are occupied by the species. Note that methodology is not encouraged for real-world conservation planning.
#' The species were simulated to represent different simplified versions of species distributions encountered:
#' \itemize{
#'	\item constant: This species has an equal probability (0.5) of occuring in all planning units.  
#'	\item normal: This species has a single range-core where it is most likely to be found. It is less likely to be found in areas further away from the centre of its range.
#'	\item bimodal: This species has two distinct ecotypes. Each ecotype has its own range cores and range marginal areas.
#' }
#' 
#' The objects contained in this dataset are:
#' \itemize{
#'	\item sim_pus: A \code{SpatialPolygonsDataFrame} with 225 planning units.
#'	\item sim_species: A \code{RasterStack} containg probability distribution data for each of the species.
#'	\item sim_demandpoints: A \code{list} of \code{DemandPoints} for each species.
#' }
#'
#' @docType data
#' @aliases sim_pus
#' @aliases sim_spp
#' @aliases sim_dps
#' @keywords datasets
#' @name simulated_data
#' @examples
#' # load data
#' data(sim_plus)
#' data(sim_spp)
#' data(sim_dps)
#' # plot data
#' plot(sim_plus)
#' plot(sim_spp)
NULL


#' Case-study dataset for a conservation planning exercise
#' 
#' This dataset contains data to generate example prioritisations for the pale-headed Rosella (\textit{Platycercus adscitus}). Specific objects in the dataset include:
#' \itemize{
#'	\item cs_spp: \code{RasterLayer} probability distribution map for the \textit{P. adscitus} dervived from records in the BirdLife Altas Database (\url{http://birdata.com.au/}).
#'	\item cs_pus: \code{SpatialPolygonsDataFrame} planning units. The units were generated as $10km^2$ squares across the species' range, and then clipped to the coast-line. They were then overliad with Australia's protected area network (obtained from the World Database on Protected Areas (WDPA) at \url{http://www.protectedplanet.net/}). This attribute table has 3 fields. The 'area' field contains the units' area, the 'cost' field is set to 1 for all units, and the 'status' filed indicates if 50\% or moer of the units' extent is covered by protected areas.
#'	\item cs_bio12: \code{RasterLayer} containing annual precipitation data across the species range (obtained from \url{http://www.worldclim.org/} as BIO12, and resampled ot $10km^2$ resolution).
#' }
#'
#' @docType data
#' @aliases cs_pus
#' @aliases cs_spp
#' @aliases cs_bio12
#' @keywords datasets
#' @name casestudy_data
#' @examples
#' # load data
#' data(cs_pus)
#' data(cs_spp)
#' data(cs_bio12)
#' # plot data
#' spplot(cs_pus, field=status)
#' plot(cs_spp)
#' plot(cs_bio12)
NULL

