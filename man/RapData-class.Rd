% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RapData.R
\docType{class}
\name{RapData-class}
\alias{RapData-class}
\title{RapData: An S4 class to represent RAP input data}
\description{
This class is used to store RAP input data.
}
\section{Slots}{

\describe{
\item{\code{polygons}}{\code{\link[PBSmapping:PolySet]{PBSmapping::PolySet()}} planning unit spatial data
or \code{NULL} if data not available.}

\item{\code{pu}}{\code{\link[base:data.frame]{base::data.frame()}} planning unit data. Columns must be
"cost" (\code{numeric}), "area" (\code{numeric}), and "status"
(\code{integer}).}

\item{\code{species}}{\code{\link[base:data.frame]{base::data.frame()}} with species data. Columns must
be "name" (\code{character}.}

\item{\code{targets}}{\code{\link[base:data.frame]{base::data.frame()}} with species data. Columns must
be "species" (\code{integer}), "target" (\code{integer}), "proportion"
(\code{numeric}).}

\item{\code{pu.species.probabilities}}{\code{\link[base:data.frame]{base::data.frame()}} with data on
the probability of species in each planning unit. Columns must be "species"
(\code{integer}), "pu" (\code{integer}), and "value" (\code{numeric}).}

\item{\code{attribute.spaces}}{\code{list} of \code{AttributeSpaces} objects with
the demand points and planning unit coordinates.}

\item{\code{boundary}}{\code{\link[base:data.frame]{base::data.frame()}} with data on the shared
boundary length of planning units. Columns must be "id1" (\code{integer}),
"id2" (\code{integer}), and "boundary" (\code{numeric}).}

\item{\code{skipchecks}}{\code{logical} Skip data integrity checks? May improve
speed for big data sets.}

\item{\code{.cache}}{\code{\link[base:environment]{base::environment()}} used to cache calculations.}
}}

\seealso{
\code{\link[PBSmapping:PolySet]{PBSmapping::PolySet()}}.
}
