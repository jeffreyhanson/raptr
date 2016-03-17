#' @include RcppExports.R rapr-internal.R generics.R misc.R SimplePoints.R DemandPoints.R
NULL

#' AttributeSpace: An S4 class to represent an attribute space.
#'
#' This class is used to store planning unit points and demand points in an attribute space.
#'
#' @slot pu \code{SimplePoints} coordinates for planning unit in the space.
#' @slot demand.points \code{list} of \code{DemandPoints} for each species.
#' @slot distance.metric \code{character} name of distance metric to use for comparing planning units to demand points.
#' @seealso \code{\link{SimplePoints-class}}, \code{\link{DemandPoints-class}}.
#' @export
setClass("AttributeSpace",
	representation(
		pu='SimplePoints',
		demand.points='list',
		distance.metric='character'
	),
	validity=function(object) {
		# check that demand points and pu all have same dimensions
		if (!all(laply(object@demand.points, function(x) {return(ncol(x@points@coords)==ncol(object@pu@coords))})))
			stop('planning units and/or demand points have different numbers of columns in the coords slot')
		# check distance.metric
		if (!is.character(object@distance.metric)) stop('argument to distance.metric is not character')
		match.arg(
			object@distance.metric,
			c('euclidean', 'bray', 'manhattan','gower',
				'canberra', 'mahalanobis',
				'jaccard', 'kulczynski'
		))
		return(TRUE)
	}
)

#' Create new AttributeSpace object
#'
#' This function creates a new \code{AttributeSpace} object.
#'
#' @param pu \code{SimplePoints} coordinates for planning unit in the space.
#' @param demand.points \code{list} of \code{DemandPoints} for each species.
#' @param distance.metric \code{character} name of distance metric to use for comparing planning units to demand points. Valid metrics are 'euclidean', 'bray', 'manhattan','gower', 'canberra', 'mahalanobis', 'jaccard', 'kulczynski', 'minkowski'. Defaults to 'euclidean'. See details for the equations used for each metric.
#' @details A variety of distance metrics are provided for comparing planning units and demand points in an attribute space. Euclidean distances are recomended for geographic attribute spaces, providing they have been projected to an equal distance coordinate system. Bray-Curtis, Gower (Gower 1971), and Kulczynski distance have been found to work well for environmental gradients (Faith \emph{et al.} 1987). The Jaccard distances are computed as \eqn{2B/(1+B)} where B is Bray-Curtis dissimilarity. Bray-Curtis distances are recomended for binary data because they devolve to Sorenson distances (Sorenson 1957). The naming convention for distance metrics used here follows that for the \code{vegdist} function in the R package \pkg{vegan}. Minkowski distances are recomended for high-dimensional data.
#' The distance \eqn{d_{ij}} between demand point \eqn{i \in I} and planning unit \eqn{j \in J}, given an attribute space with \eqn{K} dimensions (indexed using \eqn{k}).
#' \tabular{cl}{
#' \code{euclidean}
#' \tab \eqn{d_{ij} = \sqrt{sum_{k=1}^{K} (x_{ik} - x_{jk})^2}} \cr
#' \code{bray}
#' \tab \eqn{d_{ij} = \frac{\sum_{k=1}^{K} |x_{ik} -x_{jk} |}{\sum_{k=1}^{K} (x_{ik} + x_{jk})}} \cr
#' \code{manhattan}
#' \tab \eqn{d_{ij} = \frac{\sum_{k=1}^{K}|x_{ik}-x_{jk}|}{\sum_{k=0}^{K} (x_{ik}+x_{jk})}} \cr
#' \code{gower}
#' \tab \eqn{d_{ij} = (1/M) \sum_{k=1}^{K} \frac{|x_{ik}-x_{jk}|}{\max (x_{.,k}) - \min (x_{.,k})}} \cr
#' \tab where \eqn{M} is the number of columns (excluding missing values). \cr
#' \code{canberra}
#' \tab \eqn{d_{ij} = \sum_{k=0}^{K} \frac{|x_{ik}-x_{jk}|}{|x_{ik}|+|x_{jk}|}} \cr
#' \tab where \eqn{NZ} is the number of non-zero entries. \cr
#' \code{mahalanobis}
#' \tab \eqn{d_{ij} = \sqrt{(s_{i,.} - x{j,.})^T Q^{-1} (s_{i,.} - x{j,.})}} \cr
#' \tab where: \cr
#' \tab \eqn{s} = x centered.\cr
#' \tab \eqn{s_{ik} = x_{ik} - mean(x_{.,k})} \cr
#' \tab \eqn{Q} = covariance matrix planning units and demand points in attribute space.\cr
#' \code{jaccard}
#' \tab \eqn{d_{ij} = 2B_{ij}/(1+B_{ij})} \cr 
#' \tab where \eqn{B_{ij}} is the Bray-Curtis dissimilarity between \eqn{i} and \eqn{j}. \cr
#' \code{kulczynski}
#' \tab \eqn{d_{ij} =  1-0.5(\frac{\sum_{k=1}^{K} \min(x_{ik},x_{jk})}{\sum_{k=1}^{K} x_{ik}} + \frac{\sum_{k=1}^{K} \min(x_{ik},x_{jk})}{\sum_{k=1}^{K} x_{jk}} )} \cr
#' \code{minkowski}
#' \tab \eqn{d_{ij} =  (\sum_{k=1}^{k}(|x_{ik}-x_{jk}|)^k)^(1/k)} \cr
#' }
#' @seealso \code{\link{SimplePoints-class}}, \code{\link{DemandPoints-class}}.
#' @references Faith, D. P, Minchin, P. R. and Belbin, L. (1987). Compositional dissimilarity as a robust measure of ecological distance. \emph{Vegetatio} 69, 57--68.
#' Gower, J. C. (1971). A general coefficient of similarity and some of its properties. \emph{Biometrics} 27, 623--637.
#' Sorensen, T. (1948) A method of establishing groups of equal amplitude in plant sociology based on similarity of species and its application to analyses of the vegetation on Danish commons. \emph{Biologiske Skrifter / Kongelige Danske Videnskabernes Selskab} 5, 1--34.
#' @export
#' @examples
#' space <- AttributeSpace(
#'	pu <- SimplePoints(
#'		matrix(rnorm(100), ncol=2)
#'	),
#'	demand.points <- list(
#'	DemandPoints(
#'			SimplePoints(matrix(rnorm(100), ncol=2)),
#'			runif(50)
#'		)
#'	),
#'		distance.metric='euclidean'
#' )
AttributeSpace<-function(pu, demand.points, distance.metric='euclidean') {
	as<-new("AttributeSpace", pu=pu, demand.points=demand.points, distance.metric=distance.metric)
	validObject(as, test=FALSE)
	return(as)
}

