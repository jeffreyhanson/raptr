#' @include raptr-internal.R
NULL

#' Deprecation notice
#'
#' The functions listed here are deprecated.
#' This means that they once existed in earlier versions of the
#' of the \pkg{raptr} package, but they have since been removed
#' entirely, replaced by other functions, or renamed as other functions
#' in newer versions.
#' To help make it easier to transition to new versions of the \pkg{raptr}
#' package, we have listed alternatives for deprecated the functions
#' (where applicable).
#' If a function is described as being renamed, then this means
#' that only the name of the function has changed
#' (i.e., the inputs, outputs, and underlying code remain the same).
#'
#' @param ... not used.
#'
#' @details
#' The following functions have been deprecated:
#'
#' \describe{
#'
#' \item{`SpatialPolygons2PolySet()`}{renamed
#'   as the [convert2PolySet()] function.}
#' }
#'
#'
#' @keywords deprecated
#'
#' @name raptr-deprecated
NULL

#' @rdname raptr-deprecated
#' @export
SpatialPolygons2PolySet <- function(...) {
  .Defunct(
    msg = "SpatialPolygons2PolySet() has been renamed to convert2PolySet()"
  )
}
