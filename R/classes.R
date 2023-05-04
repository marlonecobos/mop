#' Constructor of S3 objects of class mop_results
#'
#' @name mop_results
#'
#' @usage
#' new_mop_results(summary = new("list"), mop_distances = NULL,
#'                 mop_basic = NULL, mop_simple = NULL,
#'                 mop_detailed = new("list"))
#'
#' @param summary a list with a summary of the data and parameters used in
#' analysis. Default = empty list.
#' @param mop_distances a `SpatRaster` or numeric vector of distances from the
#' set of conditions of reference to the set of conditions of interest.
#' Default = NULL.
#' @param mop_basic a `SpatRaster` or numeric vector showing conditions in the set
#' of interest outside the ranges in the reference set. The value \code{1}
#' indicates conditions outside one or more ranges. Default = NULL.
#' @param mop_simple a `SpatRaster` or numeric vector showing conditions in the
#' set of interest outside the ranges in the reference set. Values indicate how
#' many variables are outside reference ranges. Default = NULL.
#' @param mop_detailed a list with a detailed representation of mop results
#' in conditions outside the range of reference. Default = empty list.
#'
#' @export
#'
#' @importFrom methods new
#'
#' @return
#' An object of class \code{mop_results}.

new_mop_results <- function(summary = new("list"), mop_distances = NULL,
                            mop_basic = NULL, mop_simple = NULL,
                            mop_detailed = new("list")) {
  sclass <- class(summary)[1]
  mdisclass <- class(mop_distances)[1]
  mbasclass <- class(mop_basic)[1]
  msimclass <- class(mop_simple)[1]
  mdetclass <- class(mop_detailed)[1]

  if (!sclass %in% c("list")) {
    stop("'summary' must be of class 'list'.")
  }
  if (!mdisclass %in% c("SpatRaster", "numeric", "integer", "NULL")) {
    stop("'mop_distances' must be a 'SpatRaster', 'numeric' vector, or NULL.")
  }
  if (!mbasclass %in% c("SpatRaster", "numeric", "integer", "NULL")) {
    stop("'mop_basic' must be a 'SpatRaster', 'numeric' vector, or NULL.")
  }
  if (!msimclass %in% c("SpatRaster", "numeric", "integer", "NULL")) {
    stop("'mop_simple' must be a 'SpatRaster', 'numeric' vector, or NULL.")
  }
  if (!mdetclass %in% c("list")) {
    stop("'mop_detailed' must be of class 'list'.")
  }

  val <- list(summary = summary, mop_distances = mop_distances,
              mop_basic = mop_basic, mop_simple = mop_simple,
              mop_detailed = mop_detailed)
  class(val) <- "mop_results"
  return(val)
}
