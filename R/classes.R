#' Constructor of S3 objects of class mop_results
#'
#' @name mop_results
#'
#' @param summary = new("list"),
#' @param mop_distances = NULL,
#' @param mop_basic = NULL
#' @param mop_simple = NULL,
#' @param mop_detailed = new("list")
#'
#' @export
#'
#' @return
#' An object of class \code{mop_results}.
#'
#' @usage
#' new_mop_results(summary = new("list"), mop_distances = NULL,
#'                 mop_basic = NULL, mop_simple = NULL,
#'                 mop_detailed = new("list"))

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
