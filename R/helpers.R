#' Match NA cells for all layers in SpatRaster
#'
#' @description
#' Options to identify which values in a set of conditions of interest
#' (\code{g_matrix}) are outside the range of a set of conditions of
#' reference (\code{m_matrix}).
#'
#' @usage
#' match_na_raster(layers)
#'
#' @param layers a `SpatRaster` object containing two or more variables to be
#' matched.
#'
#' @return
#' A `SpatRaster` object with NA cells matching in all layers.
#'
#' @export
#'
#' @importFrom terra nlyr mask app
#'
#' @examples
#' # data
#' layers <- terra::rast(system.file("extdata", "reference_layers.tif",
#'                       package = "mop"))
#'
#' # add NA in some places
#' layers[20:24, 10:16][, 3] <- NA
#' terra::plot(layers)
#'
#' # match NAs
#' matched <- match_na_raster(layers)
#' terra::plot(matched)

match_na_raster <- function(layers) {
  if (missing(layers)) {
    stop("Argument 'layers' must be defined.")
  }
  if (class(layers)[1] != "SpatRaster") {
    stop("Argument 'layers' must of class 'SpatRaster'.")
  }

  # processing
  if (terra::nlyr(layers) <= 1) {
    message("'layers' has only one layer, processing is not required.")
  } else {
    msk <- terra::app(layers, sum)
    layers <- terra::mask(layers, msk)
  }

  # results
  return(layers)
}
