#' Example of matrix with variables in a set of reference
#'
#' A numeric table representing variables in a set of reference.
#'
#' @format A matrix with 723 rows and 6 columns.
#'
#' @examples
#' data("reference_matrix", package = "mop")
#'
#' head(reference_matrix)
"reference_matrix"



#' Example of matrix with variables in a set of interest
#'
#' A numeric table representing variables in a set of interest.
#'
#' @format A matrix with 723 rows and 6 columns.
#'
#' @examples
#' data("matrix_of_interest", package = "mop")
#'
#' head(matrix_of_interest)
"matrix_of_interest"



#' Example of variables for a set of reference
#'
#' A `SpatRaster` object representing variables in a set of reference. Variables
#' represent current bioclimatic variables downloaded from the WorldClim
#' database (\url{https://worldclim.org/}).
#'
#' @format A `SpatRaster` object.
#'
#' @name reference_layers
#'
#' @return No return value. Used with function \code{\link[terra]{rast}} to
#' bring raster variables to analysis.
#'
#' @examples
#' reference_layers <- terra::rast(system.file("extdata", "reference_layers.tif",
#'                                             package = "mop"))
#'
#' terra::plot(reference_layers)
NULL



#' Example of variables for a set of interest
#'
#' A `SpatRaster` object representing variables in a set of interest. Variables
#' represent future bioclimatic variables downloaded from the WorldClim database
#' (\url{https://worldclim.org/}).
#'
#' @format A `SpatRaster` object.
#'
#' @name layers_of_interest
#'
#' @return No return value. Used with function \code{\link[terra]{rast}} to
#' bring raster variables to analysis.
#'
#' @examples
#' layers_of_interest <- terra::rast(system.file("extdata",
#'                                               "layers_of_interest.tif",
#'                                               package = "mop"))
#'
#' terra::plot(layers_of_interest)
NULL
