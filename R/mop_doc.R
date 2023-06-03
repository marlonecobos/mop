#' mop: Mobility Oriented-Parity Metric
#'
#' @description
#' `mop` contains a set of tools to calculate the Mobility Oriented-Parity
#' metric, which allows a user to compare a set of conditions of reference
#' versus another set of of interest.
#'
#' @details
#' The main goals of the MOP metric are to explore conditions in the set of
#' interest that are non-analogous to those in the reference set, and to
#' quantify how different conditions in the set of interest are from the
#' reference set. The tools included here help to identify conditions outside
#' the ranges of the reference set with greater detail than in other
#' implementations. These tools are based on the methods proposed by
#' Owens et al. (2013; <doi:10.1016/j.ecolmodel.2013.04.011>).
#'
#' @section Functions in mop:
#' \code{\link{mop}}, \code{\link{mop_distance}}, \code{\link{out_range}},
#' \code{\link{match_na_raster}}
#'
#' @section Data included:
#' \code{\link{reference_matrix}}, \code{\link{matrix_of_interest}},
#' \code{\link{reference_layers}}, \code{\link{layers_of_interest}}
#'
#' @docType package
#' @name mop_package
NULL
