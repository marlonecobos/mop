#' Analysis of extrapolation risks using the MOP metric
#'
#' @description
#' Analysis to calculate the mobility-oriented parity metric and other
#' sub-products to represent dissimilarities and non-analogous conditions
#' when comparing a set of reference conditions (M; \code{m}) against another
#' set of conditions of interest (G; \code{g}).
#'
#' @usage
#' mop(m, g, type = "basic",  calculate_distance = FALSE,
#'     where_distance = "in_range", distance = "euclidean",
#'     scale = FALSE, center = FALSE, fix_NA = TRUE, percentage = 1,
#'     comp_each = 2000, tol = NULL, rescale_distance = FALSE,
#'     parallel = FALSE, n_cores = NULL, progress_bar = TRUE)
#'
#' @param m a `SpatRaster` or matrix of variables representing a set of
#' conditions of reference (e.g., the set of conditions in which a model was
#' calibrated). If a matrix is used, each column represents a variable.
#' @param g a `SpatRaster` or matrix of variables representing a set of
#' conditions of interest for which dissimilarity values and non-analogous
#' conditions will be detected (e.g., conditions in which a model is projected).
#' Variable names must match between \code{m} and \code{g}.
#' @param type `character`, type of MOP analysis to be performed. See `Details`
#' for options.
#' @param calculate_distance `logical`, whether to calculate distances
#' (dissimilarities) between \code{m} and \code{g}. The default, FALSE, runs
#' rapidly and does not assess dissimilarity levels.
#' @param where_distance `character`, where to calculate distances, considering
#' how conditions in \code{g} are positioned in comparison to the range of
#' conditions in \code{m}. See `Details` for options.
#' @param distance `character`, which distances are calculated, `euclidean` or
#' `mahalanobis`. Valid if `calculate_distance = TRUE`.
#' @param scale scaling options, `logical` or `numeric-alike` as in
#' \code{\link[base]{scale}}.
#' @param center `logical` or `numeric-alike` center options as in
#' \code{\link[base]{scale}}.
#' @param fix_NA `logical`, whether to fix layers so cells with NA values
#' are the same in all layers. Setting to FALSE may save time if the
#' rasters are big and have no NA matching problems.
#' @param percentage `numeric`, percentage of \code{m} closest conditions used
#' to derive mean environmental distances to each combination of conditions in
#' \code{g}.
#' @param comp_each `numeric`, number of combinations in \code{g} to be used for
#' distance calculations at a time. Increasing this number requires more RAM.
#' @param tol tolerance to detect linear dependencies when calculating
#' Mahalanobis distances. The default, NULL, uses `.Machine$double.eps`.
#' @param rescale_distance `logical`, whether or not to re-scale distances 0-1.
#' Re-scaling prevents comparisons of dissimilarity values obtained from runs
#' with different values of \code{percentage}.
#' @param parallel `logical`, whether calculations should be performed in
#' parallel using \code{n_cores} of the computer. Using this option will speed
#' up the analysis but will demand more RAM.
#' @param n_cores `numeric`, number of cores to be used in parallel processing.
#' If `parallel = TRUE` and `n_cores = NULL` (all CPU cores on current host - 1)
#' will be used.
#' @param progress_bar `logical`, whether to show a progress bar.
#'
#' @details
#' `type` options return results that differ in the detail of how non-analogous
#' conditions are identified.
#' - **basic** - makes calculation as proposed by Owens et al. (2013)
#' <doi:10.1016/j.ecolmodel.2013.04.011>.
#' - **simple** - calculates how many variables in the set of interest are
#' non-analogous to those in the reference set.
#' - **detailed** - calculates five additional extrapolation metrics. See
#' `mop_detailed` under `Value` below for full details.
#'
#' `where_distance` options determine what values should be used to calculate
#' dissimilarity
#' - **in_range** - only conditions inside `m` ranges
#' - **out_range** - only conditions outside `m` ranges
#' - **all** - all conditions
#'
#' When the variables used to represent conditions have different units,
#' scaling and centering are recommended. This step is only valid when Euclidean
#' distances are used.
#'
#' @return
#' A object of class \code{\link{mop_results}} containing:
#' - **summary** - a list with details of the data used in the analysis:
#'     - *variables* - names of variables considered.
#'     - *type* - type of MOP analysis performed.
#'     - *scale* - value according to the argument \code{scale}.
#'     - *center* - value according to the argument \code{center}.
#'     - *calculate_distance* - value according to the argument
#'     \code{calculate_distance}.
#'     - *distance* - option regarding distance used.
#'     - *percentage* - percentage of \code{m} used as reference for
#'     distance calculation.
#'     - *rescale_distance* - value according to the argument
#'     \code{rescale_distance}.
#'     - *fix_NA* - value according to the argument \code{fix_NA}.
#'     - *N_m* - total number of elements (cells with values or valid
#'     rows) in \code{m}.
#'     - *N_g* - total number of elements (cells with values or valid
#'     rows) in \code{g}.
#'     - *m_ranges* - matrix with ranges of variables in reference conditions
#'     (\code{m}).
#' - **mop_distances** - if \code{calculate_distance} = TRUE, a SpatRaster or
#' vector with distance values for the set of interest (\code{g}). Higher values
#' represent greater dissimilarity compared to the set of reference (\code{m}).
#' - **mop_basic** - a SpatRaster or vector, for the set of interest,
#' representing conditions in which at least one of the variables is
#' non-analogous to the set of reference. Values should be: 1 for non-analogous
#' conditions, and NA for conditions inside the ranges of the reference set.
#' - **mop_simple** - a SpatRaster or vector, for the set of interest,
#' representing how many variables in the set of interest are non-analogous to
#' those in the reference set. NA is used for conditions inside the ranges of
#' the reference set.
#' - **mop_detailed** - a list containing:
#'     - *interpretation_combined* - a data.frame to help identify combinations
#'     of variables in *towards_low_combined* and *towards_high_combined* that
#'     are non-analogous to \code{m}.
#'     - *towards_low_end* - a SpatRaster or matrix for all variables
#'     representing where non-analogous conditions were found towards low values
#'     of each variable.
#'     - *towards_high_end* - a SpatRaster or matrix for all variables
#'     representing where non-analogous conditions were found towards high
#'     values of each variable.
#'     - *towards_low_combined* - a SpatRaster or vector with values
#'     representing the identity of the variables found to have non-analogous
#'     conditions towards low values. If vector, interpretation requires the use
#'     of the data.frame *interpretation_combined*.
#'     - *towards_high_combined* - a SpatRaster or vector with values
#'     representing the identity of the variables found to have non-analogous
#'     conditions towards high values. If vector, interpretation requires the
#'     use of the data.frame *interpretation_combined*.
#'
#' @export
#'
#' @importFrom terra rast
#' @importFrom stats na.omit
#'
#' @seealso
#' \code{\link{mop_distance}}, \code{\link{out_range}}
#'
#' @examples
#' # data
#' reference_layers <- terra::rast(system.file("extdata", "reference_layers.tif",
#'                                             package = "mop"))
#'
#' layers_of_interest <- terra::rast(system.file("extdata",
#'                                               "layers_of_interest.tif",
#'                                               package = "mop"))
#'
#' # analysis
#' mop_res <- mop(m = reference_layers, g = layers_of_interest)
#'
#' summary(mop_res)

mop <- function(m, g, type = "basic", calculate_distance = FALSE,
                where_distance = "in_range", distance = "euclidean",
                scale = FALSE, center = FALSE, fix_NA = TRUE, percentage = 1,
                comp_each = 2000, tol = NULL, rescale_distance = FALSE,
                parallel = FALSE, n_cores = NULL, progress_bar = TRUE) {

  # initial tests
  if (missing(m) | missing(g)) {
    stop("Arguments 'm' and 'g' must be defined.")
  }

  clasm <- class(m)[1]
  clasg <- class(g)[1]

  if (!clasm %in% c("SpatRaster", "matrix", "data.frame")) {
    stop("'m' must be a 'SpatRaster', 'matrix', or 'data.frame'.")
  } else {
    if (clasm == "SpatRaster") {
      var_names <- names(m)
    } else {
      var_names <- colnames(m)
    }
  }
  if (!clasg %in% c("SpatRaster", "matrix", "data.frame")) {
    stop("'g' must be a 'SpatRaster', 'matrix', or 'data.frame'.")
  } else {
    if (clasg == "SpatRaster") {
      gnames <- names(g)
    } else {
      gnames <- colnames(g)
    }
  }

  if (!identical(var_names, gnames)) {
    stop("Variables in 'm' and 'g' must be named identically.")
  }

  type <- type[1]


  # preprocessing
  ## layer for mop results
  if (clasg == "SpatRaster") {
    ### fix NA mismatches across layers
    if (fix_NA == TRUE) {
      g <- match_na_raster(layers = g)
    }

    ### layer
    mop <- g[[1]]
    names(mop) <- "mop"
  }

  ## getting values
  m <- m[]
  g <- g[]

  ## variables and test
  nvar <- ncol(m)

  ## exclude NAs and number of cells
  m <- as.matrix(na.omit(m))
  g <- as.matrix(na.omit(g))

  nm <- nrow(m)
  ng <- nrow(g)

  if (clasg %in% "matrix") {
    mop <- rep(NA, ng)
  }

  ## scaling options
  if (distance == "euclidean") {
    if (scale == TRUE | center == TRUE) {
      m <- scale(rbind(m, g), center = center, scale = scale)
      g <- m[(nm + 1):nrow(m), ]
      m <- m[1:nm, ]
    }
  }


  # identifying dimensions where g is out of m box
  out_ranges <- out_range(m_matrix = m, g_matrix = g, type = type)


  # distance calculation
  ## relevant points
  if (where_distance == "in_range") {
    reduced <- is.na(out_ranges$basic)
  }
  if (where_distance == "out_range") {
    reduced <- !is.na(out_ranges$basic)
  }
  if (where_distance == "all") {
    reduced <- rep(TRUE, ng)
  }

  ## analysis
  if (calculate_distance) {
    mop1 <- mop_distance(m, g[reduced, ], distance, percentage, comp_each, tol,
                         parallel, n_cores, progress_bar)
  }


  # re-assigning values to rasters
  ## na values in mop layer
  if (clasg == "SpatRaster") {
    nona <- which(!is.na(mop[]))
  } else {
    nona <- rep(TRUE, ng)
  }

  ## simple results
  if (type != "basic") {
    mop2 <- mop
    mop2[nona] <- out_ranges$simple

    if (clasg == "SpatRaster") {
      valss <- terra::unique(mop2)[, 1]
      if (is.null(valss)) {
        cates <- data.frame(id = numeric(), category = character())
      } else {
        cates <- data.frame(id = valss, category = as.character(valss))
      }
      levels(mop2) <- cates
    }
  } else {
    mop2 <- NULL
  }

  ## detailed results
  if (type == "detailed") {
    mop3 <- mop
    mop4 <- mop

    ### results for combined variables
    mop3[nona] <- out_ranges$low_combined
    mop4[nona] <- out_ranges$high_combined

    if (clasg == "SpatRaster") {
      cates <- out_ranges$interpretation
      cates_hc <- cates[cates$values %in% terra::unique(mop4)[, 1], ]
      colnames(cates_hc) <- c("id", "category")
      levels(mop4) <- cates_hc

      cates_lc <- cates[cates$values %in% terra::unique(mop3)[, 1], ]
      colnames(cates_lc) <- c("id", "category")
      levels(mop3) <- cates_lc
    }

    ### results for independent variables
    if (clasg == "SpatRaster") {
      #### low
      mop5 <- terra::rast(lapply(1:nvar, function(x) {
        mop0 <- mop
        mop0[nona] <- out_ranges$low_all[, x]
        mop0
      }))
      names(mop5) <- var_names

      #### high
      mop6 <- terra::rast(lapply(1:nvar, function(x) {
        mop0 <- mop
        mop0[nona] <- out_ranges$high_all[, x]
        mop0
      }))
      names(mop6) <- var_names
    } else {
      mop5 <- out_ranges$low_all
      mop6 <- out_ranges$high_all
    }
  } else {
    mop3 <- NULL
    mop4 <- NULL
    mop5 <- NULL
    mop6 <- NULL
  }

  ## MOP results
  mop[nona] <- out_ranges$basic

  ## Distance results
  if (calculate_distance) {
    ### re-scaling if needed
    if (rescale_distance == TRUE) {
      minmop <- min(mop1)
      out_ranges$basic[reduced] <- (mop1 - minmop) / (max(mop1) - minmop)
      out_ranges$basic[!reduced] <- NA
    } else {
      out_ranges$basic[reduced] <- mop1
      out_ranges$basic[!reduced] <- NA
    }

    mop1 <- mop
    mop1[nona] <- out_ranges$basic
  } else {
    mop1 <- NULL
  }


  # returning results
  results <- new_mop_results(
    summary = list(variables = var_names, type = type, scale = scale,
                   center = center, calculate_distance = calculate_distance,
                   distance = distance, percentage = percentage,
                   rescale_distance = rescale_distance,
                   fix_NA = fix_NA, N_m = nm, N_g = ng,
                   m_ranges = out_ranges$m_ranges),
    mop_distances = mop1, mop_basic = mop, mop_simple = mop2,
    mop_detailed = list(
      interpretation_combined = out_ranges$interpretation,
      towards_low_end = mop5, towards_high_end = mop6,
      towards_low_combined = mop3, towards_high_combined = mop4
    )
  )

  return(results)
}
