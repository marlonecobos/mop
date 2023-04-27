#' MOP distance calculation
#'
#' @description
#' Calculates distances from each of the poins of interest in g_matrix to a
#' defined percetage of the reference conditions in m_matrix.
#'
#' @usage
#' mop_distance(m_matrix, g_matrix, distance = "euclidean", percent = 1,
#'              comp_each = 2000, parallel = FALSE, n_cores = NULL,
#'              progress_bar = TRUE)
#'
#' @param m_matrix matrix of variables representing the original area of
#' interest. Each column represents a variable.
#' @param g_matrix matrix of variables representing the area of model projection
#' (where dissimilarities and/or non-analogous conditions are to be detected).
#' Each column represents a variable. Variable names must match those in
#' \code{m_matrix}
#' @param distance (character) one of the options: "euclidean" or "mahalanobis".
#' Default = "euclidean".
#' @param percent (numeric) percentage of points of m (the closest ones) used to
#' derive mean environmental distances to each g point. Default = 1.
#' @param comp_each (numeric) number of points of the g matrix to be used for
#' distance calculations at a time (default = 2000). Increasing this number
#' requires more RAM.
#' @param parallel (logical) if TRUE, calculations will be performed in parallel
#' using \code{n_cores} of the computer. Using this option will speed up the
#' analysis  but will demand more RAM. Default = FALSE.
#' @param n_cores (numeric) number of cores to be used in parallel processing.
#' Default = NULL, in which case all CPU cores on current host - 1 will be used.
#' @param progress_bar (logical) whether to show a progress bar for calculations,
#' default = TRUE. Valid when calculation are not run in parallel.
#'
#' @export
#'
#' @importFrom fields rdist
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom stats mahalanobis
#' @importFrom parallel detectCores
#' @importFrom snow makeSOCKcluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach `%dopar%` foreach

mop_distance <- function(m_matrix, g_matrix, distance = "euclidean", percent = 1,
                         comp_each = 2000, parallel = FALSE, n_cores = NULL,
                         progress_bar = TRUE) {
  # initial tests
  if (missing(m_matrix) | missing(g_matrix)) {
    stop("Arguments 'm_matrix' and 'g_matrix' must be defined.")
  }

  clasm <- class(m_matrix)[1]
  clasg <- class(g_matrix)[1]

  if (clasm != "matrix" | clasg != "matrix") {
    stop("'m_matrix' and 'g_matrix' must be of class 'matrix'.")
  }

  if (!distance %in% c("euclidean", "mahalanobis")) {
    stop("'distance' must be 'euclidean' or 'mahalanobis'.")
  }

  if (percent <= 0 | percent > 100) {
    stop("'percent' connot be <= 0 or > 100.")
  }

  # preparing groups of points
  nred <- nrow(g_matrix)
  per_out <- round(nrow(m_matrix) * (percent / 100))

  if (nred <= comp_each) {
    comp_each <- ceiling(nred / 3)
  }

  groups <- c(seq(1, nred, comp_each),  nred + 1)
  nprocess <- length(groups) - 1

  # running analysis
  if (parallel == FALSE) {
    ## progress bar preparation
    if (progress_bar == TRUE) {
      pb <- utils::txtProgressBar(min = 1, max = nprocess, style = 3)
    }

    ## distance calculation in loop
    mop1 <- lapply(1:nprocess, function(x) {
      if (progress_bar == TRUE) {
        Sys.sleep(0.1)
        utils::setTxtProgressBar(pb, x)
      }

      ## defining sets and all distances
      seq_rdist <- groups[x]:(groups[x + 1] - 1)

      if (distance == "euclidean") {
        cdist <- fields::rdist(g_matrix[seq_rdist, ], m_matrix)
      } else {
        cv <- cov(g_matrix)
        cdist <- lapply(seq_rdist, function(y) {
          stats::mahalanobis(x = m_matrix, center = g_matrix[y, ], cov = cv)
        })
        cdist <- do.call(rbind, cdist)
      }

      ## mean of closer distances
      apply(cdist, 1, function(y) {mean(sort(y)[1:per_out])})
    })

    if (progress_bar == TRUE) {
      close(pb)
    }
    mop1 <- unlist(mop1)

  } else {
    ## parallel processing preparation
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }

    cl <- snow::makeSOCKcluster(n_cores)
    doSNOW::registerDoSNOW(cl)

    ## progress bar preparation
    if (progress_bar == TRUE) {
      pb <- utils::txtProgressBar(min = 1, max = nprocess, style = 3)
      progress <- function(n) {
        utils::setTxtProgressBar(pb, n)
      }
      opts <- list(progress = progress)
    } else {
      opts <- NULL
    }

    ## parallel running
    mop1 <- foreach::foreach(
      i = 1:nprocess, .inorder = TRUE, .options.snow = opts, .combine = "c"
    ) %dopar% {

      ## defining sets and all distances
      seq_rdist <- groups[i]:(groups[i + 1] - 1)

      if (distance == "euclidean") {
        cdist <- fields::rdist(g_matrix[seq_rdist, ], m_matrix)
      } else {
        cv <- cov(g_matrix)
        cdist <- lapply(seq_rdist, function(y) {
          stats::mahalanobis(x = m_matrix, center = g_matrix[y, ], cov = cv)
        })
        cdist <- do.call(rbind, cdist)
      }

      ## getting mean of closer distances
      return(apply(cdist, 1, function(y) {mean(sort(y)[1:per_out])}))
    }

    snow::stopCluster(cl)
  }

  return(mop1)
}
