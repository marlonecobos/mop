#' Print a short version of elements in mop objects
#' @name print
#' @aliases print,mop_results
#' @param x object of class mop_results.
#' @param ... further arguments to be passed to or from other methods. Ignored
#' in this function.
#' @export
#' @importFrom terra freq minmax
#' @return A short description of objects in the console.

print.mop_results <- function(x, ...) {
  if (missing(x)) {stop("Argument 'x' is missing.")}

  cat("MOP distances:\n")
  if (!is.null(x$mop_distances)) {
    print(x$mop_distances)
  } else {
    cat("Empty\n")
  }

  cat("\nMOP basic:\n")
  print(x$mop_basic)

  cat("\nMOP simple:\n")
  if (!is.null(x$mop_simple)) {
    print(x$mop_simple)
  } else {
    cat("Empty\n")
  }

  cat("\nMOP detailed:\n")
  if (!is.null(x$mop_detailed)) {
    print(x$mop_detailed)
  } else {
    cat("Empty\n")
  }
}




#' Summary of attributes and results
#' @name summary
#' @aliases summary,mop_results
#' @param object object of class mop_results.
#' @param ... additional arguments affecting the summary produced. Ignored in
#' this function.
#' @export
#' @return A printed summary.

summary.master_matrix <- function(object, ...) {
  if (missing(object)) {stop("Argument 'object' is missing")}

  cat("\n                        Summary of MOP resuls\n")
  cat("---------------------------------------------------------------------------\n\n")
  cat("MOP summary:\nVariables\n")
  cat(paste(object$summary$variables, collapse = ", "), "\n")
  print(as.data.frame(object$summary[-1]))

  if (!is.null(object$mop_distances)) {
    cat("\n\nDistances:\n")
    if (class(object$mop_distances)[1] == "SpatRaster") {
      dists <- na.omit(object$mop_distances[][, 1])
    } else {
      dists <- na.omit(object$mop_distances)
    }
    mmx <- range(dists)
    print(c(min = mmx[1], mean = mean(dists), max = mmx[2]))
  } else {
    cat("\n\nDistances were not calculated\n")
  }

  cat("\n\nNon-analogous environments (NAE):\n")
  if (class(object$mop_basic)[1] == "SpatRaster") {
    frequ <- terra::freq(object$mop_basic, value = 1)[1, 3]
  } else {
    frequ <- sum(object$mop_basic == 1, na.rm = TRUE)
  }
  prec <- frequ / object$summary$N_g

  cat("Percentage = ", perc, "% of all contions\n")

  if (!is.null(object$mop_simple)) {
    if (class(object$mop_basic)[1] == "SpatRaster") {
      maxi <- terra::minmax(object$mop_simple)[2, 1]
    } else {
      maxi <- max(object$mop_simpl, na.rm = TRUE)
    }

    cat("Number of variables with NAE = ", maxi, "\n")
  }
}
