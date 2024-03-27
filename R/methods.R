#' Print a short version of elements in mop objects
#' @name print
#' @aliases print,mop_results
#' @param x object of class mop_results.
#' @param ... further arguments to be passed to or from other methods. Ignored
#' in this function.
#' @export
#' @importFrom terra freq minmax
#' @importFrom utils head
#' @return A short description of objects in the console.

print.mop_results <- function(x, ...) {
  if (missing(x)) {stop("Argument 'x' is missing.")}

  cat("MOP distances:\n")
  if (!is.null(x$mop_distances)) {
    if (class(x$mop_distances)[1] == "SpatRaster") {
      print(x$mop_distances)
    } else {
      print(head(x$mop_distances))
      cat("...\n")
    }
  } else {
    cat("Empty\n")
  }

  cat("\nMOP basic:\n")
  if (class(x$mop_basic)[1] == "SpatRaster") {
    print(x$mop_basic)
  } else {
    print(head(x$mop_basic))
    cat("...\n")
  }

  cat("\nMOP simple:\n")
  if (!is.null(x$mop_simple)) {
    if (class(x$mop_simple)[1] == "SpatRaster") {
      print(x$mop_simple)
    } else {
      print(head(x$mop_simple))
      cat("...\n")
    }
  } else {
    cat("Empty\n")
  }

  cat("\nMOP detailed:\n")
  if (!is.null(x$mop_detailed$interpretation_combined)) {
    cat("interpretation_combined:\n")
    print(head(x$mop_detailed$interpretation_combined))
    cat("...\n")
    nms <- names(x$mop_detailed)[-1]
    lapply(nms, function(y) {
      cat("\n", y, ":\n", sep = "")
      if (class(x$mop_detailed[[y]])[1] == "SpatRaster") {
        print(x$mop_detailed[[y]])
      } else {
        print(head(x$mop_detailed[[y]]))
        cat("...\n")
      }
    })
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

summary.mop_results <- function(object, ...) {
  if (missing(object)) {stop("Argument 'object' is missing")}

  cat("\n                        Summary of MOP resuls\n")
  cat("---------------------------------------------------------------------------\n\n")
  cat("MOP summary:\nValues\n")
  print(as.data.frame(object$summary[c(-1, -12)]))

  cat("\nReference conditions\n")
  print(object$summary$m_ranges)

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

  cat("\n\nNon-analogous conditions (NAC):\n")
  if (class(object$mop_basic)[1] == "SpatRaster") {
    frequ <- terra::freq(object$mop_basic, value = 1)[1, 3]
  } else {
    frequ <- sum(object$mop_basic == 1, na.rm = TRUE)
  }
  perc <- frequ / object$summary$N_g

  cat("Percentage = ", round(perc, 3), "% of all conditions\n", sep = "")

  if (!is.null(object$mop_simple)) {
    if (class(object$mop_basic)[1] == "SpatRaster") {
      maxi <- terra::minmax(object$mop_simple)[2, 1]
    } else {
      maxi <- max(object$mop_simpl, na.rm = TRUE)
    }

    cat("Variables with NAC in 'simple' = ", maxi, "\n", sep = "")
  }

  if (!is.null(object$mop_detailed$interpretation_combined)) {
    cat("\n\nDetailed results were obtained, not shown here\n")
  }
}
