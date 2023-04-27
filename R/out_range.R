#' Values outside ranges of reference conditions
#'
#' @description
#' Options to identify which values in a set of conditions of interest
#' (\code{g_matrix}) are outside the range of a set of conditions of
#' reference (\code{m_matrix}).
#'
#' @usage
#' out_range(m_matrix, g_matrix, type = "basic")
#'
#' @param m_matrix matrix of variables representing the set of conditions to be
#' used as reference. Each column represents a variable.
#' @param g_matrix matrix of variables representing the set of conditions to be
#' compared against the reference conditions (where conditions outside range
#' are to be detected). Each column represents a variable. Variable names must
#' match those in \code{m_matrix}.
#' @param type (character) type of identification to be performed. Options
#' are: "basic", "simple", and "detailed".
#'
#' @export
#'
#' @importFrom utils combn


out_range <- function(m_matrix, g_matrix, type = "basic") {
  # initial tests
  if (missing(m_matrix) | missing(g_matrix)) {
    stop("Arguments 'm_matrix' and 'g_matrix' must be defined.")
  }

  clasm <- class(m_matrix)[1]
  clasg <- class(g_matrix)[1]

  if (clasm != "matrix" | clasg != "matrix") {
    stop("'m_matrix' and 'g_matrix' must be of class 'matrix'.")
  }

  type <- type[1]

  if (!type %in% c("basic", "simple", "detailed")) {
    stop("Option for 'type' is not valid.")
  }

  # variable names and number
  var_names <- colnames(m_matrix)
  nvar <- length(var_names)

  # defining range of what is inside M realms
  m_range <- apply(m_matrix, 2, range)

  # what is out basic and simple
  ## basic
  out <- sapply(1:nvar, function(x) {
    gx <- g_matrix[, x]
    gx < m_range[1, x] | gx > m_range[2, x]
  })

  out <- rowSums(out) # to become basic later

  ## simple
  if (type %in% c("simple", "detailed")) {
    out1 <- out
    out1[out1 == 0] <- NA
  } else {
    out1 <- NULL
  }


  # what is out in more detail
  if (type == "detailed") {
    mul <- 10^(1:nvar)

    ### which lower end
    outl <- sapply(1:nvar, function(x) {
      (g_matrix[, x] < m_range[1, x]) * mul[x]
    })
    colnames(outl) <- var_names

    outl1 <- rowSums(outl)
    outl[outl > 0] <- 1

    ## which high end
    outh <- sapply(1:nvar, function(x) {
      (g_matrix[, x] > m_range[2, x]) * mul[x]
    })
    colnames(outh) <- var_names

    outh1 <- rowSums(outh)
    outh[outh > 0] <- 1

    ## return to NA
    outh1[outh1 == 0] <- NA
    outl1[outl1 == 0] <- NA
    outh[outh == 0] <- NA
    outl[outl == 0] <- NA

    ## interpretation table
    inter_table <- ext_interpretation(var_names, variable_codes = mul)
  } else {
    outh <- NULL
    outl <- NULL
    outh1 <- NULL
    outl1 <- NULL
    inter_table <- NULL
  }

  # simple to basic
  wout <- out > 0
  out <- (wout) * 1
  out[!wout] <- NA

  # returning results
  return(list(basic = out, simple = out1, high_all = outh, low_all = outl,
              high_combined = outh1, low_combined = outl1,
              interpretation = inter_table))
}



# helper function to create table for mop detailed interpretation
ext_interpretation <- function (variable_names, variable_codes) {
  var_comb <- lapply(1:length(variable_names), function(x) {
    apply(utils::combn(variable_names, m = x), 2, paste, collapse = ", ")
  })
  var_comb <- unlist(var_comb)

  var_cod <- lapply(1:length(variable_codes), function(x) {
    apply(utils::combn(variable_codes, m = x), 2, sum)
  })
  var_cod <- unlist(var_cod)

  return(data.frame(values = var_cod, extrapolation_variables = var_comb))
}
