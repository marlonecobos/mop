% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mop_doc.R
\docType{package}
\name{mop-package}
\alias{mop-package}
\title{mop: Mobility Oriented-Parity Metric}
\description{
\code{mop} contains a set of tools to calculate the Mobility Oriented-Parity
metric, which allows a user to compare a set of conditions of reference
versus another set of of interest.
}
\details{
The main goals of the MOP metric are to explore conditions in the set of
interest that are non-analogous to those in the reference set, and to
quantify how different conditions in the set of interest are from the
reference set. The tools included here help to identify conditions outside
the ranges of the reference set with greater detail than in other
implementations. These tools are based on the methods proposed by
Owens et al. (2013; \url{doi:10.1016/j.ecolmodel.2013.04.011}).
}
\section{Functions in mop}{

\code{\link{mop}}, \code{\link{mop_distance}}, \code{\link{out_range}},
\code{\link{match_na_raster}}
}

\section{Data included}{

\code{\link{reference_matrix}}, \code{\link{matrix_of_interest}},
\code{\link{reference_layers}}, \code{\link{layers_of_interest}}
}

\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/marlonecobos/mop}
  \item Report bugs at \url{https://github.com/marlonecobos/mop/issues}
}

}
\author{
\strong{Maintainer}: Marlon E. Cobos \email{manubio13@gmail.com} (\href{https://orcid.org/0000-0002-2611-1767}{ORCID})

Authors:
\itemize{
  \item Hannah L. Owens \email{hannah.owens@gmail.com} (\href{https://orcid.org/0000-0003-0071-1745}{ORCID})
  \item Jorge Soberón \email{jsoberon@ku.edu} (\href{https://orcid.org/0000-0003-2160-4148}{ORCID})
  \item A. Townsend Peterson \email{town@ku.edu} (\href{https://orcid.org/0000-0003-0243-2379}{ORCID})
}

}
