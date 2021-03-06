% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{setup}
\alias{setup}
\title{A mandatory function call for the rest of this package to work.}
\usage{
setup(dir, states = NA, verbose = TRUE)
}
\arguments{
\item{dir}{A character vector identifying the root directory of where the G-NAF extract is located.}

\item{states}{A regular expression of which Australian State jurisdictions to import.}

\item{verbose}{A logical argument to determine if the function should be verbose or not. Default is \code{TRUE}.}
}
\value{
Returns \emph{\strong{UPDATE}}
}
\description{
The function identifies all relevant inputs and metadata for the importing of the G-NAF in to R.
}
\examples{
# Update
}
\author{
Kyle Haynes, \email{kyle@kylehaynes.com.au}.
}
