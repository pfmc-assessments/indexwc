% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_family.r
\name{format_family}
\alias{format_family}
\title{Format the name of a distribution}
\usage{
format_family(family, pretty = FALSE)
}
\arguments{
\item{family}{A distribution from {sdmTMB} such as \code{\link[sdmTMB:families]{sdmTMB::delta_gamma()}}.}

\item{pretty}{A logical specifying if you want the returned value to be
formatted for pretty printing, which is helpful when the text is going to
be placed in a report on a figure.}
}
\value{
A string.
}
\description{
Format \code{clean_name} which is an item in the list returned from a call to a
distribution, e.g., \code{sdmTMB::delta_gamma()[["clean_name"]]}.
}
\examples{
format_family(sdmTMB::delta_gamma())
format_family(sdmTMB::delta_gamma(), pretty = TRUE)
}
\author{
Kelli F. Johnson
}
