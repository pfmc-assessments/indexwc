% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_index_areas.R
\name{calc_index_areas}
\alias{calc_index_areas}
\title{Calculate area-specific indices between each set of boundaries}
\usage{
calc_index_areas(
  data,
  fit,
  prediction_grid,
  dir,
  boundaries = boundaries_data["Coastwide"]
)
}
\arguments{
\item{data}{A data frame containing tow-level information on catch weight
and effort. Additional columns can be present such as depth.}

\item{fit}{List created by the sdmTMB:: fit function.}

\item{prediction_grid}{The prediction grid for the survey that sdmTMB will
use to make model predictions to.}

\item{dir}{A directory location where results and figures will be saved.}

\item{boundaries}{A named list of northern and southern boundaries for a
set of areas. The list can contain as many areas as you would like but
it must contain at least one area and each area must be a vector of two
real numbers specified in decimal degrees. The order of the areas only
matters if you care what order they are plotted because the names will
be turned into factors. The default value uses a data object called
\code{boundaries_data}, which is a list of several areas along the U.S. West
Coast, including a coastwide area going from the northern Washington
border to the southern California border.}
}
\value{
A very large list is returned with predictions, model fits, and indices for
each area specified in boundaries. Potentially, two figures are also saved
to the disk displaying the index by year. The index for each area is also
saved to a csv file in \code{dir} titled \code{est_by_area.csv}.
}
\description{
Calculate annual estimates of biomass (mt) for each area specified in
\code{boundaries}. The boundaries can be any configuration as long as all
boundaries lie within the overall grid; they can even be overlapping.
The more boundaries there are the longer the function will take to run.
}
\details{
The outer areas will be truncated based on the presence of positive tows
such that the most northern and southern extents of adjoining areas will
not extrapolate into areas where positive tows are not observed at least
once. This is helpful for species that are sparsely sampled because model
convergence is poor when there are a lot of zeros.
}
\examples{
\dontrun{
# Read back in the saved object
load("sdmTMB_save.RData")
# pick which boundary you want to use, e.g., 4 for California,
boundaries_data[4]
# or you can make your own named list, e.g.,
list("test" = c(39, 38))
# run the function
index <- calc_index_areas(
  data, fit, grid,
  dir = getwd(), boundaries = list("test" = c(39, 38))
)
# look at the index, which will be in "test" because that is what we
# named the boundary
index[["test"]][["index"]]
}

}
\seealso{
\itemize{
\item \link{boundaries_data}, a data object
\item \code{\link[sdmTMB:get_index]{sdmTMB::get_index()}}, used to generate the area-specific indices
\item \code{\link[sdmTMB:sdmTMB]{sdmTMB::sdmTMB()}}, used to create the \code{fit} object
}
}
\author{
Kelli F. Johnson
}
