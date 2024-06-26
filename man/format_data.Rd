% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_data.R
\name{format_data}
\alias{format_data}
\title{Format \code{data} by standardizing column names and units}
\usage{
format_data(data, ...)
}
\arguments{
\item{data}{A data frame containing tow-level information on catch weight
and effort. Additional columns can be present such as depth.}

\item{...}{Not yet implemented for any of the available methods but present
for future development purposes.}
}
\value{
A data frame with all lower case column names.
\itemize{
\item \code{year}
\item \code{survey_name}
\item \code{common_name}
\item \code{catch_numbers}
\item \code{catch_wt} (mt)
\item \code{effort} ($km^2$)
\item \code{pass_scaled} after subtracting the mean
\item \code{vessel_year} pasted together, made numeric, and subtract 1
\item \code{fyear} the factor version of \code{year} to match notation used in {sdmTMB}
\item \code{longitude} (decimal degrees)
\item \code{latitude} (decimal degrees)
\item \code{x} and \code{y} are in Universal Transverse Mercator (UTM)
\item \code{depth} (m) with positive entries representing measurements above sea
level and negative entries representing measurements below sea level
\item \code{depth_scaled} depth (m) scaled using \code{\link[base:scale]{base::scale()}}; so, subtracting the
mean and dividing by the standard deviation, these values are saved in
attributes for this column so that you can reference them later
\item \code{depth_scaled_squared} is \code{depth_scaled^2}
}
}
\description{
Input data to \code{run(data = )} must be formatted in a certain way to allow
calculations such as the creation of the mesh to work. Therefore, all input
data frames must be formatted prior to using \code{\link[=run]{run()}}. This function will
work detect what kind of data you have and then format it appropriately to
work with all downstream functions within this package.
}
\examples{
catch_wcgbts_canary <- nwfscSurvey::pull_catch(
  common_name = "canary rockfish",
  years = 2021:2022,
  survey = "NWFSC.Combo"
)
# Must assign the data a class so {indexwc} knows how to format it
class(catch_wcgbts_canary) <- c("nwfscSurvey", class(catch_wcgbts_canary))
formatted_data <- format_data(data = catch_wcgbts_canary)
}
\author{
Chantel R. Wetzel and Kelli F. Johnson
}
