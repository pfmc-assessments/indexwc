#'
#'
#'
#'
#' @details
#' Add details
#'
#' @param area_name A character vector specifying which areas to calculate
#'   indices for. These should be names from [boundaries_data]. The default is
#'   `"Coastwide"`, which calculates only the Coastwide index
#' @param area_bound Prediction grid as modified by [filter_boundaries()] and
#'   `boundaries`.
#' @param full_pred Object of model predictions created by [stats::predict()].
#' @param full_grid Filtered grid created by [filter_grid()].
#' @param calculate_cog Logical. If `TRUE`, center of gravity estimates will also be
#'   calculated using [sdmTMB::get_cog()] for each area. Defaults to `FALSE`.
#'   Note that COG results are returned in long format with separate rows for
#'   the X (easting) and Y (northing) coordinate axes.
#' @param bias_correct Logical. If `TRUE` [sdmTMB::get_index()] and
#'   [sdmTMB::get_cog()] will use bias correction to account for the non-linear
#'   transformation of random effects when calculating the index. Will be faster
#'   if set to `FALSE`, but is `TRUE` by default
#' @export
#' @author Eric Ward and Chantel Wetzel
#'
#'
get_area_results <- function(
  area_name,
  area_bounds,
  full_pred,
  full_grid,
  calculate_cog = FALSE,
  bias_correct = TRUE
) {
  in_region <- full_grid$latitude >= area_bounds["lower"] &
    full_grid$latitude <= area_bounds["upper"]

  area_weights <- ifelse(
    test = in_region,
    yes = full_grid$area_km2_WCGBTS,
    no = 0
  )

  index <- sdmTMB::get_index(
    obj = full_pred,
    bias_correct = bias_correct,
    area = area_weights
  )

  result <- list(
    prediction = full_pred,
    index = index
  )

  if (calculate_cog) {
    result[["cog"]] <- sdmTMB::get_cog(
      obj = full_pred,
      bias_correct = bias_correct,
      area = area_weights
    )
  }

  return(result)
}
