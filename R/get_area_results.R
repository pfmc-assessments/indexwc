#'
#'
#'
#'
#' @details
#' Add details
#'
#' @param area_name Describe
#' @param area_bound Describe
#' @param full_pred Describe
#' @param full_grid Describt
#' @param calculate_cog Describe
#' @export
#' @author Eric Ward and Chantel Wetzel
#'
#'
get_area_results <- function(
  area_name,
  area_bounds,
  full_pred,
  full_grid,
  calculate_cog = FALSE
) {
  in_region <- full_grid$latitude >= area_bounds["lower"] &
    full_grid$latitude <= area_bounds["upper"]

  area_weights <- ifelse(in_region, full_grid$area_km2_WCGBTS, 0)

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
