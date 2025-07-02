#' Format the strata for a species distribution model
#'
#' @param strata An object returned from [nwfscSurvey::GetStrata.fn()].
#' @param min_depth,max_depth The minimum and maximum depths you want to use
#'   for all strata if you want to override the values in `strata`.
#' @export
#' @examples
#' format_strata()
#' format_strata(nwfscSurvey::GetStrata.fn("sablefish"))
format_strata <- function(strata = nwfscSurvey::GetStrata.fn("coast"),
                          min_depth = 55,
                          max_depth = 1280) {
  strata_temp <- strata |>
    tidyr::separate(name, into = c("dname", "STRATA"), sep = "_") |>
    dplyr::group_by(STRATA) |>
    dplyr::summarize(
      north_border = min(Latitude_dd.2),
      south_border = min(Latitude_dd.1),
      shallow_border = ifelse(is.null(min_depth), min_depth, min(Depth_m.1)),
      deep_border = ifelse(is.null(max_depth), max_depth, max(Depth_m.2))
    )
  # Don't do a summary strata if only one row or coast is present
  if (nrow(strata_temp) == 1 || "coast" %in% dplyr::pull(strata_temp, STRATA)) {
    out <- strata_temp
  } else {
    sumrow <- data.frame(
      STRATA = paste(dplyr::pull(strata_temp, STRATA), collapse = "_"),
      north_border = max(dplyr::pull(strata_temp, north_border)),
      south_border = min(dplyr::pull(strata_temp, south_border)),
      shallow_border = min(dplyr::pull(strata_temp, shallow_border)),
      deep_border = max(dplyr::pull(strata_temp, deep_border))
    )
    out <- rbind(sumrow, strata_temp)
  }
  return(out)
}
