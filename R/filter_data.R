filter_data <- function(data, ...) {
  UseMethod("filter_data", data)
}

#' @export
filter_data.default <- function(data, ...) {
  return(data)
}

#' @export
filter_data.data.frame <- function(data, ...) {
  # Nothing is needed for filtering when data has the data.frame
  # class, just move on to the next class present.
  NextMethod()
}

#' @export
filter_data.nwfscSurvey <- function(data,
                                    min_depth,
                                    max_depth,
                                    min_latitude,
                                    max_latitude,
                                    min_year,
                                    max_year,
                                    ...) {
  data %>%
    dplyr::filter(
      Depth_m >= min_depth,
      Depth_m <= max_depth,
      Latitude_dd >= min_latitude,
      Latitude_dd <= max_latitude,
      Year >= min_year,
      Year <= max_year
    )
}
