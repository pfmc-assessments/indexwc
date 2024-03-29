#' Map using [ggplot2::ggplot()]
#'
#'
#' @param data Predictions data frame created by [stats::predict()] and the
#'   prediction grid.
#' @param column The column name to plot within the plotting function (e.g.,
#'   est_non_rf2, omega_s2, epsilon_st2).
#'
#' @import ggplot2
#'
#' @author Chantel R. Wetzel
#' @export
#'
plot_map <- function(data, column) {
  lon_range <- c(min(data$longitude), max(data$longitude))
  lat_range <- c(min(data$latitude), max(data$latitude))

  ggplot2::ggplot(data, ggplot2::aes(longitude, latitude, fill = {{ column }})) +
    ggplot2::geom_tile(width = 0.1, height = 0.1) +
    ggplot2::coord_fixed() +
    nwfscSurvey::draw_theme() +
    nwfscSurvey::draw_land() +
    nwfscSurvey::draw_USEEZ(lon_range, lat_range)
}
