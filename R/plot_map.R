#' Map using [ggplot2::ggplot()]
#'
#'
#' @param data Predictions data frame created by [predict()] and the prediction
#'   grid.
#' @param column The column name to plot within the plotting function (e.g.,
#'   est_non_rf2, omega_s2, epsilon_st2).
#' 
#' @import ggplot2
#'
#' @author Chantel R. Wetzel
#' @export
#'
plot_map <- function(data, column) {

  lon_range <- c(min(data$lon), max(data$lon))
  lat_range <- c(min(data$lat), max(data$lat))

  ggplot2::ggplot(data, aes(lon, lat, fill = {{ column }})) +
    geom_raster() +
    coord_fixed() +
    nwfscSurvey::draw_theme() +
    nwfscSurvey::draw_land() +
    nwfscSurvey::draw_USEEZ(lon_range, lat_range)
}
