#' Map plotting basic settings applied to all
#' mapping diagnostic figures using ggplot2
#'
#'
#' @param data Predictions data frame created by 
#' the sdmTMB::predict function and the prediction
#' grid.
#' @param column The column name to plot within
#' the plotting function (e.g., est_non_rf2, 
#' omega_s2, epsilon_st2).
#' 
#' @import ggplot2
#'
#' @author Chantel Wetzel
#' @export
#' 
plot_map <- function(data, column) {
	ggplot2::ggplot(data, aes(lon, lat, fill = {{ column }})) +
		geom_raster() +
		coord_fixed() 
}