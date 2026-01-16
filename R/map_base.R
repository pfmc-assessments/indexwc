#' Plot a map to use as the base for other maps
#'
#' Plot a map with lines for the United States, including islands.
#'
#' @param x_min,x_max,y_min,y_max Minimum and maximum values for the extent of
#'   the desired map. These need to be in decimal degrees. The default is to
#'   map the California Current from the U.S./Canada border to the
#'   U.S./Mexico border.
#' @return
#' A [ggplot2::ggplot()] object.
#' @author Kelli F. Johnson
#' @examples
#' \dontrun{
#' # Return a blank base map
#' map_base()
#' # TODO: example with map_base second
#' }
map_base <- function(x_min = -155,
                     x_max = -115,
                     y_min = 20,
                     y_max = 48) {
  # map_data <- rnaturalearth::ne_countries(
  #   scale = "medium",
  #   returnclass = "sf",
  #   country = "united states of america"
  # )

  # Crop the polygon for plotting and efficiency:
  # sf::st_bbox(map_data) to find the rough coordinates
  coast <- suppressWarnings(suppressMessages(
    sf::st_crop(
      mapdata,
      c(xmin = x_min, ymin = y_min, xmax = x_max, ymax = y_max)
    )
  ))
  coast_proj <- sf::st_transform(coast, crs = utm_zone_10)
  gg <- ggplot2::ggplot(coast_proj) +
    ggplot2::geom_sf() +
    ggplot2::theme_bw() +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude")
  return(gg)
}
