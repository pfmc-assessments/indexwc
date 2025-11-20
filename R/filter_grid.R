#' Filter a grid created with [lookup_grid()]
#'
#' @param grid A grid returned from [lookup_grid()].
#' @param boundary_north,boundary_south Real numbers providing the northern and
#'   southern latitudinal boundaries for your desired grid.
#' @return A data frame with the same structure as `grid`.
#' @author Kelli F. Johnson
filter_grid <- function(grid, boundary_north, boundary_south) {
  sub_grid <- grid |>
    dplyr::filter(latitude < boundary_north) |>
    dplyr::filter(latitude >= boundary_south)
}
