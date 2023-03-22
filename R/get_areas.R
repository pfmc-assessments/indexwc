#' Calculate indices by areas, e.g., Washington, Oregon, and California
#'
#' @template data
#' @inheritParams get_diagnostics
#' @export
#' @author Kelli F. Johnson
#' @return
#' A {ggplot2} object is returned with a figure of the indices. This figure is
#' also saved to the disk. But, the main benefit of returning the figure is
#' that users can change parts of the figure and re-save it if they wish and
#' the calculated indices will be available in `*[["data"]]` where you should
#' replace `*` with your saved object name.
get_index_areas <- function(data, fit, prediction_grid, dir) {
# There is no way to estimate the index with bias correction in sdmTMB::sdmTMB
  # which is why we have to call [sdmTMB::get_index()] even if predictions are
  # specified in [sdmTMB::sdmTMB()].
  boundaries <-  list(
    coastwide = data %>%
      dplyr::filter(catch_weight > 0) %>%
      dplyr::pull(latitude) %>%
      range() %>%
      rev(),
    WA = c(southern_BC, southern_WA),
    OR = c(southern_WA, southern_OR),
    CA = c(southern_OR, southern_CA)
  )
  index_areas <- purrr::map_dfr(
    # Set up the area-specific prediction_grids as a list of data frames
    .x = purrr::map2(
      .x = purrr::map(boundaries, 1),
      .y = purrr::map(boundaries, 2),
      .f = filter_grid,
      grid = prediction_grid
    ),
    # Anonymous function that does both prediction and get_index
    .f = function(grid, object) {
      prediction <- predict(object, newdata = grid, return_tmb_object = TRUE)
      index <- sdmTMB::get_index(
        obj = prediction,
        bias_correct = TRUE,
        area = grid[["area_km2"]]
      )
      return(index)
    },
    object = fit,
    .id = "area"
  )

  gg_index_areas <- plot_indices(
    data = index_areas,
    save_loc = dir
  )
  if (any(grepl("wide", index_areas[["area"]]))) {
    gg_index_coastwide <- plot_indices(
      data = dplyr::filter(index_areas, grepl("wide", area)),
      save_loc = dir,
      file_name = "index_coastwide.png"
    )
  }
  return(gg_index_areas)
}
