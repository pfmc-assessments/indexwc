#' Calculate area-specific indices
#'
#' Calculate annual estimates of indicies for each area specified in
#' `boundaries`.
#'
#' @inheritParams format_data
#' @inheritParams diagnose
#' @param boundaries A list of areas with a northern and southern boundary per
#'   area specified in decimal degrees. The default uses the data object called
#'   `boundaries` that is a list of several areas along and including the
#'   entire U.S. West Coast, where the latter is called `"Coastwide"`.
#' @export
#' @author Kelli F. Johnson
#' @seealso
#' * [boundaries], a data object
#' * [sdmTMB::get_index()], used to generate the area-specific indices
#' * [sdmTMB::sdmTMB()], used to create the `fit` object
#' @return
#' A {ggplot2} object is returned with a figure of the indices. This figure is
#' also saved to the disk. But, the main benefit of returning the figure is
#' that users can change parts of the figure and re-save it if they wish and
#' the calculated indices will be available in `*[["data"]]` where you should
#' replace `*` with your saved object name.
calc_index_areas <- function(data,
                             fit,
                             prediction_grid,
                             dir,
                             boundaries = boundaries) {
  # There is no way project the index with bias correction in sdmTMB::sdmTMB
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
  if ("sablefish" %in% data[["common_name"]]) {
    boundaries <- c(
      boundaries,
      list(
        North = c(49, 36),
        South = c(36, 32)
      )
    )
  }
  if ("shortspine thornyhead" %in% data[["common_name"]]) {
    boundaries <- c(
      boundaries,
      list(
        North = c(49.00, 34.45),
        South = c(34.45, 32.00)
      )
    )
  }

  # Pull out boundaries if they are not present
  if (NROW(dplyr::filter(data, latitude > southern_WA)) == 0) {
    boundaries <- boundaries[-which(names(boundaries) == "WA")]
  }
  if (NROW(dplyr::filter(data, latitude < southern_WA & latitude > southern_OR)) == 0) {
    boundaries <- boundaries[-which(names(boundaries) == "OR")]
  }
  if (NROW(dplyr::filter(data, latitude < southern_OR)) == 0) {
    boundaries <- boundaries[-which(names(boundaries) == "CA")]
  }
  # Shrink state-specific border boundaries if no positive tows
  if_p <- function(x, y, .f) {
    ifelse(
      test = eval(parse(text = paste("x", .f, "y"))),
      yes = y,
      no = x
    )
  }
  index_areas <- purrr::map_dfr(
    # Set up the area-specific prediction_grids as a list of data frames
    .x = purrr::map2(
      .x = if_p(purrr::map(boundaries, 1), boundaries[["coastwide"]][1], ">"),
      .y = if_p(purrr::map(boundaries, 2), boundaries[["coastwide"]][2], "<"),
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

  fs::dir_create(dir, recurse = TRUE)
  index_areas$area <- factor(index_areas$area, labels = names(boundaries))
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
  if (any(grepl("north", index_areas[["area"]]))) {
    gg_index_coastwide <- plot_indices(
      data = dplyr::filter(index_areas, grepl("north|south", ignore.case = TRUE, area)),
      save_loc = dir,
      file_name = "index_north-versus-south.png"
    )
  }
  return(gg_index_areas)
}
