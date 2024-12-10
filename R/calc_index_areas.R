#' Calculate area-specific indices between each set of boundaries
#'
#' Calculate annual estimates of biomass (mt) for each area specified in
#' `boundaries`. The boundaries can be any configuration as long as all
#' boundaries lie within the overall grid; they can even be overlapping.
#' The more boundaries there are the longer the function will take to run.
#'
#' @details
#' The outer areas will be truncated based on the presence of positive tows
#' such that the most northern and southern extents of adjoining areas will
#' not extrapolate into areas where positive tows are not observed at least
#' once. This is helpful for species that are sparsely sampled because model
#' convergence is poor when there are a lot of zeros.
#'
#' @inheritParams format_data
#' @inheritParams diagnose
#' @param boundaries A named list of northern and southern boundaries for a
#'   set of areas. The list can contain as many areas as you would like but
#'   it must contain at least one area and each area must be a vector of two
#'   real numbers specified in decimal degrees. The order of the areas only
#'   matters if you care what order they are plotted because the names will
#'   be turned into factors. The default value uses a data object called
#'   `boundaries_data`, which is a list of several areas along the U.S. West
#'   Coast, including a coastwide area going from the northern Washington
#'   border to the southern California border.
#' @export
#' @author Kelli F. Johnson
#' @seealso
#' * [boundaries_data], a data object
#' * [sdmTMB::get_index()], used to generate the area-specific indices
#' * [sdmTMB::sdmTMB()], used to create the `fit` object
#' @return
#' A very large list is returned with predictions, model fits, and indices for
#' each area specified in boundaries. Potentially, two figures are also saved
#' to the disk displaying the index by year. The index for each area is also
#' saved to a csv file in `dir` titled `est_by_area.csv`.
#' @examples
#' \dontrun{
#' # Read back in the saved object
#' load("sdmTMB_save.RData")
#' # pick which boundary you want to use, e.g., 4 for California,
#' boundaries_data[4]
#' # or you can make your own named list, e.g.,
#' list("test" = c(39, 38))
#' # run the function
#' index <- calc_index_areas(
#'   data, fit, grid,
#'   dir = getwd(), boundaries = list("test" = c(39, 38))
#' )
#' # look at the index, which will be in "test" because that is what we
#' # named the boundary
#' index[["test"]][["index"]]
#' }
#'
calc_index_areas <- function(data,
                             fit,
                             prediction_grid,
                             dir,
                             boundaries = boundaries_data["Coastwide"]) {
  # There is no way project the index with bias correction in sdmTMB::sdmTMB
  # which is why we have to call [sdmTMB::get_index()] even if predictions are
  # specified in [sdmTMB::sdmTMB()].
  latitudes_of_catches <- data |>
    dplyr::filter(catch_weight > 0) |>
    dplyr::pull(latitude)
  boundaries_fixed <- filter_boundaries(
    y = latitudes_of_catches,
    boundaries = boundaries
  )
  if (NROW(boundaries_fixed) == 0) {
    cli::cli_abort(c(
      "x" = "There are no data in your supplied boundaries.",
      "i" = "We checked for data in {names(boundaries)}.",
      "i" = "Your data ranged from {.val {max(latitudes_of_catches)}}
             to {.val {min(latitudes_of_catches)}}."
    ))
  }
  boundaries_grids <- purrr::map2(
    .x = boundaries_fixed[, "upper"],
    .y = boundaries_fixed[, "lower"],
    .f = filter_grid,
    grid = prediction_grid
  )

  # Internal function used in map to make predictions and return index
  predict_and_index <- function(grid, object) {
    prediction <- predict(object, newdata = grid, return_tmb_object = TRUE)
    index <- sdmTMB::get_index(
      obj = prediction,
      bias_correct = TRUE,
      area = grid[["area_km2"]]
    )
    prediction[["index"]] <- index
    return(prediction)
  }

  # Use fit with each area (i.e., grid between a set of boundaries) and
  # return a data frame with area specifying which area the index is from
  results <- purrr::map(
    .x = boundaries_grids,
    .f = predict_and_index,
    object = fit
  )

  if (is.null(names(results))) {
    names(results) <- names(boundaries)
  }
  index_areas <- purrr::map(
    results,
    "index"
  ) |>
    purrr::list_rbind(names_to = "area") |>
    dplyr::mutate(
      # Ensure the factor is in the same order the values appear in the data
      area = forcats::fct_inorder(area)
    )

  # Make the directory to plot the index in and plot all areas on one figure
  # and then just the coastwide index on another figure by itself.
  fs::dir_create(dir, recurse = TRUE)
  gg_index_areas <- plot_indices(
    data = index_areas,
    save_loc = dir
  )
  write.csv(
    index_areas,
    file = file.path(dir, "est_by_area.csv"),
    row.names = FALSE
  )
  if (any(grepl("wide", index_areas[["area"]], ignore.case = TRUE))) {
    gg_index_coastwide <- plot_indices(
      data = dplyr::filter(
        index_areas,
        grepl("wide", area, ignore.case = TRUE)
      ),
      save_loc = dir,
      file_name = "index_coastwide.png"
    )
  }

  return(results)
}
