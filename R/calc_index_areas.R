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
#' @param data The data used for fitting the model
#' @param fit A fitted sdmTMB model object
#' @param prediction_grid The prediction grid for the survey that sdmTMB will
#'   use to make model predictions to
#' @param dir Directory path where results will be saved. If `NULL`, results
#'   will only be returned (not saved to a file)
#' @param boundaries A character vector specifying which areas to calculate
#'   indices for. These should be names from [boundaries_data]. The default is
#'   `"Coastwide"`, which calculates only the Coastwide index
#' @param cog Logical. If `TRUE`, center of gravity estimates will also be
#'   calculated using [sdmTMB::get_cog()] for each area. Defaults to `FALSE`.
#'   Note that COG results are returned in long format with separate rows for
#'   the X (easting) and Y (northing) coordinate axes.
#' @export
#' @importFrom utils write.csv
#' @author Kelli F. Johnson
#' @seealso
#' * [boundaries_data], a data object
#' * [available_areas()], helper function to list available areas
#' * [sdmTMB::get_index()], used to generate the area-specific indices
#' * [sdmTMB::get_cog()], used to generate center of gravity estimates
#' * [sdmTMB::sdmTMB()], used to create the `fit` object
#' @return
#' A list with the following components:
#' \describe{
#'   \item{`results`}{A named list with one element per area, each containing
#'     the sdmTMB prediction object, the raw `index` data frame, and (if
#'     `cog = TRUE`) the raw `cog` data frame.}
#'   \item{`indices`}{A data frame of biomass index estimates across all areas,
#'     with columns: `area`, `year`, `est`, `lwr`, `upr`, `log_est`, `se`,
#'     `se_natural`, `type`. Saved to `est_by_area.csv` if `dir` is provided.}
#'   \item{`cogs`}{*(Only present if `cog = TRUE`)* A data frame of center of
#'     gravity estimates across all areas, with columns: `area`, `year`, `est`,
#'     `lwr`, `upr`, `se`, `coord`, `type`. Unlike `indices`, COG results are
#'     in long format with one row per year per coordinate axis (`coord = "X"`
#'     for easting, `coord = "Y"` for northing). Saved to `cog_by_area.csv`
#'     if `dir` is provided.}
#'   \item{`plot_indices`}{A ggplot object of the biomass index across all
#'     areas.}
#' }
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
#'
#' # Also calculate center of gravity
#' results <- calc_index_areas(
#'   data, fit, grid,
#'   dir = getwd(), boundaries = c("Coastwide", "CA"),
#'   cog = TRUE
#' )
#' # Full COG table (long format: two rows per year per area, one per coord)
#' results[["cogs"]]
#' # Northing (latitude) only
#' dplyr::filter(results[["cogs"]], coord == "Y")
#' }
#'
calc_index_areas <- function(data,
                             fit,
                             prediction_grid,
                             dir,
                             boundaries = "Coastwide",
                             cog = FALSE) {
  # Make sure all boundaries are character vector
  if (!is.character(boundaries)) {
    cli::cli_abort(c(
      "x" = "boundaries must be a character vector",
      "i" = "Example: {.code boundaries = c('Coastwide', 'CA', 'OR')}"
    ))
  }

  # Check that all requested areas exist in boundaries_data
  if (!all(boundaries %in% names(boundaries_data))) {
    unknown_areas <- setdiff(boundaries, names(boundaries_data))
    available <- names(boundaries_data)
    cli::cli_abort(c(
      "x" = "Unknown areas: {.val {unknown_areas}}",
      "i" = "Available areas: {.val {available}}",
      "i" = "Use {.fn available_areas} to see all options"
    ))
  }

  # Convert character vector to named list (subset boundaries_data)
  boundaries <- boundaries_data[boundaries]

  # Make sure prediction grid depth is < 0
  if (mean(prediction_grid$depth) > 0) {
    cli::cli_abort("The depth of the prediction grid must be negative.")
  }
  if (mean(data$depth) > 0) {
    cli::cli_abort("The depth of the raw / filtered data must be negative.")
  }

  latitudes_of_catches <- data |>
    dplyr::filter(.data$catch_weight > 0) |>
    dplyr::pull(.data$latitude)
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
  prediction_grid <- as.data.frame(prediction_grid)

  # Name the grids from boundaries_fixed so names flow through automatically
  # to results, index_areas, and cog_areas without any after-the-fact fixup
  boundaries_grids <- purrr::map2(
    .x = boundaries_fixed[, "upper"],
    .y = boundaries_fixed[, "lower"],
    .f = filter_grid,
    grid = prediction_grid
  ) |>
    purrr::set_names(rownames(boundaries_fixed))

  # Internal function used in map to make predictions and return index,
  # and optionally center of gravity. Both get_index() and get_cog() reuse
  # the same prediction object so we only pay the prediction cost once.
  predict_and_index <- function(grid, object) {
    prediction <- predict(object, newdata = grid, return_tmb_object = TRUE)
    prediction[["index"]] <- sdmTMB::get_index(
      obj = prediction,
      bias_correct = TRUE,
      area = grid[["area_km2_WCGBTS"]]
    )
    if (cog) {
      prediction[["cog"]] <- sdmTMB::get_cog(
        obj = prediction,
        bias_correct = TRUE,
        area = grid[["area_km2_WCGBTS"]]
      )
    }
    return(prediction)
  }

  # Use fit with each area (i.e., grid between a set of boundaries) and
  # return a data frame with area specifying which area the index is from
  results <- purrr::map(
    .x = boundaries_grids,
    .f = predict_and_index,
    object = fit
  )

  index_areas <- purrr::map(results, "index") |>
    purrr::list_rbind(names_to = "area") |>
    dplyr::mutate(area = forcats::fct_inorder(.data$area))

  out <- list(
    results = results,
    indices = index_areas
  )

  if (cog) {
    cog_areas <- purrr::map(results, "cog") |>
      purrr::list_rbind(names_to = "area") |>
      dplyr::mutate(area = forcats::fct_inorder(.data$area))
    out[["cogs"]] <- cog_areas
  }

  # Make the directory to plot the index in and plot all areas on one figure
  # and then just the coastwide index on another figure by itself.
  if (!is.null(dir)) {
    fs::dir_create(dir, recurse = TRUE)

    write.csv(
      index_areas,
      file = file.path(dir, "est_by_area.csv"),
      row.names = FALSE
    )

    if (cog) {
      write.csv(
        cog_areas,
        file = file.path(dir, "cog_by_area.csv"),
        row.names = FALSE
      )
    }

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
  }

  out$plot_indices <- plot_indices(index_areas, file_name = NULL)
  return(out)
}


#' Get available areas for index calculation
#'
#' A helper function to see what areas are available in
#' [boundaries_data]. These area names can be passed to the `boundaries`
#' argument of [calc_index_areas()].
#'
#' @export
#' @seealso
#' * [calc_index_areas()], calculate indices for specified areas
#' * [boundaries_data], the data object containing area boundaries
#' @return A character vector of available area names.
#'
#' @examples
#' # See all available areas
#' available_areas()
#'
#' # Use in calc_index_areas()
available_areas <- function() {
  names(boundaries_data)
}
